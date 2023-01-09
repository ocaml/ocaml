(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Gabriel Scherer, projet Partout, INRIA Paris-Saclay          *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = {
  mutable length : int;
  mutable arr : 'a slot array;
}
(* {2 The type ['a t]}

   A dynamic array is represented using a backing array [arr] and
   a [length]. It behaves as an array of size [length] -- the indices
   from [0] to [length - 1] included contain user-provided values and
   can be [get] and [set] -- but the length may also change in the
   future by adding or removing elements at the end.

   We use the following concepts;
   - capacity: the length of the backing array:
     [Array.length  arr]
   - live space: the portion of the backing array with
     indices from [0] to [length] excluded.
   - empty space: the portion of the backing array
     from [length] to the end of the backing array.

   {2 The type ['a slot]}

   We should not keep a user-provided value in the empty space, as
   this could extend its lifetime and may result in memory leaks of
   arbitrary size. Functions that remove elements from the dynamic
   array, such as [pop_last] or [truncate], must really erase the
   element from the backing array.

   This constraint makes it difficult to represent an dynamic array of
   elements of type ['a] with a backing array of type ['a array]: what
   valid value of type ['a] would we use in the empty space? Typical
   choices include:
   - accepting scenarios where we actually leak user-provided values
     (but this can blowup memory usage in some cases, and is hard to debug)
   - requiring a "dummy" value at creation of the dynamic array
     or in the parts of the API that grow the empty space
     (but users find this very inconvenient)
   - using arcane Obj.magic tricks
     (but experts don't agree on which tricks are safe to use and/or
      should be used here)
   - using a backing array of ['a option] values, using [None]
     in the empty space
     (but this gives a noticeably less efficient memory representation)

   In the present implementation, we use the ['a option] approach,
   with a twist. With ['a option], calling [set a i x] must reallocate
   a new [Some x] block:
{[
   let set a i x =
     if i < 0 || i >= a.length then error "out of bounds";
     a.arr.(i) <- Some x
]}
   Instead we use the type ['a slot] below,
   which behaves as an option whose [Some] constructor
   (called [Elem] here) has a _mutable_ argument.
*)
and 'a slot =
| Empty
| Elem of { mutable v: 'a }
(*
   This gives an allocation-free implementation of [set] that calls
   [Array.get] (instead of [Array.set]) on the backing array and then
   mutates the [v] parameter. In pseudo-code:
{[
   let set a i x =
     if i < 0 || i >= a.length then error "out of bounds";
     match a.arr.(i) with
     | Empty -> error "invalid state: missing element"
     | Elem s -> s.v <- x
]}
   With this approach, accessing an element still pays the cost of an
   extra indirection (compared to approaches that do not box elements
   in the backing array), but only operations that add new elements at
   the end of the array pay extra allocations.

   There are some situations where ['a option] is better: it makes
   [pop_last_opt] more efficient as the underlying option can be
   returned directly, and it also lets us use [Array.blit] to
   implement [append]. We believe that optimzing [get] and [set] is
   more important for dynamic arrays.

   {2 Invariants and valid states}

   We enforce the invariant that [length >= 0] at all times.
   we rely on this invariant for optimization.

   The following conditions define what we call a "valid" dynarray:
   - valid length: [length <= Array.length arr]
   - no missing element in the live space:
     forall i, [0 <= i <=length] implies [arr.(i) <> Empty]
   - no element in the empty space:
     forall i, [0 <= i < length] implies [arr.(i) = Empty]

   Unfortunately, we cannot easily enforce validity as an invariant in
   presence of concurrent udpates. We can thus observe dynarrays in
   "invalid states". Our implementation may raise exceptions or return
   incorrect results on observing invalid states, but of course it
   must preserve memory safety.
*)

module Error = struct
  let index_out_of_bounds f ~i ~length =
    if length = 0 then
      Printf.ksprintf invalid_arg
        "Dynarray.%s: empty dynarray"
        f
    else
      Printf.ksprintf invalid_arg
        "Dynarray.%s: index %d out of bounds (0..%d)"
        f i (length - 1)

  let negative_length f n =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: negative length %d"
      f n

  let negative_capacity f n =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: negative capacity %d"
      f n

  let requested_length_out_of_bounds f requested_length =
    (* We do not consider this error as a programming error,
       so we raise [Failure] instead of [Invalid_argument]. *)
    Printf.ksprintf failwith
      "Dynarray.%s: cannot grow to requested length %d (max_array_length is %d)"
      f requested_length Sys.max_array_length

  (* When observing an invalid state ([missing_element],
     [invalid_length]), we do not give the name of the calling function
     in the error message, as the error is related to invalid operations
     performed earlier, and not to the callsite of the function
     itself. *)

  let missing_element ~i ~length =
    Printf.ksprintf invalid_arg
      "Dynarray: invalid array (missing element at position %d < length %d)"
      i length

  let invalid_length ~length ~capacity =
    Printf.ksprintf invalid_arg
      "Dynarray: invalid array (length %d > capacity %d)"
      length capacity

  (* When an [Empty] element is observed unexpectedly at index [i],
     it may be either an out-of-bounds access or an invalid-state situation
     depending on whether [i <= length]. *)
  let unexpected_empty_element f ~i ~length =
    if i < length then
      missing_element ~i ~length
    else
      index_out_of_bounds f ~i ~length
end

(** Careful unsafe access. *)

(* Postcondition on non-exceptional return:
   [length <= Array.length arr] *)
let check_valid_length length arr =
  let capacity = Array.length arr in
  if length > capacity then
    Error.invalid_length ~length ~capacity

(* Precondition: [0 <= i < length <= Array.length arr]

   This precondition is typically guaranteed by knowing
   [0 <= i < length] and calling [check_valid_length length arr].*)
let unsafe_get arr ~i ~length =
  match Array.unsafe_get arr i with
  | Empty -> Error.missing_element ~i ~length
  | Elem {v} -> v


(** {1:dynarrays Dynamic arrays} *)

let create () = {
  length = 0;
  arr = [| |];
}

let make n x =
  if n < 0 then Error.negative_length "make" n;
  {
    length = n;
    arr = Array.init n (fun _ -> Elem {v = x});
  }

let init n f =
  if n < 0 then Error.negative_length "init" n;
  {
    length = n;
    arr = Array.init n (fun i -> Elem {v = f i});
  }

let get a i =
  (* This implementation will propagate an [Invalid_arg] exception
     from array lookup if the index is out of the backing array,
     instead of using our own [Error.index_out_of_bounds]. This is
     allowed by our specification, and more efficient -- no need to
     check that [length a <= capacity a] in the fast path. *)
  match a.arr.(i) with
  | Elem s -> s.v
  | Empty ->
      Error.unexpected_empty_element "get" ~i ~length:a.length

let set a i x =
  (* See {!get} comment on the use of checked array
     access without our own bound checking. *)
  match a.arr.(i) with
  | Elem s -> s.v <- x
  | Empty ->
      Error.unexpected_empty_element "set" ~i ~length:a.length

let length a = a.length

let is_empty a = (a.length = 0)

let copy {length; arr} = {
  length;
  arr =
    Array.map (function
      | Empty -> Empty
      | Elem {v} -> Elem {v}
    ) arr;
}

(** {1:removing Removing elements} *)

let pop_last a =
  let {arr; length} = a in
  if length = 0 then raise Not_found;
  let last = length - 1 in
  (* We know [length > 0] so [last >= 0].
     See {!get} comment on the use of checked array
     access without our own bound checking.
  *)
  match arr.(last) with
  (* At this point we know that [last] is a valid index in [arr]. *)
  | Empty ->
      Error.missing_element ~i:last ~length
  | Elem s ->
      Array.unsafe_set arr last Empty;
      a.length <- last;
      s.v

let pop_last_opt a =
  match pop_last a with
  | exception Not_found -> None
  | x -> Some x

let remove_last a =
  let last = length a - 1 in
  if last >= 0 then begin
    a.length <- last;
    a.arr.(last) <- Empty;
  end

let truncate a n =
  if n < 0 then Error.negative_length "truncate" n;
  let {arr; length} = a in
  if length <= n then ()
  else begin
    a.length <- n;
    Array.fill arr n (length - n) Empty;
  end

let clear a = truncate a 0


(** {1:capacity Backing array and capacity} *)

let next_capacity n =
  let n' =
    (* For large values of n, we use 1.5 as our growth factor.

       For smaller values of n, we grow more aggressively to avoid
       reallocating too much when accumulating elements into an empty
       array.

       The constants "512 words" and "8 words" below are taken from
         https://github.com/facebook/folly/blob/
           c06c0f41d91daf1a6a5f3fc1cd465302ac260459/folly/FBVector.h#L1128-L1157
    *)
    if n <= 512 then n * 2
    else n + n / 2
  in
  (* jump directly from 0 to 8 *)
  min (max 8 n') Sys.max_array_length

let ensure_capacity a requested_length =
  let arr = a.arr in
  let cur_capacity = Array.length arr in
  if cur_capacity >= requested_length then
    (* This is the fast path, the code up to here must do as little as
       possible. (This is why we don't use [let {arr; length} = a] as
       usual, the length is not needed in the fast path.)*)
    ()
  else begin
    if requested_length < 0 then
      Error.negative_capacity "ensure_capacity" requested_length;
    if requested_length > Sys.max_array_length then
      Error.requested_length_out_of_bounds "ensure_capacity" requested_length;
    let new_capacity = ref cur_capacity in
    while !new_capacity < requested_length do
      new_capacity := next_capacity !new_capacity
    done;
    let new_capacity = !new_capacity in
    assert (new_capacity >= requested_length);
    let new_arr = Array.make new_capacity Empty in
    Array.blit arr 0 new_arr 0 a.length;
    a.arr <- new_arr;
    assert (0 <= requested_length);
    assert (requested_length <= Array.length new_arr);
  end

let fit_capacity a =
  if Array.length a.arr = a.length
  then ()
  else a.arr <- Array.sub a.arr 0 a.length

let reset a =
  clear a;
  fit_capacity a


(** {1:adding Adding elements} *)

(* We want an implementation of [add_last a x] that behaves correctly
   in presence of aynchronous code execution around allocations and
   poll points: if another thread or a callback gets executed on
   allocation, we add the element at the new end of the dynamic array.

   (We do not give the same guarantees in presence of concurrent
   updates, which are much more expansive to protect against.)
*)

(* [add_last_if_room a elem] only writes the slot if there is room, and
   returns [false] otherwise.

   It is sequentially atomic -- in absence of unsychronized concurrent
   uses, the fields of [a.arr] and [a.length] will not be mutated
   by any other code during execution of this function.
*)
let[@inline] add_last_if_room a elem =
  (* BEGIN ATOMIC *)
  let {arr; length} = a in
  (* we know [0 <= length] *)
  if length >= Array.length arr then false
  else begin
    (* we know [0 <= length < Array.length arr] *)
    Array.unsafe_set arr length elem;
    a.length <- length + 1;
    true
  end
  (* END ATOMIC *)

let add_last a x =
  let elem = Elem {v = x} in
  if add_last_if_room a elem then ()
  else begin
    (* slow path *)
    let rec grow_and_add a elem =
      ensure_capacity a (length a + 1);
      if not (add_last_if_room a elem)
      then grow_and_add a elem
    in grow_and_add a elem
  end

let rec append_list a li =
  match li with
  | [] -> ()
  | x :: xs -> add_last a x; append_list a xs

let append_array a b =
  let len_b = Array.length b in
  ensure_capacity a (length a + len_b);
  for i = 0 to len_b - 1 do
    add_last a (Array.unsafe_get b i)
  done

let append_iter a iter b =
  iter (fun x -> add_last a x) b

let append_seq a seq =
  Seq.iter (add_last a) seq

(* [append] is below, after [iter] *)

(** {1:iteration Iteration} *)

(* The specification says that it is unspecified which updates to the
   dynarray happening during iteration will be observed by the
   iterator. Our implmentation is in fact designed to give the best
   possible guarantees: we observe all updates (insertion, removal,
   modification) to parts of the array that we have not traversed yet.
*)

let iter f a =
  let i = ref 0 in
  while !i < length a do
    f (get a !i);
    incr i
  done

let append a b =
  ensure_capacity a (length a + length b);
  append_iter a iter b

let iteri f a =
  let i = ref 0 in
  while !i < length a do
    f !i (get a !i);
    incr i
  done

let map f a =
  let i = ref 0 in
  let b = create () in
  ensure_capacity b (length a);
  (* Calls to [f] may add further elements to the array [a]; those
     will get added in the final result as well. This means that the
     capacity hint above is sometimes not sufficient to guarantee the
     absence of further reallocations, but this is innocuous. *)
  while !i < length a do
    add_last b (f (get a !i));
    incr i
  done;
  b

let mapi f a =
  let i = ref 0 in
  let b = create () in
  ensure_capacity b (length a);
  while !i < length a do
    add_last b (f !i (get a !i));
    incr i
  done;
  b

let fold_left f acc a =
  let i = ref 0 in
  let acc = ref acc in
  while !i < length a do
    acc := f !acc (get a !i);
    incr i
  done;
  !acc

let exists p a =
  let i = ref 0 in
  let stop = ref false in
  while not !stop && !i < length a do
    if p (get a !i) then stop := true;
    incr i;
  done;
  !stop

let for_all p a =
  let i = ref 0 in
  let continue = ref true in
  while !continue && !i < length a do
    if not (p (get a !i)) then continue := false;
    incr i;
  done;
  !continue

let filter f a =
  let b = create () in
  iter (fun x -> if f x then add_last b x) a;
  b

let filter_map f a =
  let b = create() in
  iter (fun x ->
    match f x with
    | None -> ()
    | Some y -> add_last b y
  ) a;
  b


(** {1:conversions Conversions to other data structures} *)

(* The eager [to_*] conversion functions behave similarly to iterators
   in presence of updates during computation. The [to_seq*] functions
   obey their more permissive specification, which tolerates any
   concurrent update. *)

let of_list li =
  let a = create () in
  List.iter (fun x -> add_last a x) li;
  a

let[@tail_mod_cons] rec to_list_from a i =
  if i >= length a then []
  else
    let x = get a i in
    x :: to_list_from a (i + 1)

let to_list a = to_list_from a 0

let of_array a =
  let length = Array.length a in
  {
    length;
    arr = Array.init length (fun i -> Elem {v = Array.unsafe_get a i});
  }

let to_array a =
  let initial_length = length a in
  if initial_length = 0 then [| |]
  else begin
    let x0 = get a 0 in
    let dst = Array.make initial_length x0 in
    let i = ref 1 in
    let arr = a.arr in
    check_valid_length initial_length arr;
    while !i < initial_length && !i < length a do
      Array.unsafe_set dst !i (unsafe_get arr ~i:!i ~length:initial_length);
      incr i;
    done;
    (* At this point we know that either [!i = initial_length]
       or we have observed [!i >= length a]. *)
    if !i < initial_length then begin
      (* In this case we must have observed [!i >= length a]:
         we reached the end of [a]. *)
      Array.sub dst 0 !i
   end else if !i < length a then begin
      (* In this case we know [!i = initial_length < length a]:
         the array has grown during our iteration.
         Aim for simplicity rather than efficiency in this weird corner case. *)
      Array.append dst (Array.of_list (to_list_from a !i))
    end else begin
      (* we know [!i = initial_length] and have observed [!i >= length a] *)
      dst
    end
  end

let of_seq seq =
  let init = create() in
  append_seq init seq;
  init

let to_seq a =
  let rec aux i () =
    if i >= length a then Seq.Nil
    else begin
      Seq.Cons (get a i, aux (i + 1))
    end
  in
  aux 0

let to_seq_rev a =
  let rec aux i () =
    if i < 0 then Seq.Nil
    else if i >= length a then
      (* If some elements have been removed in the meantime, we skip
         those elements and continue with the new end of the array. *)
      aux (length a - 1) ()
    else Seq.Cons (get a i, aux (i - 1))
  in
  aux (length a - 1)
