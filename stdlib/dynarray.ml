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
     indices from [0] to [length - 1] included.
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
   implement [append]. We believe that optimizing [get] and [set] is
   more important for dynamic arrays.

   {2 Invariants and valid states}

   We enforce the invariant that [length >= 0] at all times.
   we rely on this invariant for optimization.

   The following conditions define what we call a "valid" dynarray:
   - valid length: [length <= Array.length arr]
   - no missing element in the live space:
     forall i, [0 <= i < length] implies [arr.(i) <> Empty]
   - no element in the empty space:
     forall i, [length <= i < Array.length arr] implies [arr.(i) = Empty]

   Unfortunately, we cannot easily enforce validity as an invariant in
   presence of concurrent updates. We can thus observe dynarrays in
   "invalid states". Our implementation may raise exceptions or return
   incorrect results on observing invalid states, but of course it
   must preserve memory safety.
*)

module Error = struct
  let[@inline never] index_out_of_bounds f ~i ~length =
    if length = 0 then
      Printf.ksprintf invalid_arg
        "Dynarray.%s: index %d out of bounds (empty dynarray)"
        f i
    else
      Printf.ksprintf invalid_arg
        "Dynarray.%s: index %d out of bounds (0..%d)"
        f i (length - 1)

  let[@inline never] negative_length_requested f n =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: negative length %d requested"
      f n

  let[@inline never] negative_capacity_requested f n =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: negative capacity %d requested"
      f n

  let[@inline never] requested_length_out_of_bounds f requested_length =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: cannot grow to requested length %d (max_array_length is %d)"
      f requested_length Sys.max_array_length

  (* When observing an invalid state ([missing_element],
     [invalid_length]), we do not give the name of the calling function
     in the error message, as the error is related to invalid operations
     performed earlier, and not to the callsite of the function
     itself. *)

  let invalid_state_description =
    "Invalid dynarray (unsynchronized concurrent length change)"

  let[@inline never] missing_element ~i ~length =
    Printf.ksprintf invalid_arg
      "%s: missing element at position %d < length %d"
      invalid_state_description
      i length

  let[@inline never] invalid_length ~length ~capacity =
    Printf.ksprintf invalid_arg
      "%s: length %d > capacity %d"
      invalid_state_description
      length capacity

  let[@inline never] length_change_during_iteration f ~expected ~observed =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: a length change from %d to %d occurred during iteration"
      f expected observed

  (* When an [Empty] element is observed unexpectedly at index [i],
     it may be either an out-of-bounds access or an invalid-state situation
     depending on whether [i <= length]. *)
  let[@inline never] unexpected_empty_element f ~i ~length =
    if i < length then
      missing_element ~i ~length
    else
      index_out_of_bounds f ~i ~length

  let[@inline never] empty_dynarray f =
    Printf.ksprintf invalid_arg
      "Dynarray.%s: empty array" f
end

(* Detecting iterator invalidation.

   See {!iter} below for a detailed usage example.
*)
let check_same_length f a ~length =
  let length_a = a.length in
  if length <> length_a then
    Error.length_change_during_iteration f
      ~expected:length ~observed:length_a

(** Careful unsafe access. *)

(* Postcondition on non-exceptional return:
   [length <= Array.length arr] *)
let[@inline always] check_valid_length length arr =
  let capacity = Array.length arr in
  if length > capacity then
    Error.invalid_length ~length ~capacity

(* Precondition: [0 <= i < length <= Array.length arr]

   This precondition is typically guaranteed by knowing
   [0 <= i < length] and calling [check_valid_length length arr].*)
let[@inline always] unsafe_get arr ~i ~length =
  match Array.unsafe_get arr i with
  | Empty -> Error.missing_element ~i ~length
  | Elem {v} -> v


(** {1:dynarrays Dynamic arrays} *)

let create () = {
  length = 0;
  arr = [| |];
}

let make n x =
  if n < 0 then Error.negative_length_requested "make" n;
  {
    length = n;
    arr = Array.init n (fun _ -> Elem {v = x});
  }

let init n f =
  if n < 0 then Error.negative_length_requested "init" n;
  {
    length = n;
    arr = Array.init n (fun i -> Elem {v = f i});
  }

let get a i =
  (* This implementation will propagate an [Invalid_argument] exception
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

let copy {length; arr} =
  check_valid_length length arr;
  (* use [length] as the new capacity to make
     this an O(length) operation. *)
  {
    length;
    arr = Array.init length (fun i ->
      let v = unsafe_get arr ~i ~length in
      Elem {v}
    );
  }

let get_last a =
  let {arr; length} = a in
  check_valid_length length arr;
  (* We know [length <= capacity a]. *)
  if length = 0 then Error.empty_dynarray "get_last";
  (* We know [length > 0]. *)
  unsafe_get arr ~i:(length - 1) ~length

let find_last a =
  let {arr; length} = a in
  check_valid_length length arr;
  (* We know [length <= capacity a]. *)
  if length = 0 then None
  else
    (* We know [length > 0]. *)
    Some (unsafe_get arr ~i:(length - 1) ~length)

(** {1:removing Removing elements} *)

let pop_last a =
  let {arr; length} = a in
  check_valid_length length arr;
  (* We know [length <= capacity a]. *)
  if length = 0 then raise Not_found;
  let last = length - 1 in
  (* We know [length > 0] so [last >= 0]. *)
  match Array.unsafe_get arr last with
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
  if n < 0 then Error.negative_length_requested "truncate" n;
  let {arr; length} = a in
  if length <= n then ()
  else begin
    a.length <- n;
    Array.fill arr n (length - n) Empty;
  end

let clear a = truncate a 0


(** {1:capacity Backing array and capacity} *)

let capacity a = Array.length a.arr

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

let ensure_capacity a capacity_request =
  let arr = a.arr in
  let cur_capacity = Array.length arr in
  if capacity_request < 0 then
    Error.negative_capacity_requested "ensure_capacity" capacity_request
  else if cur_capacity >= capacity_request then
    (* This is the fast path, the code up to here must do as little as
       possible. (This is why we don't use [let {arr; length} = a] as
       usual, the length is not needed in the fast path.)*)
    ()
  else begin
    if capacity_request > Sys.max_array_length then
      Error.requested_length_out_of_bounds "ensure_capacity" capacity_request;
    let new_capacity =
      (* We use either the next exponential-growth strategy,
         or the requested strategy, whichever is bigger.

         Compared to only using the exponential-growth strategy, this
         lets us use less memory by avoiding any overshoot whenever
         the capacity request is noticeably larger than the current
         capacity.

         Compared to only using the requested capacity, this avoids
         losing the amortized guarantee: we allocated "exponentially
         or more", so the amortization holds. In particular, notice
         that repeated calls to [ensure_capacity a (length a + 1)]
         will have amortized-linear rather than quadratic complexity.
      *)
      max (next_capacity cur_capacity) capacity_request in
    let new_arr = Array.make new_capacity Empty in
    Array.blit arr 0 new_arr 0 a.length;
    a.arr <- new_arr;
    (* postcondition: *)
    assert (0 <= capacity_request);
    assert (capacity_request <= Array.length new_arr);
  end

let ensure_extra_capacity a extra_capacity_request =
  ensure_capacity a (length a + extra_capacity_request)

let fit_capacity a =
  if capacity a = a.length
  then ()
  else a.arr <- Array.sub a.arr 0 a.length

let set_capacity a n =
  if n < 0 then
    Error.negative_capacity_requested "set_capacity" n;
  let arr = a.arr in
  let cur_capacity = Array.length arr in
  if n < cur_capacity then begin
    a.length <- min a.length n;
    a.arr <- Array.sub arr 0 n;
  end
  else if n > cur_capacity then begin
    let new_arr = Array.make n Empty in
    Array.blit arr 0 new_arr 0 a.length;
    a.arr <- new_arr;
  end

let reset a =
  a.length <- 0;
  a.arr <- [||]

(** {1:adding Adding elements} *)

(* We chose an implementation of [add_last a x] that behaves correctly
   in presence of asynchronous / re-entrant code execution around
   allocations and poll points: if another thread or a callback gets
   executed on allocation, we add the element at the new end of the
   dynamic array.

   (We do not give the same guarantees in presence of concurrent
   parallel updates, which are much more expensive to protect
   against.)
*)

(* [add_last_if_room a elem] only writes the slot if there is room, and
   returns [false] otherwise. *)
let[@inline] add_last_if_room a elem =
  let {arr; length} = a in
  (* we know [0 <= length] *)
  if length >= Array.length arr then false
  else begin
    (* we know [0 <= length < Array.length arr] *)
    a.length <- length + 1;
    Array.unsafe_set arr length elem;
    true
  end

let add_last a x =
  let elem = Elem {v = x} in
  if add_last_if_room a elem then ()
  else begin
    (* slow path *)
    let rec grow_and_add a elem =
      ensure_extra_capacity a 1;
      if not (add_last_if_room a elem)
      then grow_and_add a elem
    in grow_and_add a elem
  end

let rec append_list a li =
  match li with
  | [] -> ()
  | x :: xs -> add_last a x; append_list a xs

let append_iter a iter b =
  iter (fun x -> add_last a x) b

let append_seq a seq =
  Seq.iter (fun x -> add_last a x) seq

(* append_array: same [..._if_room] and loop logic as [add_last]. *)

let append_array_if_room a b =
  let {arr; length = length_a} = a in
  let length_b = Array.length b in
  if length_a + length_b > Array.length arr then false
  else begin
    a.length <- length_a + length_b;
    (* Note: we intentionally update the length *before* filling the
       elements. This "reserve before fill" approach provides better
       behavior than "fill then notify" in presence of reentrant
       modifications (which may occur below, on a poll point in the loop or
       the [Elem] allocation):

       - If some code asynchronously adds new elements after this
         length update, they will go after the space we just reserved,
         and in particular no addition will be lost. If instead we
         updated the length after the loop, any asynchronous addition
         during the loop could be erased or erase one of our additions,
         silently, without warning the user.

       - If some code asynchronously iterates on the dynarray, or
         removes elements, or otherwise tries to access the
         reserved-but-not-yet-filled space, it will get a clean "missing
         element" error. This is worse than with the fill-then-notify
         approach where the new elements would only become visible
         (to iterators, for removal, etc.) alltogether at the end of
         loop.

       To summarise, "reserve before fill" is better on add-add races,
       and "fill then notify" is better on add-remove or add-iterate
       races. But the key difference is the failure mode:
       reserve-before fails on add-remove or add-iterate races with
       a clean error, while notify-after fails on add-add races with
       silently disappearing data. *)
    for i = 0 to length_b - 1 do
      let x = Array.unsafe_get b i in
      Array.unsafe_set arr (length_a + i) (Elem {v = x})
    done;
    true
  end

let append_array a b =
  if append_array_if_room a b then ()
  else begin
    (* slow path *)
    let rec grow_and_append a b =
      ensure_extra_capacity a (Array.length b);
      if not (append_array_if_room a b)
      then grow_and_append a b
    in grow_and_append a b  end

(* append: same [..._if_room] and loop logic as [add_last],
   same reserve-before-fill logic as [append_array]. *)

(* It is a programming error to mutate the length of [b] during a call
   to [append a b]. To detect this mistake we keep track of the length
   of [b] throughout the computation and check it that does not
   change.
*)
let append_if_room a b ~length_b =
  let {arr = arr_a; length = length_a} = a in
  if length_a + length_b > Array.length arr_a then false
  else begin
    a.length <- length_a + length_b;
    let arr_b = b.arr in
    check_valid_length length_b arr_b;
    for i = 0 to length_b - 1 do
      let x = unsafe_get arr_b ~i ~length:length_b in
      Array.unsafe_set arr_a (length_a + i) (Elem {v = x})
    done;
    check_same_length "append" b ~length:length_b;
    true
  end

let append a b =
  let length_b = length b in
  if append_if_room a b ~length_b then ()
  else begin
    (* slow path *)
    let rec grow_and_append a b ~length_b =
      ensure_extra_capacity a length_b;
      (* Eliding the [check_same_length] call below would be wrong in
         the case where [a] and [b] are aliases of each other, we
         would get into an infinite loop instead of failing.

         We could push the call to [append_if_room] itself, but we
         prefer to keep it in the slow path.  *)
      check_same_length "append" b ~length:length_b;
      if not (append_if_room a b ~length_b)
      then grow_and_append a b ~length_b
    in grow_and_append a b ~length_b
  end



(** {1:iteration Iteration} *)

(* The implementation choice that we made for iterators is the one
   that maximizes efficiency by avoiding repeated bound checking: we
   check the length of the dynamic array once at the beginning, and
   then only operate on that portion of the dynarray, ignoring
   elements added in the meantime.

   The specification states that it is a programming error to mutate
   the length of the array during iteration. We check for this and
   raise an error on size change.
   Note that we may still miss some transient state changes that cancel
   each other and leave the length unchanged at the next check.
*)

let iter_ f k a =
  let {arr; length} = a in
  (* [check_valid_length length arr] is used for memory safety, it
     guarantees that the backing array has capacity at least [length],
     allowing unsafe array access.

     [check_same_length] is used for correctness, it lets the function
     fail more often if we discover the programming error of mutating
     the length during iteration.

     We could, naively, call [check_same_length] at each iteration of
     the loop (before or after, or both). However, notice that this is
     not necessary to detect the removal of elements from [a]: if
     elements have been removed by the time the [for] loop reaches
     them, then [unsafe_get] will itself fail with an [Invalid_argument]
     exception. We only need to detect the addition of new elements to
     [a] during iteration, and for this it is enough to call
     [check_same_length] once at the end.

     Calling [check_same_length] more often could catch more
     programming errors, but the only errors that we miss with this
     optimization are those that keep the array size constant --
     additions and deletions that cancel each other. We consider this
     an acceptable tradeoff.
  *)
  check_valid_length length arr;
  for i = 0 to length - 1 do
    k (unsafe_get arr ~i ~length);
  done;
  check_same_length f a ~length

let iter k a =
  iter_ "iter" k a

let iteri k a =
  let {arr; length} = a in
  check_valid_length length arr;
  for i = 0 to length - 1 do
    k i (unsafe_get arr ~i ~length);
  done;
  check_same_length "iteri" a ~length

let map f a =
  let {arr; length} = a in
  check_valid_length length arr;
  let res = {
    length;
    arr = Array.init length (fun i ->
      Elem {v = f (unsafe_get arr ~i ~length)});
  } in
  check_same_length "map" a ~length;
  res


let mapi f a =
  let {arr; length} = a in
  check_valid_length length arr;
  let res = {
    length;
    arr = Array.init length (fun i ->
      Elem {v = f i (unsafe_get arr ~i ~length)});
  } in
  check_same_length "mapi" a ~length;
  res

let fold_left f acc a =
  let {arr; length} = a in
  check_valid_length length arr;
  let r = ref acc in
  for i = 0 to length - 1 do
    let v = unsafe_get arr ~i ~length in
    r := f !r v;
  done;
  check_same_length "fold_left" a ~length;
  !r

let fold_right f a acc =
  let {arr; length} = a in
  check_valid_length length arr;
  let r = ref acc in
  for i = length - 1 downto 0 do
    let v = unsafe_get arr ~i ~length in
    r := f v !r;
  done;
  check_same_length "fold_right" a ~length;
  !r

let exists p a =
  let {arr; length} = a in
  check_valid_length length arr;
  let rec loop p arr i length =
    if i = length then false
    else
      p (unsafe_get arr ~i ~length)
      || loop p arr (i + 1) length
  in
  let res = loop p arr 0 length in
  check_same_length "exists" a ~length;
  res

let for_all p a =
  let {arr; length} = a in
  check_valid_length length arr;
  let rec loop p arr i length =
    if i = length then true
    else
      p (unsafe_get arr ~i ~length)
      && loop p arr (i + 1) length
  in
  let res = loop p arr 0 length in
  check_same_length "for_all" a ~length;
  res

let filter f a =
  let b = create () in
  iter_ "filter" (fun x -> if f x then add_last b x) a;
  b

let filter_map f a =
  let b = create () in
  iter_ "filter_map" (fun x ->
    match f x with
    | None -> ()
    | Some y -> add_last b y
  ) a;
  b


(** {1:conversions Conversions to other data structures} *)

(* The eager [to_*] conversion functions behave similarly to iterators
   in presence of updates during computation. The [*_reentrant]
   functions obey their more permissive specification, which tolerates
   any concurrent update. *)

let of_array a =
  let length = Array.length a in
  {
    length;
    arr = Array.init length (fun i -> Elem {v = Array.unsafe_get a i});
  }

let to_array a =
  let {arr; length} = a in
  check_valid_length length arr;
  let res = Array.init length (fun i ->
    unsafe_get arr ~i ~length)
  in
  check_same_length "to_array" a ~length;
  res

let of_list li =
  let a = create () in
  List.iter (fun x -> add_last a x) li;
  a

let to_list a =
  let {arr; length} = a in
  check_valid_length length arr;
  let l = ref [] in
  for i = length - 1 downto 0 do
    l := unsafe_get arr ~i ~length :: !l
  done;
  check_same_length "to_list" a ~length;
  !l

let of_seq seq =
  let init = create() in
  append_seq init seq;
  init

let to_seq a =
  let {arr; length} = a in
  check_valid_length length arr;
  let rec aux i = fun () ->
    check_same_length "to_seq" a ~length;
    if i >= length then Seq.Nil
    else begin
      let v = unsafe_get arr ~i ~length in
      Seq.Cons (v, aux (i + 1))
    end
  in
  aux 0

let to_seq_reentrant a =
  let rec aux i = fun () ->
    if i >= length a then Seq.Nil
    else begin
      let v = get a i in
      Seq.Cons (v, aux (i + 1))
    end
  in
  aux 0

let to_seq_rev a =
  let {arr; length} = a in
  check_valid_length length arr;
  let rec aux i = fun () ->
    check_same_length "to_seq_rev" a ~length;
    if i < 0 then Seq.Nil
    else begin
      let v = unsafe_get arr ~i ~length in
      Seq.Cons (v, aux (i - 1))
    end
  in
  aux (length - 1)

let to_seq_rev_reentrant a =
  let rec aux i = fun () ->
    if i < 0 then Seq.Nil
    else if i >= length a then
      (* If some elements have been removed in the meantime, we skip
         those elements and continue with the new end of the array. *)
      aux (length a - 1) ()
    else begin
      let v = get a i in
      Seq.Cons (v, aux (i - 1))
    end
  in
  aux (length a - 1)
