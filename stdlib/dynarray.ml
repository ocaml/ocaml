(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Simon Cruanes                               *)
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
  mutable size : int;
  mutable arr : 'a array;
}

(* TODO: move to runtime? bypass write barrier *)
let[@inline] fill_ (a:_ array) i ~filler : unit =
  Array.set a i filler

(* TODO: move to runtime? bypass write barrier *)
let[@inline] fill_with_junk_ (a:_ array) i len ~filler : unit =
  Array.fill a i len filler

let create () = {
  size = 0;
  arr = [| |];
}

let make n x = {
  size=n;
  arr=Array.make n x;
}

let init n f = {
  size=n;
  arr=Array.init n f;
}

let blit v1 i1 v2 i2 len =
  if i1<0 || i2<0 || i1+len >= v1.size || i2 + len >= v2.size then
    invalid_arg "Dynarray.blit";
  Array.blit v1.arr i1 v2.arr i2 len

(* is the underlying array empty? *)
let[@inline] array_is_empty_ v =
  Array.length v.arr = 0

(* next capacity, if current one is [n]. Roughly use [n * 1.5], because it
   provides the good behavior of amortized O(1) number of allocations
   without wasting too much memory in the worst case. *)
let[@inline] next_grow_ n =
  min Sys.max_array_length (1 + n + n lsr 1)

(* resize the underlying array using x to temporarily fill the array *)
let actually_resize_array_ a newcapacity ~filler : unit =
  assert (newcapacity >= a.size);
  assert (not (array_is_empty_ a));
  let new_array = Array.make newcapacity filler in
  Array.blit a.arr 0 new_array 0 a.size;
  fill_with_junk_ new_array a.size (newcapacity-a.size) ~filler;
  a.arr <- new_array

(* grow the array, using [x] as a temporary filler if required *)
let actually_grow_with_ a ~filler : unit =
  if array_is_empty_ a then (
    let len = 4 in
    a.arr <- Array.make len filler;
  ) else (
    let n = Array.length a.arr in
    let size = next_grow_ n in
    if size = n then invalid_arg "Dynarray: cannot grow the array";
    actually_resize_array_ a size ~filler
  )

(* [v] is not empty; ensure it has at least [size] slots.

   Use {!resize_} so that calling [ensure_capacity v (length v+1)] in a loop
   is still behaving well. *)
let ensure_assuming_not_empty_ v ~size =
  if size > Sys.max_array_length then (
    invalid_arg "arr.ensure: size too big"
  ) else if size > Array.length v.arr then (
    let n = ref (Array.length v.arr) in
    while !n < size do n := next_grow_ !n done;
    let filler = v.arr.(0) in
    actually_resize_array_ v !n ~filler;
  )

let ensure_capacity_with v ~filler size : unit =
  if array_is_empty_ v then (
    v.arr <- Array.make size filler;
  ) else (
    ensure_assuming_not_empty_ v ~size
  )

let ensure_capacity_nonempty v size : unit =
  if array_is_empty_ v then
    invalid_arg "Dynarray.ensure_capacity_nonempty: empty";
  ensure_assuming_not_empty_  v ~size

let[@inline] clear v =
  v.size <- 0

let[@inline] is_empty v = v.size = 0

let[@inline] unsafe_push_last v x =
  Array.set v.arr v.size x;
  v.size <- v.size + 1

let push_last v x =
  if v.size = Array.length v.arr then actually_grow_with_ v ~filler:x;
  unsafe_push_last v x

let append a b =
  if array_is_empty_ a then (
    if array_is_empty_ b then () else (
      a.arr <- Array.copy b.arr;
      a.size <- b.size
    )
  ) else (
    ensure_assuming_not_empty_ a ~size:(a.size + b.size);
    assert (Array.length a.arr >= a.size + b.size);
    Array.blit b.arr 0 a.arr a.size b.size;
    a.size <- a.size + b.size
  )

let[@inline] get v i =
  if i < 0 || i >= v.size then invalid_arg "Dynarray.get";
  Array.get v.arr i

let[@inline] set v i x =
  if i < 0 || i >= v.size then invalid_arg "Dynarray.set";
  Array.set v.arr i x

let append_seq a seq = Seq.iter (fun x -> push_last a x) seq

let append_array a b =
  let len_b = Array.length b in
  if array_is_empty_ a then (
    a.arr <- Array.copy b;
    a.size <- len_b;
  ) else (
    ensure_assuming_not_empty_ a ~size:(a.size + len_b);
    Array.blit b 0 a.arr a.size len_b;
    a.size <- a.size + len_b
  )

let append_list a b = match b with
  | [] -> ()
  | x :: _ ->
    (* use [x] as the filler, in case the array is empty.
       We ensure capacity once, then we can skip the resizing checks
       and use {!unsafe_push_last}. *)
    let len_a = a.size in
    let len_b = List.length b in
    ensure_capacity_with ~filler:x a (len_a + len_b);
    List.iter (unsafe_push_last a) b

let pop_last v =
  if v.size = 0 then raise Not_found;
  let new_size = v.size - 1 in
  v.size <- new_size;
  let x = v.arr.(new_size) in
  if new_size = 0 then (
      v.arr <- [||]; (* free elements *)
    ) else (
      (* remove pointer to (removed) last element *)
      let filler = Array.get v.arr 0 in
      fill_ v.arr new_size ~filler;
    );
  x

let pop_last_opt v =
  try Some (pop_last v)
  with Not_found -> None

let remove_last v =
  try ignore (pop_last v)
  with Not_found -> ()

let[@inline] copy v = {
  size = v.size;
  arr = Array.sub v.arr 0 v.size;
}

let truncate v n =
  let old_size = v.size in
  if n = 0 then (
    v.size <- n;
    (* free all elements *)
    v.arr <- [||];
  ) else if n < old_size then (
    (* free elements by erasing them with the first element *)
    v.size <- n;
    let filler = Array.get v.arr 0 in
    fill_with_junk_ v.arr n (old_size-n) ~filler;
  )

let shrink_capacity v : unit =
  if v.size = 0 then (
    v.arr <- [| |]
  ) else if v.size < Array.length v.arr then (
    v.arr <- Array.sub v.arr 0 v.size
  )

let iter k v =
  let n = v.size in
  for i = 0 to n-1 do
    k (Array.get v.arr i)
  done

let iteri k v =
  let n = v.size in
  for i = 0 to n-1 do
    k i (Array.get v.arr i)
  done

let map f v =
  if array_is_empty_ v
  then create ()
  else (
    let arr = Array.init v.size (fun i -> f (Array.get v.arr i)) in
    { size=v.size; arr; }
  )

let mapi f v =
  if array_is_empty_ v
  then create ()
  else (
    let arr = Array.init v.size (fun i -> f i (Array.get v.arr i)) in
    { size=v.size; arr; }
  )

let fold_left f acc v =
  let rec fold acc i =
    if i = v.size then acc
    else
      let x = Array.get v.arr i in
      fold (f acc x) (i+1)
  in fold acc 0

let filter f a =
  let b = create() in
  iter (fun x -> if f x then push_last b x) a;
  b

let filter_map f a =
  let b = create() in
  iter (fun x ->
      match f x with
      | None -> ()
      | Some y -> push_last b y)
    a;
  b

let exists p v =
  let n = v.size in
  let rec check i =
    if i = n then false
    else p v.arr.(i) || check (i+1)
  in check 0

let for_all p v =
  let n = v.size in
  let rec check i =
    if i = n then true
    else p v.arr.(i) && check (i+1)
  in check 0

let length v = v.size

let of_seq seq =
  let init = create() in
  append_seq init seq;
  init

let to_seq v =
  let rec aux i () =
    if i>= length v then Seq.Nil
    else Seq.Cons (v.arr.(i), aux (i+1))
  in
  aux 0

let to_seq_rev v =
  let rec aux i () =
    if i<0 || i > length v then Seq.Nil
    else Seq.Cons (v.arr.(i), aux (i-1))
  in
  aux (length v-1)

let of_array a =
  if Array.length a = 0
  then create ()
  else {
    size=Array.length a;
    arr=Array.copy a;
  }

let of_list l = match l with
  | [] -> create()
  | [x] -> make 1 x
  | [x;y] -> {size=2; arr=[| x; y |]}
  | x::_ ->
    let v = create() in
    ensure_capacity_with v (List.length l) ~filler:x;
    List.iter (unsafe_push_last v) l;
    v

let to_array v =
  Array.sub v.arr 0 v.size

let to_list v =
  let l = ref [] in
  for i=length v-1 downto 0 do
    l := get v i :: !l
  done;
  !l
