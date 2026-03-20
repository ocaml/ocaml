(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(* In this file, we use four different implementation strategies:

     1. Reusing [external]s for mutable arrays.  (E.g., [get].)
     2. Copying implementations from [array.ml], which in this new context read
        from immutable arrays.  (E.g., [iter].)
     3. As (2), but they construct a mutable array, which we unsafely
        reinterpret as an immutable array at the very end (E.g., [map].)
     4. (Only for sorting.) Copying the immutable array and passing it to a
        mutating function.  (E.g., [sort].)

   The first and third strategies are safe because mutable and immutable arrays
   have the same runtime representation, and we only apply them to functions
   that satisfy the following three properties:

     A. They do not mutate their array inputs;
     B. They do not hold on to their array inputs; and
     C. They return a fresh array if they return an array.

   We do not expose other primitives (e.g., [unsafe_set]) or functions (e.g.,
   [fill]).

   We choose between the four strategies as follows:

     1. We use [external]s if there is a corresponding [external].
     2. Functions that only read arrays can have their implementations safely
        copied.
     3. Functions that create an immutable array have to do so by creating a
        mutable array and freezing it, since immutable arrays are, well,
        immutable.  For efficiency, we freeze them unsafely.
     4. Sorting is implemented in-place and this allows us to reuse it.
*)

(* An alias for the type of immutable arrays. *)
type +'a t = 'a iarray

(* Array operations *)

external length : 'a iarray -> int = "%array_length"
external get : 'a iarray -> int -> 'a = "%array_safe_get"
external unsafe_get : 'a iarray -> int -> 'a = "%array_unsafe_get"
external concat : 'a iarray list -> 'a iarray = "caml_array_concat"

external append_prim : 'a iarray -> 'a iarray -> 'a iarray = "caml_array_append"
external unsafe_sub : 'a iarray -> int -> int -> 'a iarray = "caml_array_sub"
external unsafe_of_array : 'a array -> 'a iarray = "%opaque"
external unsafe_to_array : 'a iarray -> 'a array = "%opaque"

let init l f = unsafe_of_array (Array.init l f)

let append a1 a2 =
  if length a1 = 0 then a2 (* Safe because they're immutable *)
  else if length a2 = 0 then a1
  else append_prim a1 a2

let sub a ~pos ~len =
  if pos < 0 || len < 0 || pos > length a - len
  then invalid_arg "Iarray.sub"
  else unsafe_sub a pos len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iter2 f a b =
  if length a <> length b then
    invalid_arg "Iarray.iter2: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let map f a =
  let l = length a in
  let r = if l = 0 then [||] else begin
    let r = Array.make l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end in
  unsafe_of_array r

let map2 f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2: arrays must have the same length"
  else begin
    let r = if la = 0 then [||] else begin
      let r = Array.make la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        Array.unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      r
    end in
    unsafe_of_array r
  end

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  let l = length a in
  let r = if l = 0 then [||] else begin
    let r = Array.make l (f 0 (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f i (unsafe_get a i))
    done;
    r
  end in
  unsafe_of_array r

let to_list a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

let of_list l = unsafe_of_array (Array.of_list l)

let to_array ia = Array.copy (unsafe_to_array ia)

let of_array ma = unsafe_of_array (Array.copy ma)

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_left_map f acc input_array =
  let len = length input_array in
  let acc, output_array = if len = 0 then (acc, [||]) else begin
    let acc, elt = f acc (unsafe_get input_array 0) in
    let output_array = Array.make len elt in
    let acc = ref acc in
    for i = 1 to len - 1 do
      let acc', elt = f !acc (unsafe_get input_array i) in
      acc := acc';
      Array.unsafe_set output_array i elt;
    done;
    !acc, output_array
  end in
  acc, unsafe_of_array output_array

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let exists p a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let for_all p a =
  let n = length a in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  loop 0

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let exists2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.exists2"
  else let rec loop i =
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  loop 0

let equal eq a1 a2 =
  length a1 = length a2 && for_all2 eq a1 a2

let compare cmp a1 a2 =
  if length a1 <> length a2 then length a1 - length a2
  else (
    let rec loop i =
      if i = length a1 then 0
      else
        let c = cmp (unsafe_get a1 i) (unsafe_get a2 i) in
        if c <> 0 then c
        else loop (i + 1)
    in
    loop 0
  )

let mem x a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if Stdlib.compare (unsafe_get a i) x = 0 then true
    else loop (succ i) in
  loop 0

let memq x a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if x == (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let find_opt p a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      let x = unsafe_get a i in
      if p x then Some x
      else loop (succ i)
  in
  loop 0

let find_index p a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else if p (unsafe_get a i) then Some i
    else loop (succ i) in
  loop 0

let find_map f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

let find_mapi f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f i (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

let split x =
  if equal (=) (* unused *) x [||] then ([||], [||] : _ iarray * _ iarray)
  else begin
    let a0, b0 = unsafe_get x 0 in
    let n = length x in
    let a = Array.make n a0 in
    let b = Array.make n b0 in
    for i = 1 to n - 1 do
      let ai, bi = unsafe_get x i in
      Array.unsafe_set a i ai;
      Array.unsafe_set b i bi
    done;
    unsafe_of_array a, unsafe_of_array b
  end

let combine a b =
  let na = length a in
  let nb = length b in
  if na <> nb then invalid_arg "Iarray.combine";
  let r = if na = 0 then [||]
  else begin
    let x = Array.make na (unsafe_get a 0, unsafe_get b 0) in
    for i = 1 to na - 1 do
      Array.unsafe_set x i (unsafe_get a i, unsafe_get b i)
    done;
    x
  end in
  unsafe_of_array r

(* Must be fully applied due to the value restriction *)
let lift_sort sorter cmp iarr =
  let arr = to_array iarr in
  sorter cmp arr;
  unsafe_of_array arr

let sort cmp iarr = lift_sort Array.sort cmp iarr
let stable_sort cmp iarr = lift_sort Array.stable_sort cmp iarr
let fast_sort cmp iarr = lift_sort Array.fast_sort cmp iarr

let to_seq a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons (x, aux (i+1))
    else Seq.Nil
  in
  aux 0

let to_seqi a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons ((i,x), aux (i+1))
    else Seq.Nil
  in
  aux 0

let of_seq i = unsafe_of_array (Array.of_seq i)
