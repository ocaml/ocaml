(* TEST
   native;
*)

(* [Cmm_helpers.array_indexing] simplifies tagging/untagging of the indexing
expression. Since tagging/untagging rewrites the highest bit, this operation
is not neutral when the element size is 1 (meaning that no additional shift
for element size is needed that would get rid of this bit). *)

open Bigarray

let index = Int64.add Int64.min_int 5L

let[@inline never] get_char
    (b : (char, int8_unsigned_elt, c_layout) Array1.t) x =
  Array1.unsafe_get b (Int64.to_int x)

let[@inline never] get_int8
    (b : (int, int8_signed_elt, c_layout) Array1.t) x =
  Array1.unsafe_get b (Int64.to_int x)

let[@inline never] get_uint8
    (b : (int, int8_unsigned_elt, c_layout) Array1.t) x =
  Array1.unsafe_get b (Int64.to_int x)

let[@inline never] set_char
    (b : (char, int8_unsigned_elt, c_layout) Array1.t) x v =
  Array1.unsafe_set b (Int64.to_int x) v

let check name ok = Printf.printf "%s: %s\n%!" name (if ok then "ok" else "FAIL")

let () =
  assert (Int64.to_int index = 5);
  let chars = Array1.of_array char c_layout [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |] in
  check "char unsafe_get" (get_char chars index = 'f');
  let int8s = Array1.of_array int8_signed c_layout [| 0; -1; -2; -3; -4; -5 |] in
  check "int8_signed unsafe_get" (get_int8 int8s index = -5);
  let uint8s = Array1.of_array int8_unsigned c_layout [| 0; 1; 2; 3; 4; 5 |] in
  check "int8_unsigned unsafe_get" (get_uint8 uint8s index = 5);
  set_char chars index 'z';
  check "char unsafe_set" (Array1.get chars 5 = 'z')
