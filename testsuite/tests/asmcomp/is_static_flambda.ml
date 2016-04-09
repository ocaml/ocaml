(* Data that should be statically allocated by the compiler (flambda only) *)

external is_in_static_data : 'a -> bool = "caml_is_in_static_data"

(* Also after inlining *)
let g x =
  let block = (1,x) in
  assert(is_in_static_data block)

let () = (g [@inlined always]) 2

(* Toplevel immutable blocks should be static *)
let block3 = (Sys.opaque_identity 1, Sys.opaque_identity 2)
let () = assert(is_in_static_data block3)

(* Not being bound shouldn't prevent it *)
let () =
  assert(is_in_static_data (Sys.opaque_identity 1, Sys.opaque_identity 2))

(* Only with rounds >= 2 currently !
(* Also after inlining *)
let h x =
  let block = (Sys.opaque_identity 1,x) in
  assert(is_in_static_data block)

let () = (h [@inlined always]) (Sys.opaque_identity 2)
*)

(* Recursive constant values should be static *)
let rec a = 1 :: b
and b = 2 :: a
let () =
  assert(is_in_static_data a);
  assert(is_in_static_data b)

(* And a mix *)
type e = E : 'a -> e

let rec f1 a = E (g1 a, l1)
and g1 a = E (f1 a, l2)
and l1 = E (f1, l2)
and l2 = E (g1, l1)

let () =
  assert(is_in_static_data f1);
  assert(is_in_static_data g1);
  assert(is_in_static_data l1);
  assert(is_in_static_data l2)

(* Also in functions *)
let i () =
  let rec f1 a = E (g1 a, l1)
  and g1 a = E (f1 a, l2)
  and l1 = E (f1, l2)
  and l2 = E (g1, l1) in

  assert(is_in_static_data f1);
  assert(is_in_static_data g1);
  assert(is_in_static_data l1);
  assert(is_in_static_data l2)

let () = (i [@inlined never]) ()

module type P = module type of Pervasives
(* Top-level modules should be static *)
let () = assert(is_in_static_data (module Pervasives:P))

(* Not constant let rec to test extraction to initialize_symbol *)
let r = ref 0
let rec a = (incr r; !r) :: b
and b = (incr r; !r) :: a

let next =
  let r = ref 0 in
  fun () -> incr r; !r

let () =
  assert(is_in_static_data next)

(* Exceptions without arguments should be static *)
exception No_argument
let () = assert(is_in_static_data No_argument)

(* And also with constant arguments *)
exception Some_argument of string
let () = assert(is_in_static_data (Some_argument "some string"))

(* Even when exposed by inlining *)
let () =
  let exn =
    try (failwith [@inlined always]) "some other string" with exn -> exn
  in
  assert(is_in_static_data exn)
