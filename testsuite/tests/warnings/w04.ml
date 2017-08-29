[@@@ocaml.warning "+4"]

type expr = E of int [@@unboxed]

      
let f x = match x with (E e) -> e

type t = A | B

let g x = match x with
| A -> 0
| _ -> 1

type u = C | D [@@ ocaml.warning "-4"]
(* should not warn. *)
let h x = match x with
| C -> 0
| _ -> 1

[@@@ocaml.warning "-4"]

type v = F | G [@@ ocaml.warning "+4"]

let k x = match x with
| F -> 0
| _ -> 1

(* should not warn. *)
let l x = match x with
| A -> 0
| _ -> 1

[@@@ocaml.warning "--4"]
(* should not warn. *)
let m x = match x with
| F -> 0
| _ -> 1

[@@@ocaml.warning "++4"]
(*should warn. *)
let n x = match x with
| C -> 0
| _ -> 1
