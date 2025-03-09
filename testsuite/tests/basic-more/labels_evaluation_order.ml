(* TEST
*)

[@@@warning "-unerasable-optional-argument"]
let foo ?a =
    print_endline "a parameter";
    fun ~b ->
    print_endline "b parameter";
    fun ~c ->
    print_endline "c parameter"

let f = foo ~a:(print_endline "a argument") ~c:(print_endline "c argument")

let _ = print_endline "f defined"

let _ = f ~b:(print_endline "b argument")
