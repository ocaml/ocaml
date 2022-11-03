(* TEST
readonly_files = "a.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** ocamlc.byte
module = "b_bad.ml"
flags = "-warn-error +8"
ocamlc_byte_exit_status = "2"
**** check-ocamlc.byte-output
*)

let f : string A.t -> unit = function
    A.X s -> print_endline s

(* It is important that the line below is the last line of the file
   (see Makefile) *)
let () = f A.y
