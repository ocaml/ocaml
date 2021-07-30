(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let rec foldl op acc = function
    [] -> acc
    | x :: xs ->
        try (foldl [@tailcall]) op (op x acc) xs
        with Not_found -> assert false
