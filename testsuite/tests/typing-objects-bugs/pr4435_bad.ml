(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Two v's in the same class *)
class c v = object initializer  print_endline v val v = 42 end;;
new c "42";;

(* Two hidden v's in the same class! *)
class c (v : int) =
  object
    method v0 = v
    inherit ((fun v -> object method v : string = v end) "42")
  end;;
(new c 42)#v0;;
