(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module M :
   sig
     class x : int -> object method m : int end
  end
=
struct
  class x _ = object
    method m = 42
  end
end;;
