(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Bad (not regular) *)
module rec M :
    sig
      class ['a] c : 'a -> object
        method map : ('a -> 'b) -> 'b M.c
      end
    end
  = struct
      class ['a] c (x : 'a) = object
        method map : 'b. ('a -> 'b) -> 'b M.c
          = fun f -> new M.c (f x)
      end
    end;;
