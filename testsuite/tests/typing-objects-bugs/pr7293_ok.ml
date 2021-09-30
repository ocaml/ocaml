(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

type t = T : t
type s = T

class c = object (self : 'self)

  method foo : s -> 'self = function
    | T -> self#bar ()

  method bar : unit -> 'self = fun () -> self

end
