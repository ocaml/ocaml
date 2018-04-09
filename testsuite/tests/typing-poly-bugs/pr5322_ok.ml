(* TEST
flags = " -w a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

type 'par t = 'par
module M : sig val x : <m : 'a. 'a> end =
  struct let x : <m : 'a. 'a t> = Obj.magic () end

let ident v = v
class alias = object method alias : 'a . 'a t -> 'a = ident end
