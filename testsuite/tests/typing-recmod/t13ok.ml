(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* OK *)
class type [ 'node ] extension = object method node : 'node end
class type [ 'ext ] node = object constraint 'ext = 'ext node #extension end
class x = object method node : x node = assert false end
type t = x node;;
