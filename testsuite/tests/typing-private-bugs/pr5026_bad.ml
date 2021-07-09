(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

type untyped;;
type -'a typed = private untyped;;
type -'typing wrapped = private sexp
and +'a t = 'a typed wrapped
and sexp = private untyped wrapped;;
class type ['a] s3 = object
  val underlying : 'a t
end;;
class ['a] s3object r : ['a] s3 = object
  val underlying = r
end;;
