(* TEST
flags = " -w a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* OK *)
module rec A : sig type 'a t = 'a array B.t * 'a list B.t end
             = struct type 'a t = 'a array B.t * 'a list B.t end
       and B : sig type 'a t = <m: 'a B.t> end
             = struct type 'a t = <m: 'a B.t> end;;
