(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_aliases.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_aliases.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)


module A = struct type t end
module B = A

module F (X : sig type t end) = X
module F' = F
module C = F'(A)

module C' = F(B)
module D = C

module G = B
include G

module type S = sig
  module M : sig val s : unit end
  module F : functor (S : sig end ) -> sig type t end
 end
let x = (module struct
    module M = struct let s = () end
    module F (_ : sig end) = struct type t end
  end : S)

module X = (val x)
module Y = X.M
module Z = Y

(* FIXME: this sould be (Approx (No_uid)), not (Internal_error_no_uid) *)
let _ = Z.s

module Arg = struct end
module FArg = X.F (Arg)
open FArg

(* FIXME: this sould be (Approx (No_uid)), not (Internal_error_no_uid) *)
type u = t
