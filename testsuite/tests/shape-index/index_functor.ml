(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_functor.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -uid-deps -decls index_functor.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)


module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
include O
include N

module Id (X : sig type t end) = X
module type S = sig val s : unit end
module G (X : sig type t = int val x : t module M : S end) = struct
  type t = X.t
  let y = X.x

  module Y = X (* FIXME: this "alias" shape  is not a shape alias  *)

  let _ = Y.x

  let () = X.M.s

  type u = Id (X).t
end
