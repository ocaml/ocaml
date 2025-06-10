(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_local_opaque_defs.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -uid-deps -decls index_local_opaque_defs.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)


module type S (* 1 *) = sig
  val u : unit (* 0 *)
end

module G (* 3 *) (X : S) = struct
  let () = X.u (* Reduces to L0 -> 0 *)
  let () = X.u (* Reduces to L0 -> 0 *)
  include X
end

module H (* 6 *)(X : S) = struct
  let () = X.u (* Reduces to L1 -> 0 *)
  let () = X.u (* Reduces to L1 -> 0 *)
  module Y (* 5 *) = G(X)
  let () = Y.u (* Reduces to L1 -> 0 *)
end
