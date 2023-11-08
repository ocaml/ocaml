(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
readonly_files = "index_types.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_types.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index_types.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
check-program-output;
*)

type t = int

let x : t = 42 (* FIXME? duplicate Tpat_extra_constraint / Texp_constraint *)

module M = struct end

let () = match 4 with
  | (_ : t) -> ()

type poly = [`A|`B]

let () = match `A with #poly -> ()

module type S = sig
  type t2 = ..
  type t2 += B
end

type t1 = ..
type t1 += B
