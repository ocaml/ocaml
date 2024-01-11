(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
readonly_files = "index_types.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_types.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_types.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

type t = int

let x : t = 42

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

(* 5.2 local open for types *)
module N = struct type t end
type u = N.(t)
