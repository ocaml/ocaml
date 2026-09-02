(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
readonly_files = "index_nested_record_types.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_nested_record_types.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_nested_record_types.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

type t = { a : { x : int; b : { y : int } } }

(* One-level projected type annotation *)
let f (v : t.a) = v.x

(* Deep projected type annotation *)
let h (d : t.a.b) = d.y

(* Ordinary label and one-level projection as regression controls *)
let g (w : t) = w.a

module M = struct
  type mt = { ma : { mx : int } }
end

(* Module-qualified projected type annotation *)
let fm (mv : M.mt.ma) = mv.mx

(* Projected type through a re-export *)
type r = t = { a : { x : int; b : { y : int } } }
let fr (rv : r.a) = rv.x

(* Constructor inline-record paths as regression controls *)
type c = C of { cl : int }
let fc (C { cl }) = cl
