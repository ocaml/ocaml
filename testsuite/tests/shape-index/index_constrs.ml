(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
readonly_files = "index_constrs.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_constrs.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_constrs.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

exception E
module M = struct
  exception F = E
end

type t = E

let x_ = E
let () = raise E
let f x = match x with
  | E -> ()
  | exception E -> ()

let _ = None
let _ = [%extension_constructor M.F]
