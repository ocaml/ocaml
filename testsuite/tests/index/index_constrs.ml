(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
readonly_files = "index_constrs.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_constrs.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index_constrs.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
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
