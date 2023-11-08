(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_bindingops.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index_bindingops.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
check-program-output;
*)

let (let+) x f = Option.map f x

let (and+) x y =
  Option.bind x @@ fun x ->
  Option.map (fun y -> (x, y)) y

let minus_three =
  let+ foo = None
  and+ bar = None
  and+ man = None in
  foo + bar - man

let _ = (let+)
let _ = (and+)
