#load "unix.cma";;

let ocamlbuild = try Sys.getenv "OCAMLBUILD" with Not_found -> "ocamlbuild";;

#use "ocamlbuild_test.ml";;

module M = Match;;
module T = Tree;;

let _build = M.d "_build";;

test "BasicNativeTree"
  ~description:"Output tree for native compilation"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.Exact
               (_build
                  (M.lf
                      ["_digests";
                       "dummy.cmi";
                       "dummy.cmo";
                       "dummy.cmx";
                       "dummy.ml";
                       "dummy.ml.depends";
                       "dummy.native";
                       "dummy.o";
                       "_log"]))]
  ~targets:("dummy.native",[]) ();;

test "BasicByteTree"
  ~description:"Output tree for byte compilation"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.Exact
               (_build
                  (M.lf
                      ["_digests";
                       "dummy.cmi";
                       "dummy.cmo";
                       "dummy.ml";
                       "dummy.ml.depends";
                       "dummy.byte";
                       "_log"]))]
  ~targets:("dummy.byte",[]) ();;

test "SeveralTargets"
  ~description:"Several targets"
  ~tree:[T.f "dummy.ml"]
  ~matching:[_build (M.lf ["dummy.byte"; "dummy.native"])]
  ~targets:("dummy.byte",["dummy.native"]) ();;

let alt_build_dir = "BuIlD2";;

test "BuildDir"
  ~options:[`build_dir alt_build_dir]
  ~description:"Different build directory"
  ~tree:[T.f "dummy.ml"]
  ~matching:[M.d alt_build_dir (M.lf ["dummy.byte"])]
  ~targets:("dummy.byte",[]) ();;

test "camlp4.opt"
  ~description:"Fixes PR#5652"
  ~options:[`use_ocamlfind; `package "camlp4.macro";`tags ["camlp4o.opt"; "syntax\\(camp4o\\)"];
            `ppflag "camlp4o.opt"; `ppflag "-parser"; `ppflag "macro"; `ppflag "-DTEST"]
  ~tree:[T.f "dummy.ml" ~content:"IFDEF TEST THEN\nprint_endline \"Hello\";;\nENDIF;;"]
  ~matching:[M.x "dummy.native" ~output:"Hello"]
  ~targets:("dummy.native",[]) ();;

let tag_pat_msgs =
  ["*:a", "File \"_tags\", line 1, column 0: Lexing error: Invalid globbing pattern \"*\".";
   "\n<*{>:a", "File \"_tags\", line 2, column 0: Lexing error: Invalid globbing pattern \"<*{>\".";
   "<*>: ~@a,# ~a", "File \"_tags\", line 1, column 10: Lexing error: Only ',' separated tags are alllowed."];;

List.iteri (fun i (content,failing_msg) ->
  test (Printf.sprintf "TagsErrorMessage_%d" (i+1))
    ~description:"Confirm relevance of an error message due to erronous _tags"
    ~failing_msg
    ~tree:[T.f "_tags" ~content; T.f "dummy.ml"]
    ~targets:("dummy.native",[]) ()) tag_pat_msgs;;

test "SubtoolOptions"
  ~description:"Options that come from tags that needs to be spliced to the subtool invocation (PR#5763)"
  ~options:[`use_menhir; `use_ocamlfind;`tags["package\\(camlp4.fulllib\\)"]]
  ~tree:[T.f "parser.mly" ~content:"%{\n%}\n%token DUMMY\n%start<Camlp4.PreCast.Syntax.Ast.expr option> test%%test: {None}\n\n"]
  ~matching:[M.f "parser.native"; M.f "parser.byte"]
  ~targets:("parser.native",["parser.byte"])
  ();;

test "Itarget"
  ~description:".itarget building with dependencies between the modules (PR#5686)"
  ~tree:[T.f "foo.itarget" ~content:"a.cma\nb.byte\n"; T.f "a.ml"; T.f "b.ml" ~content:"open A\n"]
  ~matching:[M.f "a.cma"; M.f "b.byte"]
  ~targets:("foo.otarget",[]) ();;

test "PackAcross"
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.f "main.ml" ~content:"let _ = Pack.Packed.g ()\n";
         T.f "Pack.mlpack" ~content:"pack/Packed";
         T.f "_tags" ~content:"<lib>: include\n<pack/*.cmx>: for-pack(Pack)\n";
         T.d "lib" [T.f "Lib.ml" ~content:"let f()=()";
                    T.f "Lib.mli" ~content:"val f : unit -> unit"];
         T.d "pack" [T.f "Packed.ml" ~content:"let g() = Lib.f ()"]]
  ~matching:[M.f "main.byte"; M.f "main.native"]
  ~targets:("main.byte", ["main.native"])
  ();;

test "SyntaxFlag"
  ~description:"-syntax for ocamlbuild"
  ~options:[`use_ocamlfind; `package "camlp4.macro"; `syntax "camlp4o"]
  ~tree:[T.f "dummy.ml" ~content:"IFDEF TEST THEN\nprint_endline \"Hello\";;\nENDIF;;"]
  ~matching:[M.f "dummy.native"]
  ~targets:("dummy.native",[]) ();;

run ~root:"_test";;
