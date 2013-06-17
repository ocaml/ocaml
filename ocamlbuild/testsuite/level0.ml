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

test "PackAcross2"
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.f "a2.mli" ~content:"val f : unit -> unit";
         T.f "a2.ml" ~content:"let f _ = ()";
         T.f "lib.ml" ~content:"module A = A2";
         T.f "b.ml" ~content:"let g = Lib.A.f";
         T.f "sup.mlpack" ~content:"B";
         T.f "prog.ml" ~content:"Sup.B.g"]
  ~matching:[M.f "prog.byte"]
  ~targets:("prog.byte",[]) ();;

test "PackAcross3"
  ~description:"Pack using a module from the other tree (PR#4592)"
  ~tree:[T.d "foo" [ T.f "bar.ml" ~content:"let baz = Quux.xyzzy"];
         T.f "foo.mlpack" ~content:"foo/Bar";
         T.f "main.ml" ~content:"prerr_endline Foo.Bar.baz";
         T.f "myocamlbuild.ml";
         T.f "quux.ml" ~content:"let xyzzy = \"xyzzy\"";
         T.f "quux.mli" ~content:"val xyzzy : string"]
  ~matching:[M.f "main.byte"]
  ~targets:("main.byte",[]) ();;

test "SyntaxFlag"
  ~description:"-syntax for ocamlbuild"
  ~options:[`use_ocamlfind; `package "camlp4.macro"; `syntax "camlp4o"]
  ~tree:[T.f "dummy.ml" ~content:"IFDEF TEST THEN\nprint_endline \"Hello\";;\nENDIF;;"]
  ~matching:[M.f "dummy.native"]
  ~targets:("dummy.native",[]) ();;

test "NoIncludeNoHygiene1"
  ~description:"check that hygiene checks are only done in traversed directories\
                (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:"<must_ignore>: -traverse"]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

test "NoIncludeNoHygiene2"
  ~description:"check that hygiene checks are not done on the -build-dir \
                (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:""]
  ~options:[`build_dir "must_ignore"]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

test "NoIncludeNoHygiene3"
  ~description:"check that hygiene checks are not done on excluded dirs (PR#4502)"
  ~tree:[T.d "must_ignore" [ T.f "dirty.mli" ~content:"val bug : int"];
         T.f "hello.ml" ~content:"print_endline \"Hello, World!\"";
         T.f "_tags" ~content:""]
  ~options:[`X "must_ignore"]
  ~pre_cmd:"ocamlc -c must_ignore/dirty.mli"
            (* will make hygiene fail if must_ignore/ is checked *)
  ~targets:("hello.byte",[]) ();;

run ~root:"_test";;
