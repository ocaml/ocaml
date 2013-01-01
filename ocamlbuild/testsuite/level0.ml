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

test "TagsErrorMessage"
  ~description:"Confirm relevance of an error message due to erronous _tags"
  ~failing_msg:"Failure: lexing: empty token."
  ~tree:[T.f "_tags" ~content:"* : not_hygienic"]
  ~targets:("none.native",[]) ();;

run ~root:"_test";;
