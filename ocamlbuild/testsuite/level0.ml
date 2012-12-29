#use "topfind";;
#require "unix";;

#use "ocamlbuild_test.ml";;

module M = Match;;
module T = Tree;;

let _build = M.d "_build";;

test "BasicNativeTree"
  ~description:"Output tree for native compilation"
  ~tree:(T.f "dummy.ml")
  ~matching:(M.Exact
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
                       "_log"])))
  ~targets:("dummy.native",[]);;

test "BasicByteTree"
  ~description:"Output tree for byte compilation"
  ~tree:(T.f "dummy.ml")
  ~matching:(M.Exact
               (_build
                  (M.lf
                      ["_digests";
                       "dummy.cmi";
                       "dummy.cmo";
                       "dummy.ml";
                       "dummy.ml.depends";
                       "dummy.byte";
                       "_log"])))
  ~targets:("dummy.byte",[]);;

test "SeveralTargets"
  ~description:"Several targets"
  ~tree:(T.f "dummy.ml")
  ~matching:(_build (M.lf ["dummy.byte"; "dummy.native"]))
  ~targets:("dummy.byte",["dummy.native"]);;

let alt_build_dir = "BuIlD2";;

test "BuildDir"
  ~options:[`build_dir alt_build_dir]
  ~description:"Different build directory"
  ~tree:(T.f "dummy.ml")
  ~matching:(M.d alt_build_dir (M.lf ["dummy.byte"]))
  ~targets:("dummy.byte",[]);;


run ~root:"_test";;
