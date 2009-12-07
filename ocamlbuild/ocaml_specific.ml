(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Rule
open Tools
open Rule.Common_commands
open Outcome
open Command;;

open Ocaml_utils

module C_tools = struct
  let link_C_library clib a libname env build =
    let clib = env clib and a = env a and libname = env libname in
    let objs = string_list_of_file clib in
    let include_dirs = Pathname.include_dirs_of (Pathname.dirname a) in
    let obj_of_o x =
      if Filename.check_suffix x ".o" && !Options.ext_obj <> "o" then
        Pathname.update_extension !Options.ext_obj x
      else x in
    let resluts = build (List.map (fun o -> List.map (fun dir -> dir / obj_of_o o) include_dirs) objs) in
    let objs = List.map begin function
      | Good o -> o
      | Bad exn -> raise exn
    end resluts in
    Cmd(S[!Options.ocamlmklib; A"-o"; Px libname; T(tags_of_pathname a++"c"++"ocamlmklib"); atomize objs]);;
end

open Flags
open Command
open Rule

let init () = let module M = struct

let ext_lib = !Options.ext_lib;;
let ext_obj = !Options.ext_obj;;
let ext_dll = !Options.ext_dll;;
let x_o = "%"-.-ext_obj;;
let x_a = "%"-.-ext_lib;;
let x_dll = "%"-.-ext_dll;;
let x_p_o = "%.p"-.-ext_obj;;
let x_p_a = "%.p"-.-ext_lib;;

rule "target files"
  ~dep:"%.itarget"
  ~stamp:"%.otarget"
  begin fun env build ->
    let itarget = env "%.itarget" in
    let dir = Pathname.dirname itarget in
    List.iter ignore_good
      (build (List.map (fun x -> [dir/x]) (string_list_of_file itarget)));
    Nop
  end;;

rule "ocaml: mli -> cmi"
  ~tags:["ocaml"]
  ~prod:"%.cmi"
  ~deps:["%.mli"; "%.mli.depends"]
  (Ocaml_compiler.byte_compile_ocaml_interf "%.mli" "%.cmi");;

rule "ocaml: mlpack & d.cmo* -> d.cmo & cmi"
  ~tags:["ocaml"; "debug"; "byte"]
  ~prods:["%.d.cmo"]
  ~deps:["%.mlpack"; "%.cmi"]
  (Ocaml_compiler.byte_debug_pack_mlpack "%.mlpack" "%.d.cmo");;

rule "ocaml: mlpack & cmo* & cmi -> cmo"
  ~tags:["ocaml"; "byte"]
  ~prod:"%.cmo"
  ~deps:["%.mli"; "%.cmi"; "%.mlpack"]
  (Ocaml_compiler.byte_pack_mlpack "%.mlpack" "%.cmo");;

rule "ocaml: mlpack & cmo* -> cmo & cmi"
  ~tags:["ocaml"; "byte"]
  ~prods:["%.cmo"; "%.cmi"]
  ~dep:"%.mlpack"
  (Ocaml_compiler.byte_pack_mlpack "%.mlpack" "%.cmo");;

rule "ocaml: ml & cmi -> d.cmo"
  ~tags:["ocaml"; "byte"]
  ~prod:"%.d.cmo"
  ~deps:["%.mli"(* This one is inserted to force this rule to be skiped when
    a .ml is provided without a .mli *); "%.ml"; "%.ml.depends"; "%.cmi"]
  (Ocaml_compiler.byte_compile_ocaml_implem ~tag:"debug" "%.ml" "%.d.cmo");;

rule "ocaml: ml & cmi -> cmo"
  ~tags:["ocaml"; "byte"]
  ~prod:"%.cmo"
  ~deps:["%.mli"(* This one is inserted to force this rule to be skiped when
    a .ml is provided without a .mli *); "%.ml"; "%.ml.depends"; "%.cmi"]
  (Ocaml_compiler.byte_compile_ocaml_implem "%.ml" "%.cmo");;

rule "ocaml: mlpack & cmi & p.cmx* & p.o* -> p.cmx & p.o"
  ~tags:["ocaml"; "profile"; "native"]
  ~prods:["%.p.cmx"; x_p_o(* no cmi here you must make the byte version to have it *)]
  ~deps:["%.mlpack"; "%.cmi"]
  (Ocaml_compiler.native_profile_pack_mlpack "%.mlpack" "%.p.cmx");;

rule "ocaml: mlpack & cmi & cmx* & o* -> cmx & o"
  ~tags:["ocaml"; "native"]
  ~prods:["%.cmx"; x_o(* no cmi here you must make the byte version to have it *)]
  ~deps:["%.mlpack"; "%.cmi"]
  (Ocaml_compiler.native_pack_mlpack "%.mlpack" "%.cmx");;

rule "ocaml: ml & cmi -> p.cmx & p.o"
  ~tags:["ocaml"; "native"; "profile"]
  ~prods:["%.p.cmx"; x_p_o]
  ~deps:["%.ml"; "%.ml.depends"; "%.cmi"]
  (Ocaml_compiler.native_compile_ocaml_implem ~tag:"profile" ~cmx_ext:"p.cmx" "%.ml");;

rule "ocaml: ml & cmi -> cmx & o"
  ~tags:["ocaml"; "native"]
  ~prods:["%.cmx"; x_o]
  ~deps:["%.ml"; "%.ml.depends"; "%.cmi"]
  (Ocaml_compiler.native_compile_ocaml_implem "%.ml");;

rule "ocaml: ml -> d.cmo & cmi"
  ~tags:["ocaml"; "debug"]
  ~prods:["%.d.cmo"]
  ~deps:["%.ml"; "%.ml.depends"; "%.cmi"]
  (Ocaml_compiler.byte_compile_ocaml_implem ~tag:"debug" "%.ml" "%.d.cmo");;

rule "ocaml: ml -> cmo & cmi"
  ~tags:["ocaml"]
  ~prods:["%.cmo"; "%.cmi"]
  ~deps:["%.ml"; "%.ml.depends"]
  (Ocaml_compiler.byte_compile_ocaml_implem "%.ml" "%.cmo");;

rule "ocaml: d.cmo* -> d.byte"
  ~tags:["ocaml"; "byte"; "debug"; "program"]
  ~prod:"%.d.byte"
  ~dep:"%.d.cmo"
  (Ocaml_compiler.byte_debug_link "%.d.cmo" "%.d.byte");;

rule "ocaml: cmo* -> byte"
  ~tags:["ocaml"; "byte"; "program"]
  ~prod:"%.byte"
  ~dep:"%.cmo"
  (Ocaml_compiler.byte_link "%.cmo" "%.byte");;

rule "ocaml: p.cmx* & p.o* -> p.native"
  ~tags:["ocaml"; "native"; "profile"; "program"]
  ~prod:"%.p.native"
  ~deps:["%.p.cmx"; x_p_o]
  (Ocaml_compiler.native_profile_link "%.p.cmx" "%.p.native");;

rule "ocaml: cmx* & o* -> native"
  ~tags:["ocaml"; "native"; "program"]
  ~prod:"%.native"
  ~deps:["%.cmx"; x_o]
  (Ocaml_compiler.native_link "%.cmx" "%.native");;

rule "ocaml: mllib & d.cmo* -> d.cma"
  ~tags:["ocaml"; "byte"; "debug"; "library"]
  ~prod:"%.d.cma"
  ~dep:"%.mllib"
  (Ocaml_compiler.byte_debug_library_link_mllib "%.mllib" "%.d.cma");;

rule "ocaml: mllib & cmo* -> cma"
  ~tags:["ocaml"; "byte"; "library"]
  ~prod:"%.cma"
  ~dep:"%.mllib"
  (Ocaml_compiler.byte_library_link_mllib "%.mllib" "%.cma");;

rule "ocaml: d.cmo* -> d.cma"
  ~tags:["ocaml"; "byte"; "debug"; "library"]
  ~prod:"%.d.cma"
  ~dep:"%.d.cmo"
  (Ocaml_compiler.byte_debug_library_link "%.d.cmo" "%.d.cma");;

rule "ocaml: cmo* -> cma"
  ~tags:["ocaml"; "byte"; "library"]
  ~prod:"%.cma"
  ~dep:"%.cmo"
  (Ocaml_compiler.byte_library_link "%.cmo" "%.cma");;

rule "ocaml C stubs: clib & (o|obj)* -> (a|lib) & (so|dll)"
  ~prods:["%(path:<**/>)lib%(libname:<*> and not <*.*>)"-.-ext_lib;
          "%(path:<**/>)dll%(libname:<*> and not <*.*>)"-.-ext_dll]
  ~dep:"%(path)lib%(libname).clib"
  (C_tools.link_C_library "%(path)lib%(libname).clib" ("%(path)lib%(libname)"-.-ext_lib) "%(path)%(libname)");;

rule "ocaml: mllib & p.cmx* & p.o* -> p.cmxa & p.a"
  ~tags:["ocaml"; "native"; "profile"; "library"]
  ~prods:["%.p.cmxa"; x_p_a]
  ~dep:"%.mllib"
  (Ocaml_compiler.native_profile_library_link_mllib "%.mllib" "%.p.cmxa");;

rule "ocaml: mllib & cmx* & o* -> cmxa & a"
  ~tags:["ocaml"; "native"; "library"]
  ~prods:["%.cmxa"; x_a]
  ~dep:"%.mllib"
  (Ocaml_compiler.native_library_link_mllib "%.mllib" "%.cmxa");;

rule "ocaml: p.cmx* & p.o* -> p.cmxa & p.a"
  ~tags:["ocaml"; "native"; "profile"; "library"]
  ~prods:["%.p.cmxa"; x_p_a]
  ~deps:["%.p.cmx"; x_p_o]
  (Ocaml_compiler.native_profile_library_link "%.p.cmx" "%.p.cmxa");;

rule "ocaml: cmx* & o* -> cmxa & a"
  ~tags:["ocaml"; "native"; "library"]
  ~prods:["%.cmxa"; x_a]
  ~deps:["%.cmx"; x_o]
  (Ocaml_compiler.native_library_link "%.cmx" "%.cmxa");;

rule "ocaml dependencies ml"
  ~prod:"%.ml.depends"
  ~dep:"%.ml"
  (Ocaml_tools.ocamldep_command "%.ml" "%.ml.depends");;

rule "ocaml dependencies mli"
  ~prod:"%.mli.depends"
  ~dep:"%.mli"
  (Ocaml_tools.ocamldep_command "%.mli" "%.mli.depends");;

rule "ocamllex"
  ~tags:["ocaml"] (* FIXME "lexer" *)
  ~prod:"%.ml"
  ~dep:"%.mll"
  (Ocaml_tools.ocamllex "%.mll");;

rule "ocaml: mli -> odoc"
  ~tags:["ocaml"; "doc"]
  ~prod:"%.odoc"
  ~deps:["%.mli"; "%.mli.depends"]
  (Ocaml_tools.document_ocaml_interf "%.mli" "%.odoc");;

rule "ocaml: ml -> odoc"
  ~tags:["ocaml"; "doc"]
  ~prod:"%.odoc"
  ~deps:["%.ml"; "%.ml.depends"]
  (Ocaml_tools.document_ocaml_implem "%.ml" "%.odoc");;

rule "ocamldoc: document ocaml project odocl & *odoc -> docdir (html)"
  ~prod:"%.docdir/index.html"
  ~stamp:"%.docdir/html.stamp" (* Depend on this file if you want to depends on all files of %.docdir *)
  ~dep:"%.odocl"
  (Ocaml_tools.document_ocaml_project
      ~ocamldoc:Ocaml_tools.ocamldoc_l_dir "%.odocl" "%.docdir/index.html" "%.docdir");;

rule "ocamldoc: document ocaml project odocl & *odoc -> docdir (man)"
  ~prod:"%.docdir/man"
  ~stamp:"%.docdir/man.stamp" (* Depend on this file if you want to depends on all files of %.docdir/man *)
  ~dep:"%.odocl"
  (Ocaml_tools.document_ocaml_project
      ~ocamldoc:Ocaml_tools.ocamldoc_l_dir "%.odocl" "%.docdir/man" "%.docdir");;

rule "ocamldoc: document ocaml project odocl & *odoc -> man|latex|dot..."
  ~prod:"%(dir).docdir/%(file)"
  ~dep:"%(dir).odocl"
  (Ocaml_tools.document_ocaml_project
      ~ocamldoc:Ocaml_tools.ocamldoc_l_file "%(dir).odocl" "%(dir).docdir/%(file)" "%(dir).docdir");;

(* To use menhir give the -use-menhir option at command line,
   Or put true: use_menhir in your tag file. *)
if !Options.use_menhir || Configuration.has_tag "use_menhir" then begin
  (* Automatic handling of menhir modules, given a
     description file %.mlypack                         *)
  rule "ocaml: modular menhir (mlypack)"
    ~prods:["%.mli" ; "%.ml"]
    ~deps:["%.mlypack"]
    (Ocaml_tools.menhir_modular "%" "%.mlypack" "%.mlypack.depends");

  rule "ocaml: menhir modular dependencies"
    ~prod:"%.mlypack.depends"
    ~dep:"%.mlypack"
    (Ocaml_tools.menhir_modular_ocamldep_command "%.mlypack" "%.mlypack.depends");

  rule "ocaml: menhir"
    ~prods:["%.ml"; "%.mli"]
    ~deps:["%.mly"; "%.mly.depends"]
    (Ocaml_tools.menhir "%.mly");

  rule "ocaml: menhir dependencies"
    ~prod:"%.mly.depends"
    ~dep:"%.mly"
    (Ocaml_tools.menhir_ocamldep_command "%.mly" "%.mly.depends");

end else
  rule "ocamlyacc"
    ~tags:["ocaml"] (* FIXME "parser" *)
    ~prods:["%.ml"; "%.mli"]
    ~dep:"%.mly"
    (Ocaml_tools.ocamlyacc "%.mly");;

rule "ocaml C stubs: c -> o"
  ~prod:x_o
  ~dep:"%.c"
  begin fun env _build ->
    let c = env "%.c" in
    let o = env x_o in
    let comp = if Tags.mem "native" (tags_of_pathname c) then !Options.ocamlopt else !Options.ocamlc in
    let cc = Cmd(S[comp; T(tags_of_pathname c++"c"++"compile"); A"-c"; Px c]) in
    if Pathname.dirname o = Pathname.current_dir_name then cc
    else Seq[cc; mv (Pathname.basename o) o]
  end;;

rule "ocaml: ml & ml.depends & *cmi -> .inferred.mli"
  ~prod:"%.inferred.mli"
  ~deps:["%.ml"; "%.ml.depends"]
  (Ocaml_tools.infer_interface "%.ml" "%.inferred.mli");;

rule "ocaml: mltop -> top"
  ~prod:"%.top"
  ~dep:"%.mltop"
  (Ocaml_compiler.byte_toplevel_link_mltop "%.mltop" "%.top");;

rule "preprocess: ml -> pp.ml"
  ~dep:"%.ml"
  ~prod:"%.pp.ml"
  (Ocaml_tools.camlp4 "pp.ml" "%.ml" "%.pp.ml");;

flag ["ocaml"; "pp"] begin
  S (List.fold_right (fun x acc -> Sh x :: acc) !Options.ocaml_ppflags [])
end;;

flag ["ocaml"; "compile"] begin
  atomize !Options.ocaml_cflags
end;;

flag ["ocaml"; "link"] begin
  atomize !Options.ocaml_lflags
end;;

flag ["ocaml"; "ocamlyacc"] (atomize !Options.ocaml_yaccflags);;
flag ["ocaml"; "menhir"] (atomize !Options.ocaml_yaccflags);;

(* Tell menhir to explain conflicts *)
flag [ "ocaml" ; "menhir" ; "explain" ] (S[A "--explain"]);;

flag ["ocaml"; "ocamllex"] (atomize !Options.ocaml_lexflags);;

(* Tell ocamllex to generate ml code *)
flag [ "ocaml" ; "ocamllex" ; "generate_ml" ] (S[A "-ml"]);;

flag ["ocaml"; "byte"; "link"] begin
  S (List.map (fun x -> A (x^".cma")) !Options.ocaml_libs)
end;;

flag ["ocaml"; "native"; "link"] begin
  S (List.map (fun x -> A (x^".cmxa")) !Options.ocaml_libs)
end;;

let camlp4_flags camlp4s =
  List.iter begin fun camlp4 ->
    flag ["ocaml"; "pp"; camlp4] (A camlp4)
  end camlp4s;;

camlp4_flags ["camlp4o"; "camlp4r"; "camlp4of"; "camlp4rf"; "camlp4orf"; "camlp4oof"];;

let camlp4_flags' camlp4s =
  List.iter begin fun (camlp4, flags) ->
    flag ["ocaml"; "pp"; camlp4] flags
  end camlp4s;;

camlp4_flags' ["camlp4orr", S[A"camlp4of"; A"-parser"; A"reloaded"];
               "camlp4rrr", S[A"camlp4rf"; A"-parser"; A"reloaded"]];;

flag ["ocaml"; "pp"; "camlp4:no_quot"] (A"-no_quot");;

ocaml_lib ~extern:true "dynlink";;
ocaml_lib ~extern:true "unix";;
ocaml_lib ~extern:true "str";;
ocaml_lib ~extern:true "bigarray";;
ocaml_lib ~extern:true "nums";;
ocaml_lib ~extern:true "dbm";;
ocaml_lib ~extern:true "graphics";;
ocaml_lib ~extern:true ~tag_name:"use_toplevel" "toplevellib";;
ocaml_lib ~extern:true ~dir:"+labltk" "labltk";;
ocaml_lib ~extern:true ~dir:"+ocamldoc" "ocamldoc";;
ocaml_lib ~extern:true ~dir:"+ocamlbuild" ~tag_name:"use_ocamlbuild" "ocamlbuildlib";;

ocaml_lib ~extern:true ~dir:"+camlp4" ~tag_name:"use_camlp4" "camlp4lib";;
ocaml_lib ~extern:true ~dir:"+camlp4" ~tag_name:"use_old_camlp4" "camlp4";;
ocaml_lib ~extern:true ~dir:"+camlp4" ~tag_name:"use_camlp4_full" "camlp4fulllib";;
flag ["ocaml"; "compile"; "use_camlp4_full"]
     (S[A"-I"; A"+camlp4/Camlp4Parsers";
        A"-I"; A"+camlp4/Camlp4Printers";
        A"-I"; A"+camlp4/Camlp4Filters"]);;
flag ["ocaml"; "use_camlp4_bin"; "link"; "byte"] (A"+camlp4/Camlp4Bin.cmo");;
flag ["ocaml"; "use_camlp4_bin"; "link"; "native"] (A"+camlp4/Camlp4Bin.cmx");;

flag ["ocaml"; "debug"; "compile"; "byte"] (A "-g");;
flag ["ocaml"; "debug"; "link"; "byte"; "program"] (A "-g");;
flag ["ocaml"; "debug"; "pack"; "byte"] (A "-g");;
flag ["ocaml"; "debug"; "compile"; "native"] (A "-g");;
flag ["ocaml"; "debug"; "link"; "native"; "program"] (A "-g");;
flag ["ocaml"; "debug"; "pack"; "native"] (A "-g");;
flag ["ocaml"; "link"; "native"; "output_obj"] (A"-output-obj");;
flag ["ocaml"; "link"; "byte"; "output_obj"] (A"-output-obj");;
flag ["ocaml"; "dtypes"; "compile"] (A "-dtypes");;
flag ["ocaml"; "annot"; "compile"] (A "-annot");;
flag ["ocaml"; "rectypes"; "compile"] (A "-rectypes");;
flag ["ocaml"; "rectypes"; "infer_interface"] (A "-rectypes");;
flag ["ocaml"; "linkall"; "link"] (A "-linkall");;
flag ["ocaml"; "link"; "profile"; "native"] (A "-p");;
flag ["ocaml"; "link"; "program"; "custom"; "byte"] (A "-custom");;
flag ["ocaml"; "link"; "library"; "custom"; "byte"] (A "-custom");;
flag ["ocaml"; "compile"; "profile"; "native"] (A "-p");;
flag ["ocaml"; "compile"; "thread"] (A "-thread");;
flag ["ocaml"; "doc"; "thread"] (S[A"-I"; A"+threads"]);;
flag ["ocaml"; "link"; "thread"; "native"; "program"] (S[A "threads.cmxa"; A "-thread"]);;
flag ["ocaml"; "link"; "thread"; "byte"; "program"] (S[A "threads.cma"; A "-thread"]);;
flag ["ocaml"; "link"; "thread"; "native"; "toplevel"] (S[A "threads.cmxa"; A "-thread"]);;
flag ["ocaml"; "link"; "thread"; "byte"; "toplevel"] (S[A "threads.cma"; A "-thread"]);;
flag ["ocaml"; "compile"; "nopervasives"] (A"-nopervasives");;
flag ["ocaml"; "compile"; "nolabels"] (A"-nolabels");;

(*flag ["ocaml"; "ocamlyacc"; "quiet"] (A"-q");;*)
flag ["ocaml"; "ocamllex"; "quiet"] (A"-q");;

let ocaml_warn_flag c =
  flag ["ocaml"; "compile"; sprintf "warn_%c" (Char.uppercase c)]
       (S[A"-w"; A (sprintf "%c" (Char.uppercase c))]);
  flag ["ocaml"; "compile"; sprintf "warn_error_%c" (Char.uppercase c)]
       (S[A"-warn-error"; A (sprintf "%c" (Char.uppercase c))]);
  flag ["ocaml"; "compile"; sprintf "warn_%c" (Char.lowercase c)]
       (S[A"-w"; A (sprintf "%c" (Char.lowercase c))]);
  flag ["ocaml"; "compile"; sprintf "warn_error_%c" (Char.lowercase c)]
       (S[A"-warn-error"; A (sprintf "%c" (Char.lowercase c))]);;

List.iter ocaml_warn_flag ['A'; 'C'; 'D'; 'E'; 'F'; 'L'; 'M'; 'P'; 'S'; 'U'; 'V'; 'Y'; 'Z'; 'X'];;

flag ["ocaml"; "doc"; "docdir"; "extension:html"] (A"-html");;
flag ["ocaml"; "doc"; "docdir"; "manpage"] (A"-man");;
flag ["ocaml"; "doc"; "docfile"; "extension:dot"] (A"-dot");;
flag ["ocaml"; "doc"; "docfile"; "extension:tex"] (A"-latex");;
flag ["ocaml"; "doc"; "docfile"; "extension:ltx"] (A"-latex");;
flag ["ocaml"; "doc"; "docfile"; "extension:texi"] (A"-texi");;

ocaml_lib "ocamlbuildlib";;
ocaml_lib "ocamlbuildlightlib";;

end in ()
