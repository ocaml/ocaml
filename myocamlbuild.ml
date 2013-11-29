(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ocamlbuild_plugin
open Command
open Arch
open Format

module C = Myocamlbuild_config

let () = mark_tag_used "windows";;
let windows = Sys.os_type = "Win32";;
if windows then tag_any ["windows"];;
let ccomptype = C.ccomptype
(*let () = if ccomptype <> "cc" then eprintf "ccomptype: %s@." ccomptype;;*)

let fp_cat oc f = with_input_file ~bin:true f (fun ic -> copy_chan ic oc)

(* Improve using the command module in Myocamlbuild_config
   with the variant version (`S, `A...) *)
let mkdll out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(Printf.sprintf "%s -o %s %s %s" C.mkdll out (s files) (s opts)))

let mkexe out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(Printf.sprintf "%s -o %s %s %s" C.mkexe out (s files) (s opts)))

let mklib out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(C.mklib out (s files) (s opts)))

let syslib x = A(C.syslib x);;
let syscamllib x =
  if ccomptype = "msvc" then A(Printf.sprintf "lib%s.lib" x)
  else A("-l"^x)

let ccoutput cc obj file =
  if ccomptype = "msvc" then
    Seq[Cmd(S[cc; A"-c"; Px file]);
        mv (Pathname.basename (Pathname.update_extension C.o file)) obj]
  else
    Cmd(S[cc; A"-c"; P file; A"-o"; Px obj])

let mkobj obj file opts =
  let tags = tags_of_pathname file++"c"++"compile"++ccomptype in
  let bytecc_with_opts = S[Sh C.bytecc; Sh C.bytecccompopts; opts; T tags] in
  ccoutput bytecc_with_opts obj file

let mknatobj obj file opts =
  let nativecc_with_opts = S[Sh C.nativecc; opts; Sh C.nativecccompopts] in
  ccoutput nativecc_with_opts obj file

let add_exe a =
  if not windows || Pathname.check_extension a "exe" then a
  else a-.-"exe";;

let add_exe_if_exists a =
  if not windows || Pathname.check_extension a "exe" then a
  else
    let exe = a-.-"exe" in
    if Pathname.exists exe then exe else a;;

let convert_command_for_windows_shell spec =
  if not windows then spec else
  let rec self specs acc =
    match specs with
    | N :: specs -> self specs acc
    | S[] :: specs -> self specs acc
    | S[x] :: specs -> self (x :: specs) acc
    | S specs :: specs' -> self (specs @ specs') acc
    | (P(a) | A(a)) :: specs ->
        let dirname = Pathname.dirname a in
        let basename = Pathname.basename a in
        let p =
          if dirname = Pathname.current_dir_name then Sh(add_exe_if_exists basename)
          else Sh(add_exe_if_exists (dirname ^ "\\" ^ basename)) in
        if String.contains_string basename 0 "ocamlrun" = None then
          List.rev (p :: acc) @ specs
        else
          self specs (p :: acc)
    | [] | (Px _ | T _ | V _ | Sh _ | Quote _) :: _ ->
        invalid_arg "convert_command_for_windows_shell: invalid atom in head position"
  in S(self [spec] [])

let convert_for_windows_shell solver () =
  convert_command_for_windows_shell (solver ())

let ocamlrun = A"boot/ocamlrun"
let full_ocamlrun = P((Sys.getcwd ()) / "boot/ocamlrun")

let boot_ocamlc = S[ocamlrun; A"boot/ocamlc"; A"-I"; A"boot"; A"-nostdlib"]

let mixed = Pathname.exists "build/ocamlbuild_mixed_mode";;

let if_mixed_dir dir =
  if mixed then ".."/dir else dir;;

let unix_dir =
  if Sys.os_type = "Win32" || C.system = "mingw" then
    if_mixed_dir "otherlibs/win32unix"
  else
    if_mixed_dir "otherlibs/unix";;

let threads_dir    = if_mixed_dir "otherlibs/threads";;
let systhreads_dir = if_mixed_dir "otherlibs/systhreads";;
let dynlink_dir    = if_mixed_dir "otherlibs/dynlink";;
let str_dir        = if_mixed_dir "otherlibs/str";;
let toplevel_dir   = if_mixed_dir "toplevel";;

let systhreads_file f = "otherlibs/systhreads"/f
let systhreads_obj f = "otherlibs/systhreads"/f-.-C.o
let systhreads_lib f = "otherlibs/systhreads"/f-.-C.a
let systhreads_dll f = "otherlibs/systhreads"/f-.-C.so

let ocamlc_solver =
  let native_deps = ["ocamlc.opt"; "stdlib/stdlib.cmxa";
                    "stdlib/std_exit.cmx"; "stdlib/std_exit"-.-C.o] in
  let byte_deps = ["ocamlc"; "stdlib/stdlib.cma"; "stdlib/std_exit.cmo"] in
  fun () ->
    if Pathname.exists "../ocamlcomp.sh" then S[A"../ocamlcomp.sh"] else
    if List.for_all Pathname.exists native_deps then
      S[A"./ocamlc.opt"; A"-nostdlib"]
    else if List.for_all Pathname.exists byte_deps then
      S[ocamlrun; A"./ocamlc"; A"-nostdlib"]
    else boot_ocamlc;;

Command.setup_virtual_command_solver "OCAMLC" ocamlc_solver;;
Command.setup_virtual_command_solver "OCAMLCWIN" (convert_for_windows_shell ocamlc_solver);;

let ocamlopt_solver () =
  S[if Pathname.exists "../ocamlcompopt.sh" then S[A"../ocamlcompopt.sh"] else
    if Pathname.exists "ocamlopt.opt" && Pathname.exists ("stdlib/stdlib.cmxa")
    then A"./ocamlopt.opt"
    else S[ocamlrun; A"./ocamlopt"];
    A"-nostdlib"];;

Command.setup_virtual_command_solver "OCAMLOPT" ocamlopt_solver;;
Command.setup_virtual_command_solver "OCAMLOPTWIN" (convert_for_windows_shell ocamlopt_solver);;

let ocamlc   = V"OCAMLC";;
let ocamlopt = V"OCAMLOPT";;

let ar = A"ar";;

dispatch begin function
| Before_hygiene ->
    if mixed then
      let patt = String.concat ","
        ["asmcomp"; "bytecomp"; "debugger"; "driver";
         "lex"; "ocamldoc"; "otherlibs"; "parsing"; "stdlib"; "tools";
         "toplevel"; "typing"; "utils"]
      in Ocamlbuild_pack.Configuration.parse_string
           (sprintf "<{%s}/**>: not_hygienic, -traverse" patt)

| After_options ->
    begin
      Options.ocamlrun := ocamlrun;
      Options.ocamllex := S[ocamlrun; P"boot/ocamllex"];
      Options.ocamlyacc := if windows then P"./boot/ocamlyacc.exe" else P"boot/ocamlyacc";
      Options.ocamlmklib := S[ocamlrun; P"tools/ocamlmklib.byte"; A"-ocamlc"; Quote (V"OCAMLCWIN");
                              A"-ocamlopt"; Quote (V"OCAMLOPTWIN")(* ; A"-v" *)];
      Options.ocamldep := S[ocamlrun; P"boot/ocamldep"];

      Options.ext_obj := C.o;
      Options.ext_lib := C.a;
      Options.ext_dll := String.after C.ext_dll 1;

      Options.nostdlib := true;
      Options.make_links := false;
      if !Options.just_plugin then
        Options.ocamlc := boot_ocamlc
      else begin
        Options.ocamlc := ocamlc;
        Options.plugin := false;
        Options.ocamlopt := ocamlopt;
      end;
    end
| After_rules ->
    let module M = struct


flag ["ocaml"; "ocamlyacc"] (A"-v");;

flag ["ocaml"; "compile"; "strict_sequence"] (A"-strict-sequence");;

non_dependency "otherlibs/threads/pervasives.ml" "Unix";;
non_dependency "otherlibs/threads/pervasives.ml" "String";;

let add_extensions extensions modules =
  List.fold_right begin fun x ->
    List.fold_right begin fun ext acc ->
      x-.-ext :: acc
    end extensions
  end modules [];;


use_lib "toplevel/topstart" "toplevel/toplevellib";;
use_lib "otherlibs/dynlink/extract_crc" "otherlibs/dynlink/dynlink";;

hide_package_contents "otherlibs/dynlink/dynlinkaux";;

flag ["ocaml"; "link"; "file:driver/main.native"; "native"] begin
  S[A"-ccopt"; A C.bytecclinkopts; A"-cclib"; A C.bytecclibs]
end;;

dep ["ocaml"; "link"; "file:driver/main.native"; "native"]
    ["asmrun/meta"-.-C.o; "asmrun/dynlink"-.-C.o];;

dep ["ocaml"; "compile"; "native"] ["stdlib/libasmrun"-.-C.a];;

flag ["ocaml"; "link"] (S[A"-I"; P "stdlib"]);;
flag ["ocaml"; "compile"; "include_unix"] (S[A"-I"; P unix_dir]);;
flag ["ocaml"; "compile"; "include_str"] (S[A"-I"; P str_dir]);;
flag ["ocaml"; "compile"; "include_dynlink"] (S[A"-I"; P dynlink_dir]);;
flag ["ocaml"; "compile"; "include_toplevel"] (S[A"-I"; P toplevel_dir]);;
flag ["ocaml"; "link"; "use_unix"] (S[A"-I"; P unix_dir]);;
flag ["ocaml"; "link"; "use_dynlink"] (S[A"-I"; P dynlink_dir]);;
flag ["ocaml"; "link"; "use_str"] (S[A"-I"; P str_dir]);;
flag ["ocaml"; "link"; "use_toplevel"] (S[A"-I"; P toplevel_dir]);;

let setup_arch arch =
  let annotated_arch = annotate arch in
  let (_include_dirs_table, _for_pack_table) = mk_tables annotated_arch in
  (* Format.eprintf "%a@." (Ocaml_arch.print_table (List.print pp_print_string)) include_dirs_table;; *)
  iter_info begin fun i ->
    Pathname.define_context i.current_path i.include_dirs
  end annotated_arch;;


Pathname.define_context "" ["stdlib"];;
Pathname.define_context "utils" [Pathname.current_dir_name; "stdlib"];;
Pathname.define_context "parsing" ["parsing"; "utils"; "stdlib"];;
Pathname.define_context "typing" ["typing"; "parsing"; "utils"; "stdlib"];;
Pathname.define_context "ocamldoc" ["typing"; "parsing"; "utils"; "tools"; "bytecomp"; "stdlib"];;
Pathname.define_context "bytecomp" ["bytecomp"; "parsing"; "typing"; "utils"; "stdlib"];;
Pathname.define_context "tools" ["tools"; (* "toplevel"; *) "parsing"; "utils"; "driver"; "bytecomp"; "asmcomp"; "typing"; "stdlib"];;
Pathname.define_context "toplevel" ["toplevel"; "parsing"; "typing"; "bytecomp"; "utils"; "driver"; "stdlib"];;
Pathname.define_context "driver" ["driver"; "asmcomp"; "bytecomp"; "typing"; "utils"; "parsing"; "stdlib"];;
Pathname.define_context "debugger" ["bytecomp"; "utils"; "typing"; "parsing"; "toplevel"; "stdlib"];;
Pathname.define_context "otherlibs/dynlink" ["otherlibs/dynlink"; "bytecomp"; "utils"; "typing"; "parsing"; "stdlib"];;
Pathname.define_context "otherlibs/dynlink/nat" ["otherlibs/dynlink/nat"; "asmcomp"; "stdlib"];;
Pathname.define_context "asmcomp" ["asmcomp"; "bytecomp"; "parsing"; "typing"; "utils"; "stdlib"];;
Pathname.define_context "ocamlbuild" ["ocamlbuild"; "."];;
Pathname.define_context "lex" ["lex"; "stdlib"];;

List.iter (fun x -> let x = "otherlibs"/x in Pathname.define_context x [x; "stdlib"])
  ["bigarray"; "graph"; "num"; "str"; "systhreads"; "unix"; "win32graph"; "win32unix"];;

(* The bootstrap standard library *)
copy_rule "The bootstrap standard library" "stdlib/%" "boot/%";;

(* About the standard library *)
copy_rule "stdlib asmrun"  ("asmrun/%"-.-C.a)  ("stdlib/%"-.-C.a);;
copy_rule "stdlib byterun" ("byterun/%"-.-C.a) ("stdlib/%"-.-C.a);;

(* The thread specific standard library *)
copy_rule "The thread specific standard library (mllib)" ~insert:`bottom "stdlib/%.mllib" "otherlibs/threads/%.mllib";;
copy_rule "The thread specific standard library (cmo)"   ~insert:`bottom "stdlib/%.cmo" "otherlibs/threads/%.cmo";;
copy_rule "The thread specific standard library (cmi)"   ~insert:`top    "stdlib/%.cmi" "otherlibs/threads/%.cmi";;
copy_rule "The thread specific standard library (mli)"   ~insert:`bottom "stdlib/%.mli" "otherlibs/threads/%.mli";;
copy_rule "The thread specific unix library (mli)"       ~insert:`bottom "otherlibs/unix/%.mli" "otherlibs/threads/%.mli";;
copy_rule "The thread specific unix library (ml)"        ~insert:`bottom "otherlibs/unix/%.ml" "otherlibs/threads/%.ml";;
copy_rule "The thread specific unix library (mllib)"     ~insert:`bottom "otherlibs/unix/%.mllib" "otherlibs/threads/%.mllib";;

(* Temporary rule, waiting for a full usage of ocamlbuild *)
copy_rule "Temporary rule, waiting for a full usage of ocamlbuild" "%.mlbuild" "%.ml";;

copy_rule "graph/graphics.ml -> win32graph/graphics.ml" "otherlibs/graph/graphics.ml" "otherlibs/win32graph/graphics.ml";;
copy_rule "graph/graphics.mli -> win32graph/graphics.mli" "otherlibs/graph/graphics.mli" "otherlibs/win32graph/graphics.mli";;

rule "the ocaml toplevel"
  ~prod:"ocaml"
  ~deps:["stdlib/stdlib.mllib"; "toplevel/topstart.byte"; "toplevel/expunge.byte"]
  begin fun _ _ ->
    let modules = string_list_of_file "stdlib/stdlib.mllib" in
    Cmd(S[ocamlrun; A"toplevel/expunge.byte"; A"toplevel/topstart.byte"; Px"ocaml";
          A"outcometree"; A"topdirs"; A"toploop"; atomize modules])
  end;;

let copy_rule' ?insert src dst = copy_rule (sprintf "%s -> %s" src dst) ?insert src dst;;

copy_rule' "driver/main.byte" "ocamlc";;
copy_rule' "driver/main.native" "ocamlc.opt";;
copy_rule' "driver/optmain.byte" "ocamlopt";;
copy_rule' "driver/optmain.native" "ocamlopt.opt";;
copy_rule' "lex/main.byte" "lex/ocamllex";;
copy_rule' "lex/main.native" "lex/ocamllex.opt";;
copy_rule' "debugger/main.byte" "debugger/ocamldebug";;
copy_rule' "ocamldoc/odoc.byte" "ocamldoc/ocamldoc";;
copy_rule' "ocamldoc/odoc.native" "ocamldoc/ocamldoc.opt";;
copy_rule' "tools/ocamlmklib.byte" "tools/ocamlmklib";;
copy_rule' "otherlibs/dynlink/extract_crc.byte" "otherlibs/dynlink/extract_crc";;
copy_rule' "myocamlbuild_config.mli" "ocamlbuild/ocamlbuild_Myocamlbuild_config.mli";;
copy_rule' "myocamlbuild_config.ml" "ocamlbuild/ocamlbuild_Myocamlbuild_config.ml";;

copy_rule' ~insert:`bottom "%" "%.exe";;

ocaml_lib "stdlib/stdlib";;

let stdlib_mllib_contents =
  lazy (string_list_of_file "stdlib/stdlib.mllib");;

let import_stdlib_contents build exts =
  let l =
    List.fold_right begin fun x ->
      List.fold_right begin fun ext acc ->
        ["stdlib"/(String.uncapitalize x)-.-ext] :: acc
      end exts
    end !*stdlib_mllib_contents []
  in
  let res = build l in
  List.iter Outcome.ignore_good res
;;

rule "byte stdlib in mixed mode"
  ~stamp:"byte_stdlib_mixed_mode"
  ~deps:["stdlib/stdlib.mllib"; "stdlib/stdlib.cma";
         "stdlib/std_exit.cmo"; "stdlib/libcamlrun"-.-C.a;
         "stdlib/camlheader"; "stdlib/camlheader_ur"]
  begin fun env build ->
    let (_ : Command.t) =
      Ocamlbuild_pack.Ocaml_compiler.byte_library_link_mllib
        "stdlib/stdlib.mllib" "stdlib/stdlib.cma" env build
    in
    import_stdlib_contents build ["cmi"];
    Nop
  end;;

rule "native stdlib in mixed mode"
  ~stamp:"native_stdlib_mixed_mode"
  ~deps:["stdlib/stdlib.mllib"; "stdlib/stdlib.cmxa";
         "stdlib/stdlib"-.-C.a; "stdlib/std_exit.cmx";
         "stdlib/std_exit"-.-C.o; "stdlib/libasmrun"-.-C.a;
         "stdlib/camlheader"; "stdlib/camlheader_ur"]
  begin fun env build ->
    let (_ : Command.t) =
      Ocamlbuild_pack.Ocaml_compiler.native_library_link_mllib
        "stdlib/stdlib.mllib" "stdlib/stdlib.cmxa" env build
    in
    import_stdlib_contents build ["cmi"];
    Nop
  end;;

copy_rule' ~insert:`top "otherlibs/dynlink/natdynlink.ml" "otherlibs/dynlink/nat/dynlink.ml";;
copy_rule' ~insert:`top "otherlibs/dynlink/dynlink.mli" "otherlibs/dynlink/nat/dynlink.mli";;
copy_rule' ~insert:`top "otherlibs/dynlink/nat/dynlink.cmx" "otherlibs/dynlink/dynlink.cmx";;
copy_rule' ~insert:`top ("otherlibs/dynlink/nat/dynlink"-.-C.o) ("otherlibs/dynlink/dynlink"-.-C.o);;
copy_rule' ~insert:`top "otherlibs/dynlink/nat/dynlink.cmxa" "otherlibs/dynlink/dynlink.cmxa";;
copy_rule' ~insert:`top ("otherlibs/dynlink/nat/dynlink"-.-C.a) ("otherlibs/dynlink/dynlink"-.-C.a);;
dep ["ocaml"; "compile"; "native"; "file:otherlibs/dynlink/nat/dynlink.cmx"] ["otherlibs/dynlink/nat/dynlink.cmi"];;

rule "C files"
  ~prod:("%"-.-C.o)
  ~dep:"%.c"
  ~insert:(`before "ocaml C stubs: c -> o")
  begin fun env _ ->
    mkobj (env ("%"-.-C.o)) (env "%.c") N
  end;;

let () =
  (* define flags otherlibs_unix, otherlibs_bigarray... *)
  let otherlibs = "otherlibs" in
  let open Pathname in
  Array.iter (fun file ->
    if is_directory (concat "otherlibs" file) then
      mark_tag_used ("otherlibs_" ^ file)
  ) (readdir otherlibs);;

(* ../ is because .h files are not dependencies so they are not imported in build dir *)
flag ["c"; "compile"; "otherlibs_bigarray"] (S[A"-I"; P"../otherlibs/bigarray"]);;
flag [(* "ocaml" or "c"; *) "ocamlmklib"; "otherlibs_graph"] (S[Sh C.x11_link]);;
flag ["c"; "compile"; "otherlibs_graph"] (S[Sh C.x11_includes; A"-I../otherlibs/graph"]);;
flag ["c"; "compile"; "otherlibs_win32graph"] (A"-I../otherlibs/win32graph");;
flag ["ocaml"; "ocamlmklib"; "otherlibs_threads"] (S[A"-oc"; A"otherlibs/threads/vmthreads"]);;
flag ["c"; "compile"; "otherlibs_num"] begin
  S[A("-DBNG_ARCH_"^C.bng_arch);
    A("-DBNG_ASM_LEVEL="^C.bng_asm_level);
    A"-I"; P"../otherlibs/num"]
end;;
flag ["c"; "compile"; "otherlibs_win32unix"] (A"-I../otherlibs/win32unix");;
flag [(* "ocaml" or "c"; *) "ocamlmklib"; "otherlibs_win32unix"] (S[A"-cclib"; Quote (syslib "ws2_32")]);;
flag ["c"; "link"; "dll"; "otherlibs_win32unix"] (syslib "ws2_32");;
let flags = S[syslib "kernel32"; syslib "gdi32"; syslib "user32"] in
flag ["c"; "ocamlmklib"; "otherlibs_win32graph"] (S[A"-cclib"; Quote flags]);
flag ["c"; "link"; "dll"; "otherlibs_win32graph"] flags;;

if windows then flag ["c"; "compile"; "otherlibs_bigarray"] (A"-DIN_OCAML_BIGARRAY");;

if windows then flag ["ocamlmklib"] (A"-custom");;

flag ["ocaml"; "pp"; "ocamldoc_sources"] begin
  if windows then
    S[A"grep"; A"-v"; A"DEBUG"]
  else
    A"../ocamldoc/remove_DEBUG"
end;;

let ocamldoc = P"./ocamldoc/ocamldoc.opt" in
let stdlib_mlis =
  List.fold_right
    (fun x acc -> "stdlib"/(String.uncapitalize x)-.-"mli" :: acc)
    (string_list_of_file "stdlib/stdlib.mllib")
    ["otherlibs/unix/unix.mli"; "otherlibs/str/str.mli";
     "otherlibs/bigarray/bigarray.mli"; "otherlibs/num/num.mli"] in
rule "Standard library manual"
  ~prod:"ocamldoc/stdlib_man/Pervasives.3o"
  ~stamp:"ocamldoc/stdlib_man.stamp" (* Depend on this file if you want to depends on all files of stdlib_man/* *)
  ~deps:stdlib_mlis
  begin fun _ _ ->
    Seq[Cmd(S[A"mkdir"; A"-p"; P"ocamldoc/stdlib_man"]);
        Cmd(S[ocamldoc; A"-man"; A"-d"; P"ocamldoc/stdlib_man";
              A"-I"; P "stdlib"; A"-I"; P"otherlibs/unix"; A"-I"; P"otherlibs/num";
              A"-t"; A"OCaml library"; A"-man-mini"; atomize stdlib_mlis])]
  end;;

flag ["ocaml"; "compile"; "bootstrap_thread"]
     (S[A"-I"; P systhreads_dir; A"-I"; P threads_dir]);;

flag ["ocaml"; "link"; "bootstrap_thread"]
     (S[A"-I"; P systhreads_dir; A"-I"; P threads_dir]);;

(* Sys threads *)

let systhreads_stubs_headers =
  List.map systhreads_file
    [if windows then "st_win32.h" else "st_posix.h"; "threads.h"]
;;

rule "native systhreads"
  ~prod:(systhreads_obj "st_stubs_n")
  ~deps:(systhreads_file "st_stubs.c" :: systhreads_stubs_headers)
  ~insert:`top
  begin fun _ _ ->
    mknatobj (systhreads_obj "st_stubs_n")
             (systhreads_file "st_stubs.c")
             (S[A"-I../asmrun"; A"-I../byterun"; A"-Iotherlibs/systhreads";
                if windows then N else Sh C.sharedcccompopts;
                A"-DNATIVE_CODE"; A("-DTARGET_"^C.arch); A("-DSYS_"^C.system)])
  end;;

rule "bytecode systhreads"
  ~prod:(systhreads_obj "st_stubs_b")
  ~deps:(systhreads_file "st_stubs.c" :: systhreads_stubs_headers)
  ~insert:`top
  begin fun _ _ ->
    mkobj (systhreads_obj "st_stubs_b") (systhreads_file "st_stubs.c")
          (S[A"-I../byterun"; A"-Iotherlibs/systhreads"; Sh C.sharedcccompopts])
  end;;

rule "libthreadsnat.a"
  ~prod:(systhreads_lib "libthreadsnat")
  ~dep:(systhreads_obj "st_stubs_n")
  ~insert:`top
  begin fun _ _ ->
    if windows then
      mklib (systhreads_lib "libthreadsnat") (P(systhreads_obj "st_stubs_n")) N
    else
      (* Dynamic linking with -lpthread is risky on many platforms, so
        do not create a shared object for libthreadsnat. *)
      Cmd(S[ar; A"rc"; Px(systhreads_lib "libthreadsnat");
            P(systhreads_obj "st_stubs_n")])
  end;

(* See remark above: force static linking of libthreadsnat.a *)
if windows then
  flag ["ocaml"; "link"; "library"; "otherlibs_systhreads"; "native"] begin
    S[A"-cclib"; syscamllib "threadsnat"; (* A"-cclib"; syscamllib "unix"; seems to be useless and can be dangerous during bootstrap *) Sh C.pthread_link]
  end;;

flag ["ocaml"; "ocamlmklib"; "otherlibs_systhreads"] (S[(* A"-cclib"; syscamllib "unix";; seems to be useless and can be dangerous during bootstrap *) Sh C.pthread_link]);;

flag ["c"; "compile"; "otherlibs"] begin
  S[A"-I"; P"../byterun";
    A"-I"; P(".."/unix_dir);
    Sh C.bytecccompopts;
    Sh C.sharedcccompopts]
end;;

flag ["c"; "compile"; "otherlibs"; "cc"] (A"-O");;
flag ["c"; "compile"; "otherlibs"; "mingw"] (A"-O");;

(* The numeric opcodes *)
rule "The numeric opcodes"
  ~prod:"bytecomp/opcodes.ml"
  ~dep:"byterun/instruct.h"
  ~insert:`top
        begin fun _ _ ->
          Cmd(Sh "sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
        awk -f ../tools/make-opcodes > bytecomp/opcodes.ml")
  end;;

rule "tools/opnames.ml"
  ~prod:"tools/opnames.ml"
  ~dep:"byterun/instruct.h"
  begin fun _ _ ->
    Cmd(Sh"unset LC_ALL || : ; \
        unset LC_CTYPE || : ; \
        unset LC_COLLATE LANG || : ; \
        sed -e '/\\/\\*/d' \
              -e '/^#/d' \
              -e 's/enum \\(.*\\) {/let names_of_\\1 = [|/' \
              -e 's/};$/ |]/' \
              -e 's/\\([A-Z][A-Z_0-9a-z]*\\)/\"\\1\"/g' \
              -e 's/,/;/g' \
          byterun/instruct.h > tools/opnames.ml")
  end;;

(* The version number *)
rule "stdlib/sys.ml"
  ~prod:"stdlib/sys.ml"
  ~deps:["stdlib/sys.mlp"; "VERSION"]
  begin fun _ _ ->
    let version = with_input_file "VERSION" input_line in
    Seq [rm_f "stdlib/sys.ml";
         Cmd (S[A"sed"; A"-e";
                A(sprintf "s,%%%%VERSION%%%%,%s," version);
                Sh"<"; P"stdlib/sys.mlp"; Sh">"; Px"stdlib/sys.ml"]);
         chmod (A"-w") "stdlib/sys.ml"]
  end;;

(* The predefined exceptions and primitives *)

rule "camlheader"
  ~prods:["stdlib/camlheader"; "stdlib/camlheader_ur"]
  ~deps:["stdlib/header.c"; "stdlib/headernt.c"]
  begin fun _ _ ->
    if C.sharpbangscripts then
      Cmd(Sh("echo '#!"^C.bindir^"/ocamlrun' > stdlib/camlheader && \
              echo '#!' | tr -d '\\012' > stdlib/camlheader_ur"))
    else if windows then
      Seq[mkexe "tmpheader.exe" (P"stdlib/headernt.c") (S[A"-I../byterun"; Sh C.extralibs]);
          rm_f "camlheader.exe";
          mv "tmpheader.exe" "stdlib/camlheader";
          cp "stdlib/camlheader" "stdlib/camlheader_ur"]
    else
      let tmpheader = "tmpheader"^C.exe in
      Cmd(S[Sh C.bytecc; Sh C.bytecccompopts; Sh C.bytecclinkopts;
            A"-I"; A"../stdlib";
            A("-DRUNTIME_NAME='\""^C.bindir^"/ocamlrun\"'");
            A"stdlib/header.c"; A"-o"; Px tmpheader; Sh"&&";
            A"strip"; P tmpheader; Sh"&&";
            A"mv"; P tmpheader; A"stdlib/camlheader"; Sh"&&";
            A"cp"; A"stdlib/camlheader"; A"stdlib/camlheader_ur"])
  end;;

(* Private copy of dynlink.{ml,mli} in debugger/ *)
copy_rule "otherlibs/dynlink/dynlink.mli -> debugger/dynlink.mli" "otherlibs/dynlink/dynlink.mli" "debugger/dynlink.mli";;
rule "debugger/dynlink.ml"
  ~prod: "debugger/dynlink.ml"
  ~dep: "otherlibs/dynlink/dynlink.ml"
  begin fun _ _ ->
    Cmd(Sh"grep -v 'REMOVE_ME for ../../debugger/dynlink.ml' \
           < otherlibs/dynlink/dynlink.ml >debugger/dynlink.ml")
  end;;


copy_rule "win32unix use some unix files" "otherlibs/unix/%" "otherlibs/win32unix/%";;

(* Temporary rule *)
rule "tools/ocamlmklib.ml"
  ~prod:"tools/ocamlmklib.ml"
  ~dep:"tools/ocamlmklib.mlp"
  (fun _ _ -> cp "tools/ocamlmklib.mlp" "tools/ocamlmklib.ml");;


rule "bytecomp/runtimedef.ml"
  ~prod:"bytecomp/runtimedef.ml"
  ~deps:["byterun/primitives"; "byterun/fail.h"]
  begin fun _ _ ->
    Cmd(S[A"../build/mkruntimedef.sh";Sh">"; Px"bytecomp/runtimedef.ml"])
  end;;

(* Choose the right machine-dependent files *)

let mk_arch_rule ~src ~dst =
  let prod = "asmcomp"/dst in
  let dep = "asmcomp"/C.arch/src in
  rule (sprintf "arch specific files %S%%" dst) ~prod ~dep begin
    if windows then fun env _ -> cp (env dep) (env prod)
    else fun env _ -> ln_s (env (C.arch/src)) (env prod)
  end;;

mk_arch_rule ~src:(if ccomptype = "msvc" then "proc_nt.ml" else "proc.ml") ~dst:"proc.ml";;
List.iter (fun x -> mk_arch_rule ~src:x ~dst:x)
          ["arch.ml"; "reload.ml"; "scheduling.ml"; "selection.ml"];;

let emit_mlp = "asmcomp"/C.arch/(if ccomptype = "msvc" then "emit_nt.mlp" else "emit.mlp") in
rule "emit.mlp"
  ~prod:"asmcomp/emit.ml"
  ~deps:[emit_mlp; "tools/cvt_emit.byte"]
  begin fun _ _ ->
    Cmd(S[ocamlrun; P"tools/cvt_emit.byte"; Sh "<"; P emit_mlp;
          Sh">"; Px"asmcomp/emit.ml"])
  end;;
      end in ()
  | _ -> ()
end
