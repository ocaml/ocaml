(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

open Printf
open Misc
open Lambda
open Clambda
open Compilenv

type error =
    Illegal_renaming of string * string
  | Forward_reference of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string
  | No_binutils

exception Error of error

(* Read the unit information from a .cmx file. *)

let read_unit_info cmxfile =
  let (info, crc) = Compilenv.read_unit_info cmxfile in
  if info.ui_name
  <> String.capitalize(Filename.basename(chop_extension_if_any cmxfile))
  then raise(Error(Illegal_renaming(cmxfile, info.ui_name)));
  Asmlink.check_consistency cmxfile info crc;
  info

(* Check absence of forward references *)

let check_units cmxfiles units unit_names =
  let rec check forbidden = function
    [] -> ()
  | (cmxfile, infos) :: tl ->
      List.iter
        (fun (unit, _) ->
          if List.mem unit forbidden
          then raise(Error(Forward_reference(cmxfile, unit))))
        infos.ui_imports_cmx;
      check (list_remove infos.ui_name forbidden) tl in
  check unit_names (List.combine cmxfiles units)

(* Rename symbols in an object file.  All defined symbols of the form
   [T] or [T]__xxx, where [T] belongs to the list [units], are prefixed by 
   [pref]__ .  Return the list of renamed symbols. *)

let extract_symbols units symbolfile =
  let symbs = ref [] in
  let ic = open_in symbolfile in
  begin try
    while true do
      let l = input_line ic in
      try
        let i = 3 + (try search_substring " T " l 0
                     with Not_found -> search_substring " D " l 0) in
        let j = try search_substring "__" l i
                with Not_found -> String.length l in
        let k = if l.[i] = '_' then i + 1 else i in
        if List.mem (String.sub l k (j - k)) units then
          symbs := (String.sub l i (String.length l - i)) :: !symbs
      with Not_found ->
        ()
    done
  with End_of_file -> close_in ic
     | x -> close_in ic; raise x
  end;
  !symbs

let max_cmdline_length = 3500 (* safe approximation *)

let remove_leading_underscore s =
  if String.length s > 0 && s.[0] = '_'
  then String.sub s 1 (String.length s - 1)
  else s

let prefix_symbol p s =
  if String.length s > 0 && s.[0] = '_'
  then "_" ^ p ^ "__" ^ String.sub s 1 (String.length s - 1)
  else p ^ "__" ^ s

let rename_in_object_file units pref objfile =
  let symbolfile = Filename.temp_file "camlsymbols" "" in
  try
    let nm_cmdline =
      sprintf "%s %s > %s"
              Config.binutils_nm
              (Filename.quote objfile) (Filename.quote symbolfile) in
    if Ccomp.command nm_cmdline <> 0 then raise(Error Linking_error);
    let symbols_to_rename =
      extract_symbols units symbolfile in
    let cmdline =
      Buffer.create max_cmdline_length in
    let rec call_objcopy = function
      [] ->
        Buffer.add_char cmdline ' ';
        Buffer.add_string cmdline (Filename.quote objfile);
        if Ccomp.command (Buffer.contents cmdline) <> 0
        then raise(Error Linking_error)
    | s :: rem ->
        if Buffer.length cmdline >= max_cmdline_length then begin
          Buffer.add_char cmdline ' ';
          Buffer.add_string cmdline (Filename.quote objfile);
          if Ccomp.command (Buffer.contents cmdline) <> 0
          then raise(Error Linking_error);
          Buffer.reset cmdline;
          Buffer.add_string cmdline Config.binutils_objcopy
        end;
        bprintf cmdline " --redefine-sym '%s=%s'" s (prefix_symbol pref s);
        call_objcopy rem in
    Buffer.add_string cmdline Config.binutils_objcopy;
    call_objcopy symbols_to_rename;
    remove_file symbolfile;
    List.map remove_leading_underscore symbols_to_rename
  with x ->
    remove_file symbolfile;
    raise x

(* Rename function symbols and global symbols in value approximations *)

let rename_approx mapping approx =

  let ren_label lbl =
    try Tbl.find lbl mapping with Not_found -> lbl in
  let ren_ident id =
    if Ident.persistent id
    then Ident.create_persistent(ren_label(Ident.name id))
    else id in

  let rec ren_ulambda = function
    Uvar id ->
      Uvar(ren_ident id)
  | Uconst cst ->
      Uconst cst
  | Udirect_apply(lbl, args) ->
      Udirect_apply(ren_label lbl, List.map ren_ulambda args)
  | Ugeneric_apply(fn, args) ->
      Ugeneric_apply(ren_ulambda fn, List.map ren_ulambda args)
  | Uclosure(fns, env) ->
      (* never present in an inlined function body *)
      assert false
  | Uoffset(lam, ofs) -> Uoffset(ren_ulambda lam, ofs)
  | Ulet(id, u, body) -> Ulet(id, ren_ulambda u, ren_ulambda body)
  | Uletrec(defs, body) ->
      (* never present in an inlined function body *)
      assert false
  | Uprim(prim, args) ->
      let prim' =
        match prim with
          Pgetglobal id -> Pgetglobal(ren_ident id)
        | Psetglobal id -> assert false (* never present in inlined fn body *)
        | _ -> prim in
      Uprim(prim', List.map ren_ulambda args)
  | Uswitch(u, cases) ->
      Uswitch(ren_ulambda u,
        {cases with
         us_actions_consts = Array.map ren_ulambda cases.us_actions_consts;
         us_actions_blocks = Array.map ren_ulambda cases.us_actions_blocks})
  | Ustaticfail(tag, args) ->
      Ustaticfail(tag, List.map ren_ulambda args)
  | Ucatch(nfail, ids, u1, u2) ->
      Ucatch(nfail, ids, ren_ulambda u1, ren_ulambda u2)
  | Utrywith(u1, id, u2) ->
      Utrywith(ren_ulambda u1, id, ren_ulambda u2)
  | Uifthenelse(u1, u2, u3) ->
      Uifthenelse(ren_ulambda u1, ren_ulambda u2, ren_ulambda u3)
  | Usequence(u1, u2) ->
      Usequence(ren_ulambda u1, ren_ulambda u2)
  | Uwhile(u1, u2) ->
      Uwhile(ren_ulambda u1, ren_ulambda u2)
  | Ufor(id, u1, u2, dir, u3) ->
      Ufor(id, ren_ulambda u1, ren_ulambda u2, dir, ren_ulambda u3)
  | Uassign(id, u) ->
      Uassign(id, ren_ulambda u)
  | Usend(u1, u2, ul) ->
      Usend(ren_ulambda u1, ren_ulambda u2, List.map ren_ulambda ul) in

  let rec ren_approx = function
      Value_closure(fd, res) ->
        let fd' =
          {fd with
           fun_label = ren_label fd.fun_label;
           fun_inline =
             match fd.fun_inline with
               None -> None
             | Some(params, body) -> Some(params, ren_ulambda body)} in
        Value_closure(fd', ren_approx res)
    | Value_tuple comps ->
        Value_tuple (Array.map ren_approx comps)
    | app -> app

  in ren_approx approx

(* Make the .cmx file for the package *)

let build_package_cmx units unit_names target symbols_to_rename cmxfile =
  let filter lst =
    List.filter (fun (name, crc) -> not (List.mem name unit_names)) lst in
  let union lst =
    List.fold_left
      (List.fold_left
          (fun accu n -> if List.mem n accu then accu else n :: accu))
      [] lst in
  let mapping =
    List.fold_left (fun tbl s -> Tbl.add s (target ^ "__" ^ s) tbl)
                   Tbl.empty symbols_to_rename in
  let defines =
    map_end (fun s -> target ^ "__" ^ s)
            (List.concat (List.map (fun info -> info.ui_defines) units))
            [target] in
  let approx =
    Compilenv.global_approx (Ident.create_persistent target) in
  let pkg_infos =
    { ui_name = target;
      ui_defines = defines;
      ui_imports_cmi = (target, Env.crc_of_unit target) ::
                       filter(Asmlink.extract_crc_interfaces());
      ui_imports_cmx = filter(Asmlink.extract_crc_implementations());
      ui_approx = rename_approx mapping approx;
      ui_curry_fun = union(List.map (fun info -> info.ui_curry_fun) units);
      ui_apply_fun = union(List.map (fun info -> info.ui_apply_fun) units);
      ui_force_link = List.exists (fun info -> info.ui_force_link) units
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .o file for the package (not renamed yet) *)

let make_package_object ppf unit_names objfiles
                        targetobj targetname coercion =
  let objtemp = Filename.temp_file "camlpackage" Config.ext_obj in
  Location.input_name := targetname; (* set the name of the "current" input *)
  Compilenv.reset targetname; (* set the name of the "current" compunit *)
  Asmgen.compile_implementation
    (chop_extension_if_any objtemp) ppf
    (Translmod.transl_store_package
       (List.map Ident.create_persistent unit_names)
       (Ident.create_persistent targetname) coercion);
  let ld_cmd =
    sprintf "%s -o %s %s %s"
            Config.native_pack_linker 
            (Filename.quote targetobj)
            (Filename.quote objtemp)
            (Ccomp.quote_files objfiles) in
  let retcode = Ccomp.command ld_cmd in
  remove_file objtemp;
  if retcode <> 0 then raise(Error Linking_error)

(* Make the .cmx and the .o for the package *)

let package_object_files ppf cmxfiles targetcmx 
                         targetobj targetname coercion =
  let units = map_left_right read_unit_info cmxfiles in
  let unit_names = List.map (fun info -> info.ui_name) units in
  check_units cmxfiles units unit_names;
  let objfiles =
    List.map (fun f -> chop_extension_if_any f ^ Config.ext_obj) cmxfiles in
  make_package_object ppf unit_names objfiles targetobj targetname coercion;
  let symbols = rename_in_object_file unit_names targetname targetobj in
  build_package_cmx units unit_names targetname symbols targetcmx

(* The entry point *)

let package_files ppf files targetcmx =
  if Config.binutils_objcopy = "" || Config.binutils_nm = ""
  then raise (Error No_binutils);
  let cmxfiles =
    List.map
      (fun f ->
        try find_in_path !Config.load_path f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extension_if_any targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = prefix ^ Config.ext_obj in
  let targetname = String.capitalize(Filename.basename prefix) in
  try
    let coercion = Typemod.package_units cmxfiles targetcmi targetname in
    package_object_files ppf cmxfiles targetcmx targetobj targetname coercion
  with x ->
    remove_file targetcmx; remove_file targetobj;
    raise x

(* Error report *)

open Format

let report_error ppf = function
    Illegal_renaming(file, id) ->
      fprintf ppf "Wrong file naming: %s@ contains the code for@ %s"
        file id
  | Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %s" ident file
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %s" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"
  | No_binutils ->
      fprintf ppf "ocamlopt -pack is not supported on this platform.@ \
                   Reason: the GNU `binutils' tools are not available"
