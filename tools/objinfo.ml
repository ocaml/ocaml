(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*         Mehdi Dogguy, PPS laboratory, University Paris Diderot         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2010 Mehdi Dogguy                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dump info on .cmi, .cmo, .cmx, .cma, .cmxa, .cmxs files
   and on bytecode executables. *)

open Printf
open Cmo_format

(* Command line options to prevent printing approximation,
   function code and CRC
 *)
let quiet = ref false
let no_approx = ref false
let no_code = ref false
let no_crc = ref false
let shape = ref false
let index = ref false
let decls = ref false
let uid_deps = ref false

module Magic_number = Misc.Magic_number

let dummy_crc = String.make 32 '-'
let null_crc = String.make 32 '0'

let string_of_crc crc = if !no_crc then null_crc else Digest.to_hex crc

let print_name_crc (name, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> string_of_crc crc
  in
    printf "\t%s\t%s\n" crc name

let print_line name =
  printf "\t%s\n" name

let print_required_compunit (Compunit cu_name) =
  printf "\t%s\n" cu_name

let print_cmo_infos cu =
  printf "Unit name: %s\n" (Symtable.Compunit.name cu.cu_name);
  print_string "Interfaces imported:\n";
  List.iter print_name_crc cu.cu_imports;
  print_string "Required globals:\n";
  List.iter print_required_compunit cu.cu_required_compunits;
  printf "Uses unsafe features: ";
  (match cu.cu_primitives with
    | [] -> printf "no\n"
    | l  ->
        printf "YES\n";
        printf "Primitives declared in this module:\n";
        List.iter print_line l);
  printf "Force link: %s\n" (if cu.cu_force_link then "YES" else "no")

let print_spaced_string s =
  printf " %s" s

let print_cma_infos (lib : Cmo_format.library) =
  printf "Force custom: %s\n" (if lib.lib_custom then "YES" else "no");
  printf "Extra C object files:";
  (* PR#4949: print in linking order *)
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev lib.lib_ccopts);
  printf "\n";
  print_string "Extra dynamically-loaded libraries:";
  List.iter print_spaced_string (List.rev lib.lib_dllibs);
  printf "\n";
  List.iter print_cmo_infos lib.lib_units

let print_cmi_infos name crcs =
  if not !quiet then begin
    printf "Unit name: %s\n" name;
    printf "Interfaces imported:\n";
    List.iter print_name_crc crcs
  end

let print_cmt_infos cmt =
  let open Cmt_format in
  if not !quiet then begin
    printf "Cmt unit name: %s\n" cmt.cmt_modname;
    print_string "Cmt interfaces imported:\n";
    List.iter print_name_crc cmt.cmt_imports;
    printf "Source file: %s\n"
          (match cmt.cmt_sourcefile with None -> "(none)" | Some f -> f);
    printf "Compilation flags:";
    Array.iter print_spaced_string cmt.cmt_args;
    printf "\nLoad path:\n  Visible:";
    List.iter print_spaced_string cmt.cmt_loadpath.visible;
    printf "\n  Hidden:";
    List.iter print_spaced_string cmt.cmt_loadpath.hidden;
    printf "\n";
    printf "cmt interface digest: %s\n"
      (match cmt.cmt_interface_digest with
      | None -> ""
      | Some crc -> string_of_crc crc);
  end;
  if !shape then begin
    printf "Implementation shape: ";
    (match cmt.cmt_impl_shape with
    | None -> printf "(none)\n"
    | Some shape -> Format.printf "\n%a" Shape.print shape)
  end;
  if !index then begin
    printf "Indexed shapes:\n";
    List.iter (fun (loc, item) ->
      let pp_loc fmt { Location.txt; loc } =
        Format.fprintf fmt "%a (%a)"
          Pprintast.longident txt Location.print_loc loc
      in
      Format.printf "@[<hov 2>%a:@ %a@]@;"
        Shape_reduce.print_result item pp_loc loc)
      cmt.cmt_ident_occurrences;
    Format.print_flush ()
  end;
  if !uid_deps then begin
    printf "\nUid dependencies:\n";
    let arr = Array.of_list cmt.cmt_declaration_dependencies in
    let () =
      Array.sort (fun (_tr, u1, u2) (_tr', u1', u2') ->
                    match Shape.Uid.compare u1 u1' with
                    | 0 -> Shape.Uid.compare u2 u2'
                    | n -> n) arr
    in
    Format.printf "@[<v>";
    Array.iter (fun (rk, u1, u2) ->
      let rk = match rk with
        | Definition_to_declaration -> "<-"
        | Declaration_to_declaration -> "<->"
      in
      Format.printf "@[<h>%a %s %a@]@;"
        Shape.Uid.print u1
        rk
        Shape.Uid.print u2) arr;
    Format.printf "@]";
  end;
  if !decls then begin
    printf "\nUid of decls:\n";
    let decls = Array.of_list (Shape.Uid.Tbl.to_list cmt.cmt_uid_to_decl) in
    Array.sort (fun (uid, _) (uid', _) -> Shape.Uid.compare uid uid') decls;
    Array.iter (fun (uid, item) ->
      let loc = match (item : Typedtree.item_declaration) with
        | Value vd -> vd.val_name
        | Value_binding vb ->
          let (_, name, _, _) =
            List.hd (Typedtree.let_bound_idents_full [vb])
          in
          name
        | Type td -> td.typ_name
        | Constructor cd -> cd.cd_name
        | Extension_constructor ec -> ec.ext_name
        | Label ld -> ld.ld_name
        | Module md ->
          { md.md_name with
            txt = Option.value md.md_name.txt ~default:"_" }
        | Module_substitution ms -> ms.ms_name
        | Module_binding mb ->
          { mb.mb_name with
            txt = Option.value mb.mb_name.txt ~default:"_" }
        | Module_type mtd -> mtd.mtd_name
        | Class cd -> cd.ci_id_name
        | Class_type ctd -> ctd.ci_id_name
      in
      let pp_loc fmt { Location.txt; loc } =
        Format.fprintf fmt "%s (%a)"
           txt Location.print_loc loc
      in
      Format.printf "@[<hov 2>%a:@ %a@]@;"
        Shape.Uid.print uid
        pp_loc loc)
      decls;
    Format.print_flush ()
  end

let print_general_infos name crc defines cmi cmx =
  printf "Name: %s\n" name;
  printf "CRC of implementation: %s\n" (string_of_crc crc);
  printf "Globals defined:\n";
  List.iter print_line defines;
  printf "Interfaces imported:\n";
  List.iter print_name_crc cmi;
  printf "Implementations imported:\n";
  List.iter print_name_crc cmx

let print_global_table table =
  printf "Globals defined:\n";
  Symtable.iter_global_map
    (fun global _ ->
       let desc = Format_doc.compat Symtable.Global.description in
       print_line (Format.asprintf "%a" desc global)
    )
    table

open Cmx_format
open Cmxs_format

let print_cmx_infos (ui, crc) =
  print_general_infos
    ui.ui_name crc ui.ui_defines ui.ui_imports_cmi ui.ui_imports_cmx;
  begin match ui.ui_export_info with
  | Clambda approx ->
    if not !no_approx then begin
      printf "Clambda approximation:\n";
      Format.fprintf Format.std_formatter "  %a@." Printclambda.approx approx
    end else
      Format.printf "Clambda unit@.";
  | Flambda export ->
    if not !no_approx || not !no_code then
      printf "Flambda export information:\n"
    else
      printf "Flambda unit\n";
    if not !no_approx then begin
      let cu =
        Compilation_unit.create (Ident.create_persistent ui.ui_name)
          (Linkage_name.create "__dummy__")
      in
      Compilation_unit.set_current cu;
      let root_symbols =
        List.map (fun s ->
            Symbol.of_global_linkage cu (Linkage_name.create ("caml"^s)))
          ui.ui_defines
      in
      Format.printf "approximations@ %a@.@."
        Export_info.print_approx (export, root_symbols)
    end;
    if not !no_code then
      Format.printf "functions@ %a@.@."
        Export_info.print_functions export
  end;
  let pr_funs _ fns =
    List.iter (fun arity -> printf " %d" arity) fns in
  printf "Currying functions:%a\n" pr_funs ui.ui_curry_fun;
  printf "Apply functions:%a\n" pr_funs ui.ui_apply_fun;
  printf "Send functions:%a\n" pr_funs ui.ui_send_fun;
  printf "Force link: %s\n" (if ui.ui_force_link then "YES" else "no");
  printf "For pack: %s\n"
    (match ui.ui_for_pack with
     | None -> "no"
     | Some pack -> "YES: " ^ pack)

let print_cmxa_infos (lib : Cmx_format.library_infos) =
  printf "Extra C object files:";
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev lib.lib_ccopts);
  printf "\n";
  List.iter print_cmx_infos lib.lib_units

let print_cmxs_infos header =
  List.iter
    (fun ui ->
       print_general_infos
         ui.dynu_name
         ui.dynu_crc
         ui.dynu_defines
         ui.dynu_imports_cmi
         ui.dynu_imports_cmx)
    header.dynu_units

let p_title title = printf "%s:\n" title

let p_section title = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print_name_crc l

let p_list title print = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print l

let dump_byte ic =
  let toc = Bytesections.read_toc ic in
  let all = Bytesections.all toc in
  List.iter
    (fun {Bytesections.name = section; len; _} ->
       try
         if len > 0 then match section with
           | CRCS ->
               let imported_units : (string * Digest.t option) list =
                 Bytesections.read_section_struct toc ic section in
               p_section "Imported units" imported_units
           | DLLS ->
               let dlls =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Used DLLs" print_line dlls
           | DLPT ->
               let dll_paths =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Additional DLL paths" print_line dll_paths
           | PRIM ->
               let prims =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Primitives used" print_line prims
           | SYMB ->
               let symb = Bytesections.read_section_struct toc ic section in
               print_global_table symb
           | _ -> ()
       with _ -> ()
    )
    all

let find_dyn_offset filename =
  match Binutils.read filename with
  | Ok t ->
      Binutils.symbol_offset t "caml_plugin_header"
  | Error _ ->
      None

let exit_err msg = print_endline msg; exit 2
let exit_errf fmt = Printf.ksprintf exit_err fmt

let exit_magic_msg msg =
  exit_errf
     "Wrong magic number:\n\
      this tool only supports object files produced by compiler version\n\
      \t%s\n\
      %s"
    Sys.ocaml_version msg

let exit_magic_error ~expected_kind err =
  exit_magic_msg Magic_number.(match err with
    | Parse_error err -> explain_parse_error expected_kind err
    | Unexpected_error err -> explain_unexpected_error err)

(* assume that 'ic' is already positioned at the right place
   depending on the format (usually right after the magic number,
   but Exec and Cmxs differ) *)
let dump_obj_by_kind filename ic obj_kind =
  let open Magic_number in
  match obj_kind with
    | Cmo ->
       let cu_pos = input_binary_int ic in
       seek_in ic cu_pos;
       let cu = (input_value ic : compilation_unit) in
       close_in ic;
       print_cmo_infos cu
    | Cma ->
       let toc_pos = input_binary_int ic in
       seek_in ic toc_pos;
       let toc = (input_value ic : library) in
       close_in ic;
       print_cma_infos toc
    | Cmi | Cmt ->
       close_in ic;
       let cmi, cmt = Cmt_format.read filename in
       begin match cmi with
         | None -> ()
         | Some cmi ->
            print_cmi_infos cmi.Cmi_format.cmi_name cmi.Cmi_format.cmi_crcs
       end;
       begin match cmt with
         | None -> ()
         | Some cmt -> print_cmt_infos cmt
       end
    | Cmx _config ->
       let ui = (input_value ic : unit_infos) in
       let crc = Digest.input ic in
       close_in ic;
       print_cmx_infos (ui, crc)
    | Cmxa _config ->
       let li = (input_value ic : library_infos) in
       close_in ic;
       print_cmxa_infos li
    | Exec ->
       (* no assumptions on [ic] position,
          [dump_byte] will seek at the right place *)
       dump_byte ic;
       close_in ic
    | Cmxs ->
       (* we assume we are at the offset of the dynamic information,
          as returned by [find_dyn_offset]. *)
       let header = (input_value ic : dynheader) in
       close_in ic;
       print_cmxs_infos header;
    | Ast_impl | Ast_intf ->
       exit_errf "The object file type %S \
                  is currently unsupported by this tool."
         (human_name_of_kind obj_kind)

let dump_obj filename =
  let open Magic_number in
  let dump_standard ic =
    match read_current_info ~expected_kind:None ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind:None err
      | Ok { kind; version = _ } ->
         dump_obj_by_kind filename ic kind;
         Ok ()
      | Error (Parse_error head_error) ->
         Error head_error
  and dump_exec ic =
    let pos_trailer = in_channel_length ic - Magic_number.magic_length in
    let _ = seek_in ic pos_trailer in
    let expected_kind = Some Exec in
    match read_current_info ~expected_kind ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind err
      | Ok _ ->
         dump_obj_by_kind filename ic Exec;
         Ok ()
      | Error (Parse_error _)  ->
         Error ()
  and dump_cmxs ic =
    flush stdout;
    match find_dyn_offset filename with
      | None ->
         exit_errf "Unable to read info on %s %s."
           (human_name_of_kind Cmxs) filename
      | Some offset ->
         LargeFile.seek_in ic offset;
         let header = (input_value ic : dynheader) in
         let expected_kind = Some Cmxs in
         match parse header.dynu_magic with
           | Error err ->
              exit_magic_error ~expected_kind (Parse_error err)
           | Ok info ->
         match check_current Cmxs info with
           | Error err ->
              exit_magic_error ~expected_kind (Unexpected_error err)
           | Ok () ->
         LargeFile.seek_in ic offset;
         dump_obj_by_kind filename ic Cmxs;
         ()
  in
  if not !quiet then printf "File %s\n" filename;
  let ic = open_in_bin filename in
  match dump_standard ic with
    | Ok () -> ()
    | Error head_error ->
  match dump_exec ic with
    | Ok () -> ()
    | Error () ->
  if Filename.check_suffix filename ".cmxs"
  then dump_cmxs ic
  else exit_magic_error ~expected_kind:None (Parse_error head_error)

let print_version () =
  Format.printf "ocamlobjinfo, version %s@." Sys.ocaml_version;
  exit 0

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0

let arg_list = [
  "-quiet", Arg.Set quiet,
    " Only print explicitly required information";
  "-no-approx", Arg.Set no_approx,
    " Do not print module approximation information";
  "-no-code", Arg.Set no_code,
    " Do not print code from exported flambda functions";
  "-shape", Arg.Set shape,
    " Print the shape of the module";
  "-index", Arg.Set index,
    " Print a list of all usages of values, types, etc. in the module";
  "-decls", Arg.Set decls,
    " Print a list of all declarations in the module";
  "-uid-deps", Arg.Set uid_deps,
    " Print the declarations' uids dependencies of the module";
  "-null-crc", Arg.Set no_crc, " Print a null CRC for imported interfaces";
  "-version", Arg.Unit print_version, " Print version and exit";
  "-vnum", Arg.Unit print_version_num, " Print version number and exit";
  "-args", Arg.Expand Arg.read_arg,
     "<file> Read additional newline separated command line arguments \n\
     \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
     "<file> Read additional NUL separated command line arguments from \n\
     \      <file>";
]
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main () =
  Arg.parse_expand arg_list dump_obj arg_usage;
  exit 0

let _ = main ()
