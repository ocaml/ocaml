(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type info = {
  source_file : string (** the source file read from the CLI*);
  human_source_file : string
(** the original source file written by a human.
    In particular, generated code should point to its real source file by using
    line directives *);
  module_name : string;
  output_prefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let cmx i = i.output_prefix ^ ".cmx"
let obj i = i.output_prefix ^ Config.ext_obj
let cmo i = i.output_prefix ^ ".cmo"
let annot i = i.output_prefix ^ ".annot"

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let module_name = Compenv.module_of_filename source_file output_prefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    module_name;
    output_prefix;
    env;
    source_file;
    human_source_file=source_file;
    ppf_dump;
    tool_name;
    native;
  }


(** Recompute human source path using line directives *)
let human_source_path proj ast info =
  let location loc item =
    let item_loc = proj item in
    if item_loc.Location.loc_ghost then loc
      else
        Some item_loc.Location.loc_start.Lexing.pos_fname
  in
  match List.fold_left location None ast with
  | None -> info
  | Some source -> { info with human_source_file = source }

let impl_human_source ast info =
  human_source_path (fun item -> item.Parsetree.pstr_loc) ast info

let intf_human_source ast info =
  human_source_path (fun item -> item.Parsetree.psig_loc) ast info

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.source_file)
          sg);
  ignore (Includemod.signatures info.env ~mark:Mark_both sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg =
    let alerts = Builtin_attributes.alerts_of_sig ast in
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      ~modname:info.module_name ~source_file:info.human_source_file
      (info.output_prefix ^ ".cmi")
  in
  Typemod.save_signature info.module_name tsg
    info.output_prefix info.source_file info.env sg

let interface info =
  Profile.record_call info.source_file @@ fun () ->
  let ast = parse_intf info in
  let info = intf_human_source ast info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tsg = typecheck_intf info ast in
    if not !Clflags.print_types then begin
      emit_signature info ast tsg
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  parsetree
  |> Profile.(record typing)
    (Typemod.type_implementation
       i.source_file i.output_prefix i.module_name i.env)
  |> print_if i.ppf_dump Clflags.dump_typedtree
    Printtyped.implementation_with_coercion
  |> print_if i.ppf_dump Clflags.dump_shape
    (fun fmt {Typedtree.shape; _} -> Shape.print fmt shape)

let implementation info ~backend =
  Profile.record_call info.source_file @@ fun () ->
  let exceptionally () =
    let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
    List.iter (fun suf -> remove_file (suf info)) sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in
    let info = impl_human_source parsed info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Warnings.check_fatal ();
  )
