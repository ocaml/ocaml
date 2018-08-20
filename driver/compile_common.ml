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
open Compenv

type info = {
  sourcefile : string;
  modulename : string;
  outputprefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
}


let cmx i = i.outputprefix ^ ".cmx"
let obj i = i.outputprefix ^ Config.ext_obj
let cmo i = i.outputprefix ^ ".cmo"
let annot i = i.outputprefix ^ ".annot"

let init ppf_dump ~init_path ~tool_name ~sourcefile ~outputprefix =
  Compmisc.init_path init_path;
  let modulename = module_of_filename sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  { modulename; outputprefix; env; sourcefile; ppf_dump; tool_name }


(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.sourcefile
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface info.sourcefile info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env
      (fun (module Printtyp:Printtyp.S) ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.sourcefile)
          sg);
  ignore (Includemod.signatures info.env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg =
    let deprecated = Builtin_attributes.deprecated_of_sig ast in
    Env.save_signature ~deprecated tsg.Typedtree.sig_type
      info.modulename (info.outputprefix ^ ".cmi")
  in
  Typemod.save_signature info.modulename tsg
    info.outputprefix info.sourcefile info.env sg

let interface ~tool_name ~sourcefile ~outputprefix =
  Compmisc.with_ppf_dump ~fileprefix:(outputprefix ^ ".cmi") @@ fun ppf_dump ->
  Profile.record_call sourcefile @@ fun () ->
  let info =
    init ppf_dump ~init_path:false ~tool_name ~sourcefile ~outputprefix
  in
  let ast = parse_intf info in
  let tsg = typecheck_intf info ast in
  if not !Clflags.print_types then begin
    emit_signature info ast tsg
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.sourcefile
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  let always () = Stypes.dump (Some (annot i)) in
  Misc.try_finally ~always (fun () ->
    parsetree
    |> Profile.(record typing)
      (Typemod.type_implementation
         i.sourcefile i.outputprefix i.modulename i.env)
    |> print_if i.ppf_dump Clflags.dump_typedtree
      Printtyped.implementation_with_coercion
  )

let implementation ~tool_name ~native ~backend ~sourcefile ~outputprefix =
  let suf, sufs = if native then ".cmx", [ cmx; obj ] else ".cmo", [ cmo ] in
  Compmisc.with_ppf_dump ~fileprefix:(outputprefix ^ suf) @@ fun ppf_dump ->
  let info =
    init ppf_dump ~init_path:native ~tool_name ~sourcefile ~outputprefix
  in
  Profile.record_call info.sourcefile @@ fun () ->
  let parsed = parse_impl info in
  let typed = typecheck_impl info parsed in
  if not !Clflags.print_types then begin
    let exceptionally () =
      List.iter (fun suf -> remove_file (suf info)) sufs;
    in
    Misc.try_finally ~exceptionally (fun () -> backend info typed)
  end;
  Warnings.check_fatal ();
