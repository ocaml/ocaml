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
  target: Unit_info.t;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let target = Unit_info.make ~source_file output_prefix in
  Env.set_unit_name (Unit_info.modname target);
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    target;
    env;
    ppf_dump;
    tool_name;
    native;
  }

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name (Unit_info.source_file i.target)
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.interface

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tintf =
    ast
    |> Typemod.type_interface info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let tsg = tintf.Typedtree.intf_signature in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature (Unit_info.source_file info.target))
          sg);
  ignore (Includemod.signatures info.env ~mark:Mark_both sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tintf

let emit_interface info ast tintf =
  let tsg = tintf.Typedtree.intf_signature in
  let sg =
    let alerts = Builtin_attributes.alerts_of_sig ast.Parsetree.pintf_signature in
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      (Unit_info.cmi info.target)
  in
  Typemod.save_signature info.target tsg info.env sg

let interface info =
  Profile.record_call (Unit_info.source_file info.target) @@ fun () ->
  let ast = parse_intf info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tintf = typecheck_intf info ast in
    if not !Clflags.print_types then begin
      emit_interface info ast tintf
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  let sourcefile = Unit_info.source_file i.target in
  Pparse.parse_implementation ~tool_name:i.tool_name sourcefile
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.implementation

let typecheck_impl i parsetree =
  parsetree
  |> Profile.(record typing)
    (Typemod.type_implementation i.target i.env)
  |> print_if i.ppf_dump Clflags.dump_typedtree
    Printtyped.implementation
  |> print_if i.ppf_dump Clflags.dump_shape
    (fun fmt {Typedtree.impl_shape; _} -> Shape.print fmt impl_shape)

let implementation info ~backend =
  Profile.record_call (Unit_info.source_file info.target) @@ fun () ->
  let exceptionally () =
    let sufs =
      if info.native then Unit_info.[ cmx; obj ]
      else Unit_info.[ cmo ] in
    List.iter
      (fun suf -> remove_file (Unit_info.Artifact.filename @@ suf info.target))
      sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Warnings.check_fatal ();
  )
