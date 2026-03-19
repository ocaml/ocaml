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

let with_info ~native ~tool_name ~dump_ext unit_info k =
  Compmisc.init_path ();
  Env.set_current_unit unit_info ;
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [Unit_info.prefix unit_info; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    target = unit_info;
    env;
    ppf_dump;
    tool_name;
    native;
  }

module Parse_result = struct
  type 'a t = { ast : 'a; info : info }

  let update_unit_info ~info ast =
    let source_file = !Location.input_name in
    let new_target =
      Unit_info.update_human_source_file_name info.target source_file
    in
    { ast; info = { info with target = new_target } }

  let print_ast_if flag ppf { ast; info } =
    { ast = print_if info.ppf_dump flag ppf ast; info }
end

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface
    ~tool_name:i.tool_name
    (Unit_info.human_source_file i.target)
  |> Parse_result.update_unit_info ~info:i
  |> Parse_result.print_ast_if Clflags.dump_parsetree Printast.interface
  |> Parse_result.print_ast_if Clflags.dump_source Pprintast.signature

let typecheck_intf { Parse_result.ast; info } =
  Profile.(record_call typing) @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let alerts = Builtin_attributes.alerts_of_sig ~mark:true ast in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature
             (Unit_info.human_source_file info.target))
          sg);
  ignore (Includemod.signatures info.env ~mark:true sg sg);
  Typecore.force_delayed_checks ();
  Builtin_attributes.warn_unused ();
  Warnings.check_fatal ();
  alerts, tsg

let emit_signature info alerts tsg =
  let sg =
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      (Unit_info.cmi info.target)
  in
  Typemod.save_signature info.target tsg info.env sg

let interface info =
  Profile.record_call (Unit_info.input_source_file info.target) @@ fun () ->
  let parse_result = parse_intf info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let alerts, tsg = typecheck_intf parse_result in
    if not !Clflags.print_types then begin
      emit_signature info alerts tsg
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation
    ~tool_name:i.tool_name
    (Unit_info.human_source_file i.target)
  |> Parse_result.update_unit_info ~info:i
  |> Parse_result.print_ast_if Clflags.dump_parsetree Printast.implementation
  |> Parse_result.print_ast_if Clflags.dump_source Pprintast.structure

let typecheck_impl { Parse_result.ast = parsetree; info = i } =
  parsetree
  |> Profile.(record typing)
    (Typemod.type_implementation i.target i.env)
  |> print_if i.ppf_dump Clflags.dump_typedtree
    Printtyped.implementation_with_coercion
  |> print_if i.ppf_dump Clflags.dump_shape
    (fun fmt {Typedtree.shape; _} -> Shape.print fmt shape)

let implementation info ~backend =
  Profile.record_call (Unit_info.input_source_file info.target) @@ fun () ->
  let exceptionally () =
    let sufs =
      if info.native then Unit_info.[ cmx; obj ]
      else Unit_info.[ cmo ] in
    List.iter
      (fun suf -> remove_file (Unit_info.Artifact.filename @@ suf info.target))
      sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parse_result = parse_impl info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl parse_result in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Builtin_attributes.warn_unused ();
    Warnings.check_fatal ();
  )
