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

(* The interactive toplevel loop *)

open Format
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Topcommon
module String = Misc.Stdlib.String

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings : Obj.t String.Map.t ref = ref String.Map.empty

let getvalue name =
  try
    String.Map.find name !toplevel_value_bindings
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := String.Map.add name v !toplevel_value_bindings

let implementation_label = ""

(* To print values *)

module EvalBase = struct

  let eval_ident id =
    if Ident.global id then begin
      let name = Ident.name id in
      match Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:name with
      | Some v -> v
      | None ->
          raise (Undefined_global name)
    end else begin
      let name = Translmod.toplevel_name id in
      try
        String.Map.find name !toplevel_value_bindings
      with Not_found ->
        raise (Undefined_global name)
    end

end

include Topcommon.MakeEvalPrinter(EvalBase)

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let instrs, can_free = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a@."
    Printinstr.instrlist instrs;
  let (code, reloc, events) =
    Emitcode.to_memory instrs
  in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Dynlink.check_global_initialized reloc;
  Dynlink.update_global_table();
  let initial_bindings = !toplevel_value_bindings in
  let bytecode, closure = Meta.reify_bytecode code [| events |] None in
  match
    may_trace := true;
    closure ()
  with
  | retval ->
    may_trace := false;
    if can_free then Meta.release_bytecode bytecode;

    Result retval
  | exception x ->
    may_trace := false;
    record_backtrace ();
    if can_free then Meta.release_bytecode bytecode;

    toplevel_value_bindings := initial_bindings; (* PR#6211 *)
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let pr_item =
  Out_type.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (getvalue (Translmod.toplevel_name id))
                  val_type)
      | _ -> None
    )

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      let (str, sg', newenv) = typecheck_phrase ppf oldenv sstr in
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf lam in
        let out_phr =
          match res with
          | Result v ->
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                  match str.str_items with
                  | [] -> Ophr_signature []
                  | _ ->
                      match find_eval_phrase str with
                      | Some (exp, _, _) ->
                        let outv = outval_of_value newenv v exp.exp_type in
                        let ty =
                          Out_type.prepare_for_printing [exp.exp_type];
                          Out_type.tree_of_typexp Type_scheme exp.exp_type
                        in
                        Ophr_eval (outv, ty)
                      | None -> Ophr_signature (pr_item oldenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        begin match out_phr with
        | Ophr_signature [] -> ()
        | _ ->
            Location.separate_new_message ppf;
            !print_out_phrase ppf out_phr;
        end;
        if Printexc.backtrace_status ()
        then begin
          match !backtrace with
            | None -> ()
            | Some b ->
                Location.separate_new_message ppf;
                pp_print_string ppf b;
                pp_print_flush ppf ();
                backtrace := None;
        end;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      try_run_directive ppf dir_name pdir_arg

let execute_phrase print_outcome ppf phr =
  try execute_phrase print_outcome ppf phr
  with exn ->
    Warnings.reset_fatal ();
    raise exn


(* Additional directives for the bytecode toplevel only *)

open Cmo_format

(* Loading files *)

exception Load_failed

let rec load_file recursive ppf name =
  let filename =
    try Some (Load_path.find name) with Not_found -> None
  in
  match filename with
  | None -> fprintf ppf "Cannot find file %s.@." name; false
  | Some filename ->
      let ic = open_in_bin filename in
      Misc.try_finally
        ~always:(fun () -> close_in ic)
        (fun () -> really_load_file recursive ppf name filename ic)

and really_load_file recursive ppf name filename ic =
  let buffer = really_input_string ic (String.length Config.cmo_magic_number) in
  try
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu : compilation_unit = input_value ic in
      if recursive then
        List.iter
          (fun (reloc, _) -> match reloc with
            | Reloc_getcompunit cu
              when not (Symtable.is_global_defined
                (Symtable.Global.Glob_compunit cu)) ->
                let file = (Symtable.Compunit.name cu) ^ ".cmo" in
                begin match Load_path.find_normalized file with
                | exception Not_found -> ()
                | file ->
                    if not (load_file recursive ppf file) then raise Load_failed
                end
            | Reloc_getcompunit _
            | Reloc_literal _ | Reloc_getpredef _ | Reloc_setcompunit _
            | Reloc_primitive _ -> ()
          )
          cu.cu_reloc;
      try Dynlink.loadfile filename; true
      with Dynlink.Error (Dynlink.Library's_module_initializers_failed x) ->
        print_exception_outcome ppf x; false
    end else
      if buffer = Config.cma_magic_number then
        try Dynlink.loadfile filename; true
        with
          | Dynlink.Error (Cannot_open_dynamic_library (name, Failure msg)) ->
              fprintf ppf "Cannot load required shared library %s.@.\
                           Reason: %s.@." name msg;
              false
          | Dynlink.Error (Library's_module_initializers_failed x) ->
              print_exception_outcome ppf x;
              false
      else begin
        fprintf ppf "File %s is not a bytecode object file.@." name;
        false
      end
  with Load_failed -> false

let () =
  Location.register_error_of_exn
    (function
      | Dynlink.Error(Linking_error(_, err)) ->
          let err = match err with
          | Dynlink.Undefined_global (Compilation_unit cu) ->
              Symtable.Undefined_global
                (Symtable.Global.Glob_compunit (Cmo_format.Compunit cu))
          | Dynlink.Undefined_global (Predefined_exception exn) ->
              Symtable.Undefined_global
                (Symtable.Global.Glob_predef(Cmo_format.Predef_exn exn))
          | Dynlink.Unavailable_primitive s ->
              Symtable.Unavailable_primitive s
          | Dynlink.Uninitialized_global s ->
              Symtable.Uninitialized_global
                (Symtable.Global.Glob_compunit(Cmo_format.Compunit s))
          in
          Some (Location.error_of_printer_file Symtable.report_error_doc err)
      | _ -> None
    )

let init () =
  (* This call must precede the call to Symtable.init_toplevel - Dynlink must be
     initialised before the bytecode compiler. *)
  Dynlink.allow_unsafe_modules true;
  let crc_intfs = Symtable.init_toplevel() in
  Compmisc.init_path ();
  Env.import_crcs ~source:Sys.executable_name crc_intfs;
  ()
