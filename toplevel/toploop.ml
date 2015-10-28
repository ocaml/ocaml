(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The interactive toplevel loop *)

open Config

include Toploop_shared

(* The table of toplevel value bindings and its accessors *)

module StringMap = Misc.StringMap

let toplevel_value_bindings : Obj.t StringMap.t ref = ref StringMap.empty

let getvalue name =
  try
    StringMap.find name !toplevel_value_bindings
  with Not_found ->
    Misc.fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := StringMap.add name v !toplevel_value_bindings

(* Return the value referred to by a path *)

let () =
  let rec eval_path =
    let open Path in
    function
    | Pident id ->
        if Ident.persistent id || Ident.global id then
          Symtable.get_global_value id
        else begin
          let name = Translmod.toplevel_name id in
          try
            StringMap.find name !toplevel_value_bindings
          with Not_found ->
            raise (Symtable.Error(Symtable.Undefined_global name))
        end
    | Pdot(p, s, pos) ->
        Obj.field (eval_path p) pos
    | Papply(p1, p2) ->
        Misc.fatal_error "Toploop.eval_path"
  in
  set_eval_path eval_path


(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)

let backtrace = ref None

let record_backtrace () =
  if Printexc.backtrace_status ()
  then backtrace := Some (Printexc.get_backtrace ())

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then Format.fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then Format.fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    Format.fprintf ppf "%a%a@."
    Printinstr.instrlist init_code
    Printinstr.instrlist fun_code;
  let (code, code_size, reloc, events) =
    Emitcode.to_memory init_code fun_code
  in
  Meta.add_debug_info code code_size [| events |];
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  let initial_bindings = !toplevel_value_bindings in
  try
    may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    may_trace := false;
    if can_free then begin
      Meta.remove_debug_info code;
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Ok retval
  with x ->
    may_trace := false;
    record_backtrace ();
    if can_free then begin
      Meta.remove_debug_info code;
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    toplevel_value_bindings := initial_bindings; (* PR#6211 *)
    Symtable.restore_state initial_symtable;
    Error x

(* Print the outcome of an evaluation *)

let pr_item = pr_item (fun id -> getvalue (Translmod.toplevel_name id))

let print_backtrace ppf =
  if Printexc.backtrace_status () then
    match !backtrace with
    | None -> ()
    | Some b ->
        Format.pp_print_string ppf b;
        Format.pp_print_flush ppf ();
        backtrace := None

(* Print an exception produced by an evaluation *)

let print_exception_outcome ppf exn =
  print_exception_outcome ppf exn;
  print_backtrace ppf

(* Execute a toplevel phrase *)

let () =
  set_execute_phrase (fun print_outcome ppf phr ->
    let open Parsetree in
    let open Outcometree in
    match phr with
    | Ptop_def sstr ->
        let oldenv = !toplevel_env in
        Typecore.reset_delayed_checks ();
        let (str, sg, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
        if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
        let sg' = Typemod.simplify_signature sg in
        ignore (Includemod.signatures oldenv sg sg');
        Typecore.force_delayed_checks ();
        let lam = Translmod.transl_toplevel_definition str in
        Warnings.check_fatal ();
        begin try
          toplevel_env := newenv;
          let res = load_lambda ppf lam in
          let out_phr =
            match res with
            | Ok v ->
                if print_outcome then
                  Printtyp.wrap_printing_env oldenv (fun () ->
                    let open Typedtree in
                    match str.str_items with
                    | [ { str_desc = Tstr_eval (exp, _attrs) }] ->
                        let outv = outval_of_value newenv v exp.exp_type in
                        let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                        Ophr_eval (outv, ty)
                    | [] -> Ophr_signature []
                    | _ -> Ophr_signature (pr_item newenv sg'))
                    else
                      Ophr_signature []
            | Error exn ->
                toplevel_env := oldenv;
                if exn = Out_of_memory then Gc.full_major();
                let outv =
                  outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
                in
                Ophr_exception (exn, outv)
          in
          !print_out_phrase ppf out_phr;
          print_backtrace ppf;
          begin match out_phr with
          | Ophr_eval (_, _) | Ophr_signature _ -> true
          | Ophr_exception _ -> false
          end
        with x ->
          toplevel_env := oldenv; raise x
        end
    | Ptop_dir(dir_name, dir_arg) ->
        execute_topdir ppf dir_name dir_arg
  )

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Clflags.debug := true;
  Sys.interactive := true;
  let crc_intfs = Symtable.init_toplevel() in
  Compmisc.init_path false;
  List.iter
    (fun (name, crco) ->
      Env.add_import name;
      match crco with
        None -> ()
      | Some crc->
          Consistbl.set Env.crc_units name crc Sys.executable_name)
    crc_intfs

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  Dll.add_path !load_path

(* The interactive loop *)

let loop ppf =
  Location.formatter_for_warnings := ppf;
  Format.fprintf ppf "        OCaml version %s@.@." Config.version;
  loop_no_header ppf
