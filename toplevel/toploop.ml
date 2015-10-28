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

open Path
open Format
open Config
open Misc
open Parsetree
open Typedtree
open Outcometree

include Toploop_shared
(* The table of toplevel value bindings and its accessors *)

module StringMap = Map.Make(String)

let toplevel_value_bindings : Obj.t StringMap.t ref = ref StringMap.empty

let getvalue name =
  try
    StringMap.find name !toplevel_value_bindings
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := StringMap.add name v !toplevel_value_bindings

(* Return the value referred to by a path *)

let () =
  let rec eval_path = function
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
        fatal_error "Toploop.eval_path"
  in
  set_eval_path eval_path


(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)
type evaluation_outcome = Result of Obj.t | Exception of exn

let backtrace = ref None

let record_backtrace () =
  if Printexc.backtrace_status ()
  then backtrace := Some (Printexc.get_backtrace ())

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a%a@."
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
    Result retval
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
    Exception x

(* Print the outcome of an evaluation *)

let pr_item = pr_item (fun id -> getvalue (Translmod.toplevel_name id))

(* Print an exception produced by an evaluation *)

let print_exception_outcome ppf exn =
  print_exception_outcome ppf exn;
  if Printexc.backtrace_status ()
  then
    match !backtrace with
      | None -> ()
      | Some b ->
          print_string b;
          backtrace := None

(* Execute a toplevel phrase *)

let () =
  set_execute_phrase (fun print_outcome ppf phr ->
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
            | Result v ->
                if print_outcome then
                  Printtyp.wrap_printing_env oldenv (fun () ->
                    match str.str_items with
                    | [ { str_desc = Tstr_eval (exp, _attrs) }] ->
                        let outv = outval_of_value newenv v exp.exp_type in
                        let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                        Ophr_eval (outv, ty)
                    | [] -> Ophr_signature []
                    | _ -> Ophr_signature (pr_item newenv sg'))
                else Ophr_signature []
            | Exception exn ->
                toplevel_env := oldenv;
                if exn = Out_of_memory then Gc.full_major();
                let outv =
                  outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
                in
                Ophr_exception (exn, outv)
          in
          !print_out_phrase ppf out_phr;
          if Printexc.backtrace_status ()
          then begin
            match !backtrace with
              | None -> ()
              | Some b ->
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
    | Ptop_dir(dir_name, dir_arg) ->
        let d =
          try Some (Hashtbl.find directive_table dir_name)
          with Not_found -> None
        in
        begin match d with
        | None ->
            fprintf ppf "Unknown directive `%s'.@." dir_name;
            false
        | Some d ->
            match d, dir_arg with
            | Directive_none f, Pdir_none -> f (); true
            | Directive_string f, Pdir_string s -> f s; true
            | Directive_int f, Pdir_int n -> f n; true
            | Directive_ident f, Pdir_ident lid -> f lid; true
            | Directive_bool f, Pdir_bool b -> f b; true
            | _ ->
                fprintf ppf "Wrong type of argument for directive `%s'.@."
                  dir_name;
                false
        end
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
  fprintf ppf "        OCaml version %s@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done
