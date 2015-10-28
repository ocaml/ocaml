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

let _dummy = (Ok (Obj.magic 0), Error "")

external ndl_run_toplevel: string -> string -> (Obj.t, string) result
  = "caml_natdynlink_run_toplevel"
external ndl_loadsym: string -> Obj.t = "caml_natdynlink_loadsym"

let global_symbol id =
  let sym = Compilenv.symbol_for_global id in
  try ndl_loadsym sym
  with _ -> Misc.fatal_error ("Opttoploop.global_symbol " ^ (Ident.unique_name id))

let need_symbol sym =
  try ignore (ndl_loadsym sym); false
  with _ -> true

let dll_run dll entry =
  match ndl_run_toplevel dll entry with
  | exception exn -> Error exn
  | Ok x -> Ok x
  | Error s -> Misc.fatal_error ("Opttoploop.dll_run " ^ s)


(* Return the value referred to by a path *)

let toplevel_value id =
  let (glb,pos) = Translmod.nat_toplevel_name id in
  (Obj.magic (global_symbol glb)).(pos)

let () =
  let rec eval_path =
    let open Path in
    function
    | Pident id ->
        if Ident.persistent id || Ident.global id
        then global_symbol id
        else toplevel_value id
    | Pdot(p, s, pos) ->
        Obj.field (eval_path p) pos
    | Papply(p1, p2) ->
        Misc.fatal_error "Toploop.eval_path"
  in
  set_eval_path eval_path


(* Load in-core and execute a lambda term *)

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

let load_lambda ppf (size, lam) =
  if !Clflags.dump_rawlambda then Format.fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then Format.fprintf ppf "%a@." Printlambda.lambda slam;

  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let fn = Filename.chop_extension dll in
  Asmgen.compile_implementation ~toplevel:need_symbol fn ppf (size, slam);
  Asmlink.call_linker_shared [fn ^ ext_obj] dll;
  Sys.remove (fn ^ ext_obj);

  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  let res = dll_run dll !phrase_name in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

(* Print the outcome of an evaluation *)

let pr_item = pr_item toplevel_value

(* Execute a toplevel phrase *)

let () =
  set_execute_phrase (fun print_outcome ppf phr ->
    let open Parsetree in
    let open Outcometree in
    match phr with
    | Ptop_def sstr ->
        let oldenv = !toplevel_env in
        incr phrase_seqid;
        phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
        Compilenv.reset ?packname:None !phrase_name;
        Typecore.reset_delayed_checks ();
        let (str, sg, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
        if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
        let sg' = Typemod.simplify_signature sg in
        ignore (Includemod.signatures oldenv sg sg');
        Typecore.force_delayed_checks ();
        let lam = Translmod.transl_store_phrases !phrase_name str in
        Warnings.check_fatal ();
        begin try
          toplevel_env := newenv;
          let res = load_lambda ppf lam in
          let out_phr =
            match res with
            | Ok v ->
                Compilenv.record_global_approx_toplevel ();
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
  Sys.interactive := true;
  Dynlink.init ();
  Compmisc.init_path true;
  Clflags.dlcode := true;
  ()

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  ()

(* The interactive loop *)

let loop ppf =
  Location.formatter_for_warnings := ppf;
  Format.fprintf ppf "        OCaml version %s - native toplevel@.@." Config.version;
  loop_no_header ppf
