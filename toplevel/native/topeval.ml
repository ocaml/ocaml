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
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Topcommon

type res = Ok of Obj.t | Err of string
type evaluation_outcome = Result of Obj.t | Exception of exn

let _dummy = (Ok (Obj.magic 0), Err "")

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"

let implementation_label = "native toplevel"

let global_symbol id =
  let sym = Compilenv.symbol_for_global id in
  match Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym with
  | None ->
    fatal_error ("Toploop.global_symbol " ^ (Ident.unique_name id))
  | Some obj -> obj

let need_symbol sym =
  Option.is_none (Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym)

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
         with exn -> Exception exn)
  with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Toploop.dll_run " ^ s)


let remembered = ref Ident.empty

let rec remember phrase_name i = function
  | [] -> ()
  | Sig_value  (id, _, _) :: rest
  | Sig_module (id, _, _, _, _) :: rest
  | Sig_typext (id, _, _, _) :: rest
  | Sig_class  (id, _, _, _) :: rest ->
      remembered := Ident.add id (phrase_name, i) !remembered;
      remember phrase_name (succ i) rest
  | _ :: rest -> remember phrase_name i rest

let toplevel_value id =
  try Ident.find_same id !remembered
  with _ -> Misc.fatal_error @@ "Unknown ident: " ^ Ident.unique_name id

let close_phrase lam =
  let open Lambda in
  Ident.Set.fold (fun id l ->
    let glb, pos = toplevel_value id in
    let glob =
      Lprim (Pfield pos,
             [Lprim (Pgetglobal glb, [], Loc_unknown)],
             Loc_unknown)
    in
    Llet(Strict, Pgenval, id, glob, l)
  ) (free_variables lam) lam

let toplevel_value id =
  let glob, pos =
    if Config.flambda then toplevel_value id else Translmod.nat_toplevel_name id
  in
  (Obj.magic (global_symbol glob)).(pos)

(* Return the value referred to by a path *)

module EvalBase = struct

  let eval_ident id =
    try
      if Ident.persistent id || Ident.global id
      then global_symbol id
      else toplevel_value id
    with _ ->
      raise (Undefined_global (Ident.name id))

end

include Topcommon.MakeEvalPrinter(EvalBase)

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

(* CR-soon trefis for mshinwell: copy/pasted from Optmain. Should it be shared
   or?
   mshinwell: It should be shared, but after 4.03. *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let load_lambda ppf ~module_ident ~required_globals lam size =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;

  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  let program =
    { Lambda.
      code = slam;
      main_module_block_size = size;
      module_ident;
      required_globals;
    }
  in
  let middle_end =
    if Config.flambda then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda
  in
  Asmgen.compile_implementation ~toplevel:need_symbol
    ~backend ~filename ~prefixname:filename
    ~middle_end ~ppf_dump:ppf program;
  Asmlink.call_linker_shared [filename ^ ext_obj] dll;
  Sys.remove (filename ^ ext_obj);

  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  match
    Fun.protect
      ~finally:(fun () ->
          (try Sys.remove dll with Sys_error _ -> ()))
            (* note: under windows, cannot remove a loaded dll
               (should remember the handles, close them in at_exit, and then
               remove files) *)
      (fun () -> dll_run dll !phrase_name)
  with
  | res -> res
  | exception x ->
      record_backtrace ();
      Exception x

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      incr phrase_seqid;
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      Compilenv.reset ?packname:None !phrase_name;
      Typecore.reset_delayed_checks ();
      let sstr, rewritten =
        match sstr with
        | [ { pstr_desc = Pstr_eval (e, attrs) ; pstr_loc = loc } ]
        | [ { pstr_desc = Pstr_value (Asttypes.Nonrecursive,
                                      [{ pvb_expr = e
                                       ; pvb_pat = { ppat_desc = Ppat_any ; _ }
                                       ; pvb_attributes = attrs
                                       ; _ }])
            ; pstr_loc = loc }
          ] ->
            let pat = Ast_helper.Pat.var (Location.mknoloc "_$") in
            let vb = Ast_helper.Vb.mk ~loc ~attrs pat e in
            [ Ast_helper.Str.value ~loc Asttypes.Nonrecursive [vb] ], true
        | _ -> sstr, false
      in
      let (str, sg, names, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      ignore (Includemod.signatures oldenv ~mark:Mark_positive sg sg');
      Typecore.force_delayed_checks ();
      let module_ident, res, required_globals, size =
        if Config.flambda then
          let { Lambda.module_ident; main_module_block_size = size;
                required_globals; code = res } =
            Translmod.transl_implementation_flambda !phrase_name
              (str, Tcoerce_none)
          in
          remember module_ident 0 sg';
          module_ident, close_phrase res, required_globals, size
        else
          let size, res = Translmod.transl_store_phrases !phrase_name str in
          Ident.create_persistent !phrase_name, res, Ident.Set.empty, size
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf ~required_globals ~module_ident res size in
        let out_phr =
          match res with
          | Result _ ->
              if Config.flambda then
                (* CR-someday trefis: *)
                Env.register_import_as_opaque (Ident.name module_ident)
              else
                Compilenv.record_global_approx_toplevel ();
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                match str.str_items with
                | [] -> Ophr_signature []
                | _ ->
                    if rewritten then
                      match sg' with
                      | [ Sig_value (id, vd, _) ] ->
                          let outv =
                            outval_of_value newenv (toplevel_value id)
                              vd.val_type
                          in
                          let ty = Printtyp.tree_of_type_scheme vd.val_type in
                          Ophr_eval (outv, ty)
                      | _ -> assert false
                    else
                      Ophr_signature (pr_item oldenv sg'))
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
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      begin match get_directive dir_name with
      | None ->
          fprintf ppf "Unknown directive `%s'.@." dir_name;
          false
      | Some d ->
          match d, pdir_arg with
          | Directive_none f, None -> f (); true
          | Directive_string f, Some {pdira_desc = Pdir_string s} -> f s; true
          | Directive_int f, Some {pdira_desc = Pdir_int (n,None)} ->
             begin match Int_literal_converter.int n with
             | n -> f n; true
             | exception _ ->
               fprintf ppf "Integer literal exceeds the range of \
                            representable integers for directive `%s'.@."
                       dir_name;
               false
             end
          | Directive_int _, Some {pdira_desc = Pdir_int (_, Some _)} ->
              fprintf ppf "Wrong integer literal for directive `%s'.@."
                dir_name;
              false
          | Directive_ident f, Some {pdira_desc = Pdir_ident lid} -> f lid; true
          | Directive_bool f, Some {pdira_desc = Pdir_bool b} -> f b; true
          | _ ->
              fprintf ppf "Wrong type of argument for directive `%s'.@."
                dir_name;
              false
      end


(* API compat *)

let getvalue _ = assert false
let setvalue _ _ = assert false

(* Loading files *)

(* Load in-core a .cmxs file *)

let load_file _ (* fixme *) ppf name0 =
  let name =
    try Some (Load_path.find name0)
    with Not_found -> None
  in
  match name with
  | None -> fprintf ppf "File not found: %s@." name0; false
  | Some name ->
    let fn,tmp =
      if Filename.check_suffix name ".cmx" || Filename.check_suffix name ".cmxa"
      then
        let cmxs = Filename.temp_file "caml" ".cmxs" in
        Asmlink.link_shared ~ppf_dump:ppf [name] cmxs;
        cmxs,true
      else
        name,false
    in
    let success =
      (* The Dynlink interface does not allow us to distinguish between
          a Dynlink.Error exceptions raised in the loaded modules
          or a genuine error during dynlink... *)
      try Dynlink.loadfile fn; true
      with
      | Dynlink.Error err ->
        fprintf ppf "Error while loading %s: %s.@."
          name (Dynlink.error_message err);
        false
      | exn ->
        print_exception_outcome ppf exn;
        false
    in
    if tmp then (try Sys.remove fn with Sys_error _ -> ());
    success

let init () =
  Compmisc.init_path ();
  Clflags.dlcode := true;
  ()
