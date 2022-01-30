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

let implementation_label = "native toplevel"

let global_symbol id =
  let sym = Compilenv.symbol_for_global id in
  match Tophooks.lookup sym with
  | None ->
    fatal_error ("Toploop.global_symbol " ^ (Ident.unique_name id))
  | Some obj -> obj

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
      Lprim (Pfield (pos, Pointer, Mutable),
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

let load_lambda ppf ~module_ident ~required_globals phrase_name lam size =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;

  let program =
    { Lambda.
      code = slam;
      main_module_block_size = size;
      module_ident;
      required_globals;
    }
  in
  Tophooks.load ppf phrase_name program

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* Execute a toplevel phrase *)

let phrase_seqid = ref 0

let name_expression ~loc ~attrs exp =
  let name = "_$" in
  let id = Ident.create_local name in
  let vd =
    { val_type = exp.exp_type;
      val_kind = Val_reg;
      val_loc = loc;
      val_attributes = attrs;
      val_uid = Uid.internal_not_actually_unique; }
   in
   let sg = [Sig_value(id, vd, Exported)] in
   let pat =
     { pat_desc = Tpat_var(id, mknoloc name);
       pat_loc = loc;
       pat_extra = [];
       pat_type = exp.exp_type;
       pat_env = exp.exp_env;
       pat_attributes = []; }
   in
   let vb =
     { vb_pat = pat;
       vb_expr = exp;
       vb_attributes = attrs;
       vb_loc = loc; }
   in
   let item =
     { str_desc = Tstr_value(Nonrecursive, [vb]);
       str_loc = loc;
       str_env = exp.exp_env; }
   in
   let final_env = Env.add_value id vd exp.exp_env in
   let str =
     { str_items = [item];
       str_type = sg;
       str_final_env = final_env }
   in
   str, sg

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      incr phrase_seqid;
      let phrase_name = "TOP" ^ string_of_int !phrase_seqid in
      Compilenv.reset ?packname:None phrase_name;
      Typecore.reset_delayed_checks ();
      let (str, sg, names, shape, newenv) =
        Typemod.type_toplevel_phrase oldenv sstr
      in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      ignore (Includemod.signatures oldenv ~mark:Mark_positive sg sg');
      Typecore.force_delayed_checks ();
      let shape = Shape.local_reduce shape in
      if !Clflags.dump_shape then Shape.print ppf shape;
      (* `let _ = <expression>` or even just `<expression>` require special
         handling in toplevels, or nothing is displayed. In bytecode, the
         lambda for <expression> is directly executed and the result _is_ the
         value. In native, the lambda for <expression> is compiled and loaded
         from a DLL, and the result of loading that DLL is _not_ the value
         itself. In native, <expression> must therefore be named so that it can
         be looked up after the DLL has been dlopen'd.

         The expression is "named" after typing in order to ensure that both
         bytecode and native toplevels always type-check _exactly_ the same
         expression. Adding the binding at the parsetree level (before typing)
         can create observable differences (e.g. in type variable names, see
         tool-toplevel/topeval.ml in the testsuite) *)
      let str, sg', rewritten =
         match find_eval_phrase str with
         | Some (e, attrs, loc) ->
             let str, sg' = name_expression ~loc ~attrs e in
             str, sg', true
         | None -> str, sg', false
      in
      let module_ident, res, required_globals, size =
        if Config.flambda then
          let { Lambda.module_ident; main_module_block_size = size;
                required_globals; code = res } =
            Translmod.transl_implementation_flambda phrase_name
              (str, Tcoerce_none)
          in
          remember module_ident 0 sg';
          module_ident, close_phrase res, required_globals, size
        else
          let size, res = Translmod.transl_store_phrases phrase_name str in
          Ident.create_persistent phrase_name, res, Ident.Set.empty, size
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res =
          load_lambda ppf ~required_globals ~module_ident phrase_name res size
        in
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
      try_run_directive ppf dir_name pdir_arg


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
