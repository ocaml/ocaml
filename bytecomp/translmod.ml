(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Typedtree
open Lambda
open Translcore


(* Compile a coercion *)

let rec apply_coercion restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure pos_cc_list ->
      name_lambda arg (fun id ->
        Lprim(Pmakeblock 0, List.map (apply_coercion_field id) pos_cc_list))
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.new "funarg" in
      name_lambda arg (fun id ->
        Lfunction(param,
          apply_coercion cc_res
            (Lapply(Lvar id, [apply_coercion cc_arg (Lvar param)]))))

and apply_coercion_field id (pos, cc) =
  apply_coercion cc (Lprim(Pfield pos, [Lvar id]))

(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure pc1, Tcoerce_structure pc2) ->
      let v2 = Array.of_list pc2 in
      Tcoerce_structure
        (List.map (fun (p1, c1) ->
                let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(* Compile a module expression *)

let rec transl_module env cc mexp =
  match mexp.mod_desc with
    Tmod_ident path ->
      apply_coercion cc (transl_path path)
  | Tmod_structure str ->
      transl_structure env [] cc str
  | Tmod_functor(param, mty, body) ->
      begin match cc with
        Tcoerce_none ->
          Lfunction(param, transl_module env Tcoerce_none body)
      | Tcoerce_functor(ccarg, ccres) ->
          let param' = Ident.new "funarg" in
          Lfunction(param',
            Llet(param, apply_coercion ccarg (Lvar param'),
              transl_module env ccres body))
      | Tcoerce_structure _ ->
          fatal_error "Translmod.transl_module"
      end
  | Tmod_apply(funct, arg, ccarg) ->
      apply_coercion cc
        (Lapply(transl_module env Tcoerce_none funct,
                [transl_module env ccarg arg]))
  | Tmod_constraint(arg, mty, ccarg) ->
      transl_module env (compose_coercions cc ccarg) arg

and transl_structure env fields cc = function
    [] ->
      begin match cc with
        Tcoerce_none ->
          Lprim(Pmakeblock 0,
                List.map (fun id -> transl_access env id) (List.rev fields))
      | Tcoerce_structure pos_cc_list ->
          let v = Array.of_list (List.rev fields) in
          Lprim(Pmakeblock 0,
                List.map (fun (pos, cc) ->
                        apply_coercion cc (transl_access env v.(pos)))
                    pos_cc_list)
      | Tcoerce_functor(_, _) ->
          fatal_error "Translmod.transl_structure"
      end
  | Tstr_eval expr :: rem ->
      Lsequence(transl_exp env expr, transl_structure env fields cc rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ext_fields = let_bound_idents pat_expr_list @ fields in
      let (ext_env, add_let) = transl_let env rec_flag pat_expr_list in
      add_let(transl_structure ext_env ext_fields cc rem)
  | Tstr_primitive(id, descr) :: rem ->
      Llet(id, transl_primitive descr.val_prim,
           transl_structure env (id :: fields) cc rem)
  | Tstr_type(decls) :: rem ->
      transl_structure env fields cc rem
  | Tstr_exception(id, decl) :: rem ->
      Llet(id, transl_exception id decl,
           transl_structure env (id :: fields) cc rem)
  | Tstr_module(id, modl) :: rem ->
      Llet(id, transl_module env Tcoerce_none modl,
           transl_structure env (id :: fields) cc rem)
  | Tstr_modtype(id, decl) :: rem ->
      transl_structure env fields cc rem
  | Tstr_open path :: rem ->
      transl_structure env fields cc rem

(* Compile an implementation *)

let transl_implementation module_name str cc =
  let module_id = Ident.new_persistent module_name in
  Lprim(Psetglobal module_id, [transl_structure empty_env [] cc str])

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem -> Lsequence(fn x, make_sequence fn rem)

(* Compile a toplevel phrase *)

let transl_toplevel_item = function
    Tstr_eval expr ->
      transl_exp empty_env expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      let (env, add_lets) = transl_let empty_env rec_flag pat_expr_list in
      let lam =
        add_lets(make_sequence
                  (fun id -> Lprim(Psetglobal id, [transl_access env id]))
                  idents) in
      List.iter Ident.make_global idents;
      lam
  | Tstr_primitive(id, descr) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_primitive descr.val_prim])
  | Tstr_type(decls) ->
      lambda_unit
  | Tstr_exception(id, decl) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_exception id decl])
  | Tstr_module(id, modl) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_module empty_env Tcoerce_none modl])
  | Tstr_modtype(id, decl) ->
      lambda_unit
  | Tstr_open path ->
      lambda_unit

let transl_toplevel_definition str =
  make_sequence transl_toplevel_item str
