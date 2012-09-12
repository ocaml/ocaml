open Misc
open Asttypes
open Longident
open Path
open Types
open Typedtree
open Primitive
open Lambda
open Translobj
open Translcore
open Translclass

let synonym id    = Ident.rename id
let wrapped_id id = Ident.create ((Path.name id) ^ "__wrapped")

let bolt_id   = Ident.create "Bolt"
let logger_id = Ident.create "Logger"
let level_id  = Ident.create "Level"
let log_id    = Ident.create "log"
let trace_id  = Ident.create "TRACE"
let daikon_id = Ident.create "Daikon"
let t_id      = Ident.create "t"

let make_string env s =
  { exp_desc = Texp_constant (Const_string s);
    exp_loc  = Location.none;
    exp_type = Predef.type_string;
    exp_env  = env }

let make_int env i =
  { exp_desc = Texp_constant (Const_int i);
    exp_loc  = Location.none;
    exp_type = Predef.type_int;
    exp_env  = env }

let make_ident env id typ =
  { exp_desc = Texp_ident (Pident id, { val_type = typ; val_kind = Val_reg });
    exp_loc  = Location.none;
    exp_type = typ;
    exp_env  = env }


let string_couple = (*XXX newty ?*)
  { desc  = Ttuple [Predef.type_string; Predef.type_string];
    level = 9999; (*XXX*)
    id    = 9999 } (*XXX*)

(* XXX reifier les types (p. ex. 'a ~> int) *)
(* XXX Bo/Daikon: gérer les 'a option par liste ? *)
(* XXX Bo/Daikon: gérer les char par string of length 1 ? *)
(* XXX Bo/Daikon: quid int32/int64/nativeint ? *)

let rec daikon_type top typ =
  match typ.desc with
  | Tconstr (path, [], _) ->
      begin
        try
          let res_type, res_path, res_name =
            List.find
            (fun (_, p, _) -> Path.same path p)
              [ Predef.type_bool, Predef.path_bool, "bool";
                Predef.type_int, Predef.path_int, "int";
                Predef.type_float, Predef.path_float, "float";
                Predef.type_string, Predef.path_string, "string" ] in
          res_type, res_name
        with Not_found ->
          Misc.fatal_error ("unhandled data type: " ^ (Path.name path))
      end
  | Tconstr (path, [t], _) when top ->
      begin
        try
          let res_type, res_path, res_name =
            List.find
              (fun (_, p, _) -> Path.same path p)
              [ Predef.type_list, Predef.path_list, "_list";
                Predef.type_array, Predef.path_array, "_array" ] in
          let ty, nm = daikon_type false t in
          (res_type ty), (nm ^ res_name)
        with Not_found ->
          Misc.fatal_error ("unhandled data type: " ^ (Path.name path))
      end
  | _ ->
      Misc.fatal_error "unhandled data type"

let daikon_variable env id exp =
  let typ = (Env.find_value (Pident id) exp.exp_env).val_type in
  let daikon_typ, daikon_nam = daikon_type true typ in
  let call =
    { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Daikon", 0), daikon_nam, 0),
                             { val_type = daikon_typ; val_kind = Val_reg }); (*XXX*)
      exp_loc  = Location.none;
      exp_type = daikon_typ;  (*XXX*)
      exp_env  = env } in
  let name_expr = make_string env (Ident.name id) in
  let id_expr   = make_ident env id typ in
  { exp_desc = Texp_apply (call, [Some name_expr, Required;
                                  Some id_expr, Required]);
    exp_loc  = Location.none;
    exp_type = Predef.type_unit;
    exp_env  = env }

let string_couple_list = Predef.type_list string_couple

let none = Env.lookup_constructor_and_path (Longident.Lident "None") Env.initial

let cons = Env.lookup_constructor_and_path (Longident.Lident "::") Env.initial

let nil = Env.lookup_constructor_and_path (Longident.Lident "[]") Env.initial

let nil_expr env typ =
  { exp_desc = Texp_construct (fst nil, snd nil, []);
    exp_loc  = Location.none;
    exp_type = Predef.type_list typ;
    exp_env  = env }

let bolt_log loc name env id exp ret =
  let typ = (Env.find_value (Pident id) exp.exp_env).val_type in
(*XXX  let typ = exp.exp_type in*)
  let file, line, column = Location.get_pos_info (loc.Location.loc_start) in
  let mdl = Filename.chop_extension file in
  let mdl = Filename.basename mdl in
  let mdl = String.capitalize mdl in
  let module_expr = make_string env mdl in
  let level_type = Predef.type_int in (*XXX faire un lookup, à l'exterieur de la fonction*)
  let level_expr =
    { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Logger", 1), "TRACE", 0),
                             { val_type = level_type; val_kind = Val_reg });
      exp_loc  = Location.none;
      exp_type = level_type;
      exp_env  = env } in
  let file_expr = make_string env file in
  let line_expr = make_int env line in
  let column_expr = make_int env column in
  let variable = daikon_variable env id exp in
  let variable_type = Predef.type_int in (*XXX Bolt.Daikon.variable*)
  let params_expr =
    { exp_desc = Texp_construct (fst cons, snd cons, [variable; nil_expr env typ]);
      exp_loc  = Location.none;
      exp_type = Predef.type_list variable_type;
      exp_env  = env } in
  let properties_expr = match ret with
  | Some (ret_id, ret_exp) ->
      let func =
        { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Daikon", 0), "exit", 0),
                     { val_type = Predef.type_unit; val_kind = Val_reg }); (*XXX*)
          exp_loc  = Location.none;
          exp_type = Predef.type_unit; (*XXX*)
          exp_env  = env } in
      { exp_desc = Texp_apply (func, [Some (make_string env name), Required;
                                      Some (daikon_variable env ret_id ret_exp), Required;
                                      Some params_expr, Required]);
        exp_loc = Location.none;
        exp_type = Predef.type_unit; (*XXX Bolt.Daikon.properties*)
        exp_env = env }
  | None ->
      let func =
        { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Daikon", 0), "enter", 0),
                      { val_type = Predef.type_unit; val_kind = Val_reg }); (*XXX*)
          exp_loc  = Location.none;
          exp_type = Predef.type_unit; (*XXX*)
          exp_env  = env } in
      { exp_desc = Texp_apply (func, [Some (make_string env name), Required;
                                      Some params_expr, Required]);
        exp_loc  = Location.none;
        exp_type = Predef.type_unit; (*XXX Bolt.Daikon.properties*)
        exp_env  = env } in
  let error_expr =
    { exp_desc = Texp_construct (fst none, snd none, []);
      exp_loc  = Location.none;
      exp_type = Predef.type_option Predef.type_exn;
      exp_env  = env } in
  let daikon_t_type = Predef.type_string in
  let daikon_t_expr =
    { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Daikon", 0), "t", 0),
                             { val_type = daikon_t_type; val_kind = Val_reg });
      exp_loc  = Location.none;
      exp_type = daikon_t_type;
      exp_env  = env } in
  let call =
    { exp_desc = Texp_ident (Pdot (Pdot (Pident bolt_id, "Logger", 0), "log", 0),
                     { val_type = Predef.type_unit; val_kind = Val_reg }); (*XXX*)
      exp_loc  = Location.none;
      exp_type = Predef.type_unit; (*XXX*)
      exp_env  = env } in
  { exp_desc = Texp_apply (call, [Some module_expr, Required;
                                  Some level_expr, Required;
                                  Some file_expr, Optional;
                                  Some line_expr, Optional;
                                  Some column_expr, Optional;
                                  Some properties_expr, Optional;
                                  Some error_expr, Optional;
                                  Some daikon_t_expr, Required]);
    exp_loc = Location.none;
    exp_type = Predef.type_unit;
    exp_env = env }
  

let rec extract acc = function
  | Tctr_arrow (_, left, right) ->
      (match left.contract_desc with
      | Tctr_pred (id, exp, _) ->
          extract ((id, exp) :: acc) right.contract_desc
      | _ -> assert false) (* XXX *)
  | Tctr_pred (id, exp, _) ->
      acc, (id, exp)
  | _ -> assert false (* XXX *)

let pattern_of_param env (id, exp) =
  { pat_desc = Tpat_var id;
    pat_loc = Location.none;
    pat_type = exp.exp_type;
    pat_env = env }

let type_expr_of_param (_, exp) =
  exp.exp_type

(* Typedtree.contract_declaration -> Typedtree.structure_item *)
let transl_str_contract c =
  let env = c.ttopctr_desc.contract_env in
  (try
    (* XXX le faire à l'initialisation du module, avec une erreur "propre" *)
    ignore ()(*Env.lookup_module (Lident (Ident.name bolt_id)) env*) (*XXX*)
  with e ->
    prerr_endline (Printexc.to_string e);
    Misc.fatal_error "should add Bolt to search path");
  match c.ttopctr_desc.contract_desc with
  | (Tctr_arrow _) as x ->
      let params, (ret_id, ret_exp) = extract [] x in
      let patt = { pat_desc = Tpat_var (wrapped_id c.ttopctr_id);
                   pat_loc = c.ttopctr_loc;
                   pat_type = c.ttopctr_type;
                   pat_env = env } in
(*
      let params =
        { pat_desc = Tpat_tuple (List.map (pattern_of_param env) params);
          pat_loc = c.ttopctr_loc;
          pat_type = { desc = Ttuple (List.map type_expr_of_param params); level = 999; id = 999 }; (*XXX type_expr*)
          pat_env = env } in
*)
      let res_id = Ident.rename ret_id in
      let res_typ = (Env.find_value (Pident ret_id) ret_exp.exp_env).val_type in
      let env = Env.add_value res_id { val_type = res_typ; val_kind = Val_reg } env in
      let p = List.hd params in (*XXX*)
      let params =
        { pat_desc = Tpat_var (fst p);
          pat_loc = c.ttopctr_loc;
          pat_type = type_expr_of_param (p);
          pat_env = env } in
      let original =
        { exp_desc = Texp_ident (c.ttopctr_id,
                                 { val_type = c.ttopctr_type; val_kind = Val_reg});
          exp_loc = Location.none;
          exp_type = c.ttopctr_type;
          exp_env = env } in
      let value =
        { exp_desc = Texp_ident (Pident (fst p),
                                 { val_type = params.pat_type; val_kind = Val_reg});
          exp_loc = Location.none;
          exp_type = params.pat_type;
          exp_env = env } in
      let call = { exp_desc =
                   Texp_apply (original, [Some value, Required]);
                   exp_loc = c.ttopctr_loc;
                   exp_type = res_typ;
                   exp_env = env } in
      let res = { pat_desc = Tpat_var res_id;
                  pat_loc = Location.none;
                  pat_type = res_typ;
                  pat_env = env } in
      let return_res = { exp_desc = Texp_ident (Pident res_id, {val_type = res.pat_type; val_kind = Val_reg});
                         exp_loc = Location.none;
                         exp_type = res.pat_type;
                         exp_env = env } in
      let exit = bolt_log c.ttopctr_loc (Path.name c.ttopctr_id) env (fst p) (snd p) (Some (res_id, return_res)) in
      let return_res = { exp_desc = Texp_sequence (exit, return_res);
                         exp_loc = Location.none;
                         exp_type = res.pat_type;
                         exp_env = env } in
      let body = { exp_desc =
                   Texp_let (Nonrecursive, [res, call], return_res);
                   exp_loc = Location.none;
                   exp_type = res.pat_type;
                   exp_env = env } in
      let enter = bolt_log c.ttopctr_loc (Path.name c.ttopctr_id) env (fst p) (snd p) None in
      let seq = { exp_desc = Texp_sequence (enter, body);
                   exp_loc = Location.none;
                   exp_type = res.pat_type;
                   exp_env = env } in
      let expr = { exp_desc = Texp_function ([params, seq], Total);
                   exp_loc = c.ttopctr_loc;
                   exp_type = res.pat_type;
                   exp_env = env } in
      [Tstr_value (Nonrecursive, [patt, expr])]
  | _ -> []

(* Typedtree.contract_declaration list -> Typedtree.structure_item list *)
let transl_str_contracts l =
  List.flatten (List.map transl_str_contract l)

(* copied from "bytecomp/translmod.ml" *)
type module_coercion = Types.module_coercion
let transl_contracts (str, cc) =
 let rec extract_contracts xs = 
        match xs with
           | [] -> ([], Ident.empty)		 
           | (Tstr_mty_contracts(t) :: rem) ->
               let (current_contracts, mty_opened_contracts) = extract_contracts rem in 
               (current_contracts, Ident.merge t mty_opened_contracts)
	   | (Tstr_opened_contracts(t) :: rem) ->
	       let (current_contracts, mty_opened_contracts) = extract_contracts rem in
	       (current_contracts, Ident.merge t mty_opened_contracts) 
           | ((Tstr_contract (ds)) :: rem) -> 
	       let (current_contracts, mty_opened_contracts) = extract_contracts rem in	       
	       (ds@current_contracts, mty_opened_contracts)
           | (_::rem) -> extract_contracts rem
  in
  let (tstr_contracts, mty_opened_contracts) = extract_contracts str in
  let checked_tstr_contracts = List.map (fun c -> 
                 let new_c_desc = Verify.contract_id_in_contract 
                                    (ThmEnv.initEnv
                                    c.ttopctr_desc in
                 {c with ttopctr_desc = new_c_desc}) tstr_contracts in
  ignore (mty_opened_contracts); (* XXX *) (* included by 'open' constructs / inner- outer- modules *)
  (str @ (transl_str_contracts checked_tstr_contracts), cc)
