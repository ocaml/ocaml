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

(* $Id$ *)

(* Abstract syntax tree after typing *)

open Misc
open Asttypes
open Types

(* Value expressions for the core language *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr;
    mutable pat_env: Env.t }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern list
  | Tpat_variant of label * pattern option * row_desc ref
  | Tpat_record of (label_description * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * row_desc option
  | Tpat_lazy of pattern

type partial = Partial | Total
type optional = Required | Optional

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr;
    exp_env: Env.t }

and expression_desc =
    Texp_ident of Path.t * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list * partial
  | Texp_apply of expression * (expression option * optional) list
  | Texp_match of expression * (pattern * expression) list * partial
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of (label_description * expression) list * expression option
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_send of expression * meth
  | Texp_new of Path.t * class_declaration
  | Texp_instvar of Path.t * Path.t
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list
  | Texp_letmodule of Ident.t * module_expr * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_lazy of expression
  | Texp_object of class_structure * class_signature * string list
  | Texp_pack of module_expr
(*> JOCAML *)
  | Texp_asyncsend of expression * expression
  | Texp_spawn of expression
  | Texp_par of expression * expression
  | Texp_null
  | Texp_reply of expression * Ident.t
  | Texp_def of joinautomaton list * expression
  | Texp_loc of joinlocation list * expression


and joinlocation =
    {jloc_desc : joinident * joinautomaton list * expression ;
      jloc_loc : Location.t}

and 'a joinautomaton_gen =
    {jauto_desc : 'a ;
     jauto_name : Ident.t * Ident.t ;
     jauto_names : (Ident.t * joinchannel) list ;
     jauto_original : Ident.t list ;
     jauto_nchans : int;
     (* names defined, description *)
     jauto_loc : Location.t}

and joinautomaton =
 (joindispatcher list * joinreaction list * joinforwarder list)
      joinautomaton_gen

and joindispatcher =
  Disp of
    Ident.t * joinchannel * (pattern * joinchannel) list * partial

and joinclause = 
   Ident.t * joinpattern list * joinpattern list list * 
  (Ident.t * pattern) list * expression

and joinreaction = Reac of joinclause

and joinforwarder = Fwd of joinclause

and joinchannel =
    {jchannel_sync : bool ;
     jchannel_id   : jchannel_id ;
     jchannel_ident : Ident.t ;
     jchannel_type : type_expr;
     jchannel_env : Env.t;}

and jchannel_id = Chan of Ident.t * int | Alone of Ident.t

and joinpattern =
    { jpat_desc: joinident * pattern ;
      jpat_kont : Ident.t option ref ;
      jpat_loc: Location.t}

and joinident =
    { jident_desc : Ident.t ;
      jident_loc  : Location.t;
      jident_type : type_expr;
      jident_env : Env.t;}

and joinarg =
    { jarg_desc : Ident.t option ;
      jarg_loc  : Location.t;
      jarg_type : type_expr;
      jarg_env : Env.t;}
(*< JOCAML *)

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

(* Value expressions for the class language *)

and class_expr =
  { cl_desc: class_expr_desc;
    cl_loc: Location.t;
    cl_type: class_type;
    cl_env: Env.t }

and class_expr_desc =
    Tclass_ident of Path.t
  | Tclass_structure of class_structure
  | Tclass_fun of pattern * (Ident.t * expression) list * class_expr * partial
  | Tclass_apply of class_expr * (expression option * optional) list
  | Tclass_let of rec_flag *  (pattern * expression) list *
                  (Ident.t * expression) list * class_expr
  | Tclass_constraint of class_expr * string list * string list * Concr.t

and class_structure =
  { cl_field: class_field list;
    cl_meths: Ident.t Meths.t }

and class_field =
    Cf_inher of class_expr * (string * Ident.t) list * (string * Ident.t) list
  | Cf_val of string * Ident.t * expression option * bool
  | Cf_meth of string * expression
  | Cf_init of expression

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type;
    mod_env: Env.t }

and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * module_type * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of module_expr * module_type * module_coercion
  | Tmod_unpack of expression * module_type

and structure = structure_item list

and structure_item =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_exception of Ident.t * exception_declaration
  | Tstr_exn_rebind of Ident.t * Path.t
  | Tstr_module of Ident.t * module_expr
  | Tstr_recmodule of (Ident.t * module_expr) list
  | Tstr_modtype of Ident.t * module_type
  | Tstr_open of Path.t
  | Tstr_class of
      (Ident.t * int * string list * class_expr * virtual_flag) list
  | Tstr_cltype of (Ident.t * cltype_declaration) list
  | Tstr_include of module_expr * Ident.t list
(*> JOCAML *)
  | Tstr_def of joinautomaton list
  | Tstr_loc of joinlocation list
  | Tstr_exn_global of Location.t * Path.t
(*< JOCAML *)

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description

(* Auxiliary functions over the a.s.t. *)

let iter_pattern_desc f = function
  | Tpat_alias(p, id) -> f p
  | Tpat_tuple patl -> List.iter f patl
  | Tpat_construct(cstr, patl) -> List.iter f patl
  | Tpat_variant(_, pat, _) -> may f pat
  | Tpat_record lbl_pat_list ->
      List.iter (fun (lbl, pat) -> f pat) lbl_pat_list
  | Tpat_array patl -> List.iter f patl
  | Tpat_or(p1, p2, _) -> f p1; f p2
  | Tpat_lazy p -> f p
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> ()

let map_pattern_desc f d =
  match d with
  | Tpat_alias (p1, id) ->
      Tpat_alias (f p1, id)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map f pats)
  | Tpat_record lpats ->
      Tpat_record (List.map (fun (l,p) -> l, f p) lpats)
  | Tpat_construct (c,pats) ->
      Tpat_construct (c, List.map f pats)
  | Tpat_array pats ->
      Tpat_array (List.map f pats)
  | Tpat_lazy p1 -> Tpat_lazy (f p1)
  | Tpat_variant (x1, Some p1, x2) ->
      Tpat_variant (x1, Some (f p1), x2)
  | Tpat_or (p1,p2,path) ->
      Tpat_or (f p1, f p2, path)
  | Tpat_var _
  | Tpat_constant _
  | Tpat_any
  | Tpat_variant (_,None,_) -> d

(* List the identifiers bound by a pattern or a let *)

let idents = ref([]: Ident.t list)

let rec bound_idents pat =
  match pat.pat_desc with
  | Tpat_var id -> idents := id :: !idents
  | Tpat_alias(p, id) -> bound_idents p; idents := id :: !idents
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments binds the same variables *)
      bound_idents p1
  | d -> iter_pattern_desc bound_idents d

let pat_bound_idents pat =
  idents := []; bound_idents pat; let res = !idents in idents := []; res

let rev_let_bound_idents pat_expr_list =
  idents := [];
  List.iter (fun (pat, expr) -> bound_idents pat) pat_expr_list;
  let res = !idents in idents := []; res

let let_bound_idents pat_expr_list =
  List.rev(rev_let_bound_idents pat_expr_list)

(*> JOCAML *)
let do_def_bound_idents autos r =
  List.fold_right
    (fun {jauto_original=names} r -> names@r)
    autos r

let do_loc_bound_idents locs r =
  List.fold_right
    (fun {jloc_desc=(id_loc,autos,_)} r ->
      id_loc.jident_desc::do_def_bound_idents autos r)
    locs r
      

let def_bound_idents d = do_def_bound_idents d []
let loc_bound_idents d = do_loc_bound_idents d []

let rev_def_bound_idents d = List.rev (def_bound_idents d)
let rev_loc_bound_idents d =  List.rev (loc_bound_idents d)
(*< JOCAML *)

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Tpat_var id -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Tpat_var (alpha_var env id) with
     | Not_found -> Tpat_any}
| Tpat_alias (p1, id) ->
    let new_p =  alpha_pat env p1 in
    begin try
      {p with pat_desc = Tpat_alias (new_p, alpha_var env id)}
    with
    | Not_found -> new_p
    end
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}
