(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_iterator

let err = Syntaxerr.ill_formed_ast

let empty_record loc = err loc "Records cannot be empty."
let invalid_tuple loc = err loc "Tuples must have at least 2 components."
let empty_open_tuple_pat loc =
  err loc "Open tuple patterns must have at least one component."
let short_closed_tuple_pat loc =
  err loc "Closed tuple patterns must have at least two components."
let no_args loc = err loc "Function application with no argument."
let empty_let loc = err loc "Let with no bindings."
let empty_type loc = err loc "Type declarations cannot be empty."
let empty_poly_binder loc =
  err loc "Explicit universal type quantification cannot be empty."
let complex_id loc = err loc "Functor application not allowed here."
let module_type_substitution_missing_rhs loc =
  err loc "Module type substitution with no right hand side"
let function_without_value_parameters loc =
  err loc "Function without any value parameters"
let invalid_struct_item loc =
  err loc "This kind of structure item is not allowed in this context."

let simple_longident id =
  let rec is_simple = function
    | Longident.Lident _ -> true
    | Longident.Ldot (id, _) -> is_simple id.txt
    | Longident.Lapply _ -> false
  in
  if not (is_simple id.txt) then complex_id id.loc

let iterator =
  let super = Ast_iterator.default_iterator in
  let type_declaration self td =
    super.type_declaration self td;
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_record [] -> empty_record loc
    | _ -> ()
  in
  let typ self ty =
    super.typ self ty;
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_tuple ([] | [_]) -> invalid_tuple loc
    | Ptyp_package ptyp ->
      List.iter (fun (id, _) -> simple_longident id) ptyp.ppt_cstrs
    | Ptyp_poly([],_) -> empty_poly_binder loc
    | _ -> ()
  in
  let pat self pat =
    begin match pat.ppat_desc with
    | Ppat_construct (_, Some (_, ({ppat_desc = Ppat_tuple _} as p)))
      when Builtin_attributes.explicit_arity pat.ppat_attributes ->
        super.pat self p (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.pat self pat
    end;
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_tuple (([] | [_]), Closed) -> short_closed_tuple_pat loc
    | Ppat_tuple ([], Open) -> empty_open_tuple_pat loc
    | Ppat_record ([], _) -> empty_record loc
    | Ppat_construct (id, _) -> simple_longident id
    | Ppat_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | _ -> ()
  in
  let expr self exp =
    begin match exp.pexp_desc with
    | Pexp_construct (_, Some ({pexp_desc = Pexp_tuple _} as e))
      when Builtin_attributes.explicit_arity exp.pexp_attributes ->
        super.expr self e (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.expr self exp
    end;
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_tuple ([] | [_]) -> invalid_tuple loc
    | Pexp_record ([], _) -> empty_record loc
    | Pexp_apply (_, []) -> no_args loc
    | Pexp_let (_, [], _) -> empty_let loc
    | Pexp_ident id
    | Pexp_construct (id, _)
    | Pexp_field (_, id)
    | Pexp_setfield (_, id, _)
    | Pexp_new id -> simple_longident id
    | Pexp_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | Pexp_function (params, _, Pfunction_body _) ->
        if
          List.for_all
            (function
              | { pparam_desc = Pparam_newtype _ } -> true
              | { pparam_desc = Pparam_val _ } -> false)
            params
        then function_without_value_parameters loc
    | Pexp_struct_item ({pstr_desc = Pstr_extension _ |
                                     Pstr_open _ |
                                     Pstr_exception _ |
                                     Pstr_module _}, _) -> ()
    | Pexp_struct_item ({pstr_loc = loc}, _) -> invalid_struct_item loc
    | _ -> ()
  in
  let extension_constructor self ec =
    super.extension_constructor self ec;
    match ec.pext_kind with
    | Pext_rebind id -> simple_longident id
    | _ -> ()
  in
  let class_expr self ce =
    super.class_expr self ce;
    let loc = ce.pcl_loc in
    match ce.pcl_desc with
    | Pcl_apply (_, []) -> no_args loc
    | Pcl_constr (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_type self mty =
    super.module_type self mty;
    match mty.pmty_desc with
    | Pmty_alias id -> simple_longident id
    | _ -> ()
  in
  let open_description self opn =
    super.open_description self opn
  in
  let with_constraint self wc =
    super.with_constraint self wc;
    match wc with
    | Pwith_type (id, _)
    | Pwith_module (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_expr self me =
    super.module_expr self me;
    match me.pmod_desc with
    | Pmod_ident id -> simple_longident id
    | _ -> ()
  in
  let structure_item self st =
    super.structure_item self st;
    let loc = st.pstr_loc in
    match st.pstr_desc with
    | Pstr_type (_, []) -> empty_type loc
    | Pstr_value (_, []) -> empty_let loc
    | _ -> ()
  in
  let signature_item self sg =
    super.signature_item self sg;
    let loc = sg.psig_loc in
    match sg.psig_desc with
    | Psig_type (_, []) -> empty_type loc
    | Psig_modtypesubst {pmtd_type=None; _ } ->
        module_type_substitution_missing_rhs loc
    | _ -> ()
  in
  let row_field self field =
    super.row_field self field;
    let loc = field.prf_loc in
    match field.prf_desc with
    | Rtag _ -> ()
    | Rinherit _ ->
      if field.prf_attributes = []
      then ()
      else err loc
          "In variant types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let object_field self field =
    super.object_field self field;
    let loc = field.pof_loc in
    match field.pof_desc with
    | Otag _ -> ()
    | Oinherit _ ->
      if field.pof_attributes = []
      then ()
      else err loc
          "In object types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let attribute self attr =
    (* The change to `self` here avoids registering attributes within attributes
       for the purposes of warning 53, while keeping all the other invariant
       checks for attribute payloads.  See comment on [current_phase] in
       [builtin_attributes.mli]. *)
    super.attribute { self with attribute = super.attribute } attr;
    Builtin_attributes.(register_attr Invariant_check attr.attr_name)
  in
  { super with
    type_declaration
  ; typ
  ; pat
  ; expr
  ; extension_constructor
  ; class_expr
  ; module_expr
  ; module_type
  ; open_description
  ; with_constraint
  ; structure_item
  ; signature_item
  ; row_field
  ; object_field
  ; attribute
  }

let structure st = iterator.structure iterator st
let signature sg = iterator.signature iterator sg

let check_loc_ghost meth v ~source_contents =
  let equal_modulo_loc =
    let no_locs =
      { Ast_mapper.default_mapper
        with location = (fun _ _ -> Location.none);
             attributes = (fun _ _ -> []);
        (* type z = (int [@foo]) create int at location "int" instead of
           "int [@foo]". I'd rather loosen the check than worsen the location
           for type errors. *)
      }
    in
    fun meth node1 node2 ->
      let norm1 = (meth no_locs) no_locs node1 in
      let norm2 = (meth no_locs) no_locs node2 in
      Stdlib.(=) norm1 norm2
  in
  let super = Ast_iterator.default_iterator in
  let depth = ref 0 in
  let limit_quadratic_complexity meth f =
    fun self v ->
      if !depth < 1000 then (
        depth := !depth + 1;
        (meth super) self v;
        depth := !depth -1 ;
        f v;
    )
  in
  let check ?print ?(wrap = Fun.id) meth parse ast1 (loc : Location.t) =
    let source_fragment =
      wrap (
          String.sub source_contents
            loc.loc_start.pos_cnum
            (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
        )
    in
    let lexbuf = Lexing.from_string source_fragment in
    let should_be_loc_ghost, error_if_not =
      match parse lexbuf with
      | exception Parsing.Parse_error | exception _ ->
         true, "non-ghost location points to a non parsable range"
      | ast2 ->
         if equal_modulo_loc meth ast1 ast2
         then false, "ghost location should be non-ghost"
         else true, "non-ghost location points to a range of source \
                     code that contains the wrong ast"
    in
    if loc.loc_ghost <> should_be_loc_ghost
    then (
      Format.eprintf "@[<2>%a: %s%t@]@." Location.print_loc loc error_if_not
        (fun f ->
          match print with
          | None -> ()
          | Some print -> Format.fprintf f "@\n%a" print ast1)
    )
  in
  let self =
    { super with
      expr =
        limit_quadratic_complexity (fun s -> s.expr)
          (fun v ->
            check (fun s -> s.expr) Parse.expression v v.pexp_loc
              (* ~print:(fun f ty -> Printast.expression 0 f ty) *)
              (* Add parens because in 1 + 2, + gets assigned a non-ghost
                 location, but + without parens is not a valid expression. *)
              ~wrap:(fun s -> "( " ^ s ^ " )"))
    ; pat =
        limit_quadratic_complexity (fun s -> s.pat)
          (fun v -> check (fun s -> s.pat) Parse.pattern v v.ppat_loc )
    ; typ =
        limit_quadratic_complexity (fun s -> s.typ)
          (fun v ->
            check
              (* ~print:(fun f ty -> Printast.payload 0 f (PTyp ty)) *)
              (fun s -> s.typ) Parse.core_type v v.ptyp_loc )
    ; attribute = (fun self attr ->
      (* Doc comments would probably need some special case to check they are
         correctly placed. *)
      if attr.attr_name.txt = "ocaml.doc"
         || attr.attr_name.txt = "ocaml.text"
      then ()
      else super.attribute self attr)
    }
  in
  (meth self) self v
