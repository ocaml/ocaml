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

(** Abstract syntax tree after typing *)


(** By comparison with {!Parsetree}:
    - Every {!Longindent.t} is accompanied by a resolved {!Path.t}.

*)

open Asttypes

(* Value expressions for the core language *)

type partial = Partial | Total

(** {1 Extension points} *)

type attribute = Parsetree.attribute
type attributes = attribute list

(** {1 Core language} *)

type value = Value_pattern
type computation = Computation_pattern

type _ pattern_category =
| Value : value pattern_category
| Computation : computation pattern_category

type pattern = value general_pattern
and 'k general_pattern = 'k pattern_desc pattern_data

and 'a pattern_data =
  { pat_desc: 'a;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attributes) list;
    pat_type: Types.type_expr;
    pat_env: Env.t;
    pat_attributes: attributes;
   }

and pat_extra =
  | Tpat_constraint of core_type
        (** P : T          { pat_desc = P
                           ; pat_extra = (Tpat_constraint T, _, _) :: ... }
         *)
  | Tpat_type of Path.t * Longident.t loc
        (** #tconst        { pat_desc = disjunction
                           ; pat_extra = (Tpat_type (P, "tconst"), _, _) :: ...}

                           where [disjunction] is a [Tpat_or _] representing the
                           branches of [tconst].
         *)
  | Tpat_open of Path.t * Longident.t loc * Env.t
  | Tpat_unpack
        (** (module P)     { pat_desc  = Tpat_var "P"
                           ; pat_extra = (Tpat_unpack, _, _) :: ... }
         *)

and 'k pattern_desc =
  (* value patterns *)
  | Tpat_any : value pattern_desc
        (** _ *)
  | Tpat_var : Ident.t * string loc -> value pattern_desc
        (** x *)
  | Tpat_alias :
      value general_pattern * Ident.t * string loc -> value pattern_desc
        (** P as a *)
  | Tpat_constant : constant -> value pattern_desc
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple : value general_pattern list -> value pattern_desc
        (** (P1, ..., Pn)

            Invariant: n >= 2
         *)
  | Tpat_construct :
      Longident.t loc * Types.constructor_description *
        value general_pattern list * (Ident.t loc list * core_type) option ->
      value pattern_desc
        (** C                             ([], None)
            C P                           ([P], None)
            C (P1, ..., Pn)               ([P1; ...; Pn], None)
            C (P : t)                     ([P], Some ([], t))
            C (P1, ..., Pn : t)           ([P1; ...; Pn], Some ([], t))
            C (type a) (P : t)            ([P], Some ([a], t))
            C (type a) (P1, ..., Pn : t)  ([P1; ...; Pn], Some ([a], t))
          *)
  | Tpat_variant :
      label * value general_pattern option * Types.row_desc ref ->
      value pattern_desc
        (** `A             (None)
            `A P           (Some P)

            See {!Types.row_desc} for an explanation of the last parameter.
         *)
  | Tpat_record :
      (Longident.t loc * Types.label_description * value general_pattern) list *
        closed_flag ->
      value pattern_desc
        (** { l1=P1; ...; ln=Pn }     (flag = Closed)
            { l1=P1; ...; ln=Pn; _}   (flag = Open)

            Invariant: n > 0
         *)
  | Tpat_array : value general_pattern list -> value pattern_desc
        (** [| P1; ...; Pn |] *)
  | Tpat_lazy : value general_pattern -> value pattern_desc
        (** lazy P *)
  (* computation patterns *)
  | Tpat_value : tpat_value_argument -> computation pattern_desc
        (** P

            Invariant: Tpat_value pattern should not carry
            pat_attributes or pat_extra metadata coming from user
            syntax, which must be on the inner pattern node -- to
            facilitate searching for a certain value pattern
            constructor with a specific attributed.

            To enforce this restriction, we made the argument of
            the Tpat_value constructor a private synonym of [pattern],
            requiring you to use the [as_computation_pattern] function
            below instead of using the [Tpat_value] constructor directly.
         *)
  | Tpat_exception : value general_pattern -> computation pattern_desc
        (** exception P *)
  (* generic constructions *)
  | Tpat_or :
      'k general_pattern * 'k general_pattern * Types.row_desc option ->
      'k pattern_desc
        (** P1 | P2

            [row_desc] = [Some _] when translating [Ppat_type _],
                         [None] otherwise.
         *)

and tpat_value_argument = private value general_pattern

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attributes) list;
    exp_type: Types.type_expr;
    exp_env: Env.t;
    exp_attributes: attributes;
   }

and exp_extra =
  | Texp_constraint of core_type
        (** E : T *)
  | Texp_coerce of core_type option * core_type
        (** E :> T           [Texp_coerce (None, T)]
            E : T0 :> T      [Texp_coerce (Some T0, T)]
         *)
  | Texp_poly of core_type option
        (** Used for method bodies. *)
  | Texp_newtype of string
        (** fun (type t) ->  *)

and expression_desc =
    Texp_ident of Path.t * Longident.t loc * Types.value_description
        (** x
            M.x
         *)
  | Texp_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of { arg_label : arg_label; param : Ident.t;
      cases : value case list; partial : partial; }
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            [param] is the identifier that is to be used to name the
            parameter of the function.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * (arg_label * expression option) list
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * computation case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 | exception P3 -> E2
            | exception P4 -> E3

            [Texp_match (E0, [(P1, E1); (P2 | exception P3, E2);
                              (exception P4, E3)], _)]
         *)
  | Texp_try of expression * value case list
        (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple of expression list
        (** (E1, ..., EN) *)
  | Texp_construct of
      Longident.t loc * Types.constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * expression option
  | Texp_record of {
      fields : ( Types.label_description * record_label_definition ) array;
      representation : Types.record_representation;
      extended_expression : expression option;
    }
        (** { l1=P1; ...; ln=Pn }           (extended_expression = None)
            { E0 with l1=P1; ...; ln=Pn }   (extended_expression = Some E0)

            Invariant: n > 0

            If the type is { l1: t1; l2: t2 }, the expression
            { E0 with t2=P2 } is represented as
            Texp_record
              { fields = [| l1, Kept t1; l2 Override P2 |]; representation;
                extended_expression = Some E0 }
        *)
  | Texp_field of expression * Longident.t loc * Types.label_description
  | Texp_setfield of
      expression * Longident.t loc * Types.label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Ident.t * string loc * expression) list
  | Texp_letmodule of
      Ident.t option * string option loc * Types.module_presence * module_expr *
        expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression * Location.t
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      body : value case;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  | Texp_open of open_declaration * expression
        (** let open[!] M in e *)

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t
  | Tmeth_ancestor of Ident.t * Path.t

and 'k case =
    {
     c_lhs: 'k general_pattern;
     c_guard: expression option;
     c_rhs: expression;
    }

and record_label_definition =
  | Kept of Types.type_expr * mutable_flag
  | Overridden of Longident.t loc * expression

and binding_op =
  {
    bop_op_path : Path.t;
    bop_op_name : string loc;
    bop_op_val : Types.value_description;
    bop_op_type : Types.type_expr;
    (* This is the type at which the operator was used.
       It is always an instance of [bop_op_val.val_type] *)
    bop_exp : expression;
    bop_loc : Location.t;
  }

(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: Types.class_type;
     cl_env: Env.t;
     cl_attributes: attributes;
    }

and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * expression option) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list
      * Types.MethSet.t
  (* Visible instance variables, methods and concrete methods *)
  | Tcl_open of open_description * class_expr

and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: Types.class_signature;
   cstr_meths: Ident.t Types.Meths.t;
  }

and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attributes;
  }

and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression

and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
  (** The module type constraint has been synthesized during typechecking. *)
  | Tmodtype_explicit of module_type
  (** The module type was in the source file. *)

and functor_parameter =
  | Unit
  | Named of Ident.t option * string option loc * module_type

and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of functor_parameter * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
    (** ME          (constraint = Tmodtype_implicit)
        (ME : MT)   (constraint = Tmodtype_explicit MT)
     *)
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of type_exception
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding =
    {
     mb_id: Ident.t option;
     mb_name: string option loc;
     mb_presence: Types.module_presence;
     mb_expr: module_expr;
     mb_attributes: attributes;
     mb_loc: Location.t;
    }

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Env.t * Path.t * module_coercion

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attributes;
   }

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of functor_parameter * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc

and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: Types.type_expr;
    pc_env: Env.t;
    pc_loc : Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of type_exception
  | Tsig_module of module_declaration
  | Tsig_modsubst of module_substitution
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_modtypesubst of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration =
    {
     md_id: Ident.t option;
     md_name: string option loc;
     md_presence: Types.module_presence;
     md_type: module_type;
     md_attributes: attributes;
     md_loc: Location.t;
    }

and module_substitution =
    {
     ms_id: Ident.t;
     ms_name: string loc;
     ms_manifest: Path.t;
     ms_txt: Longident.t loc;
     ms_attributes: attributes;
     ms_loc: Location.t;
    }

and module_type_declaration =
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_type: module_type option;
     mtd_attributes: attributes;
     mtd_loc: Location.t;
    }

and 'a open_infos =
    {
     open_expr: 'a;
     open_bound_items: Types.signature;
     open_override: override_flag;
     open_env: Env.t;
     open_loc: Location.t;
     open_attributes: attribute list;
    }

and open_description = (Path.t * Longident.t loc) open_infos

and open_declaration = module_expr open_infos


and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_attributes: attribute list;
    }

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_modtype of module_type
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc
  | Twith_modtypesubst of module_type

and core_type =
  { mutable ctyp_desc : core_type_desc;
      (** mutable because of [Typeclass.declare_method] *)
    mutable ctyp_type : Types.type_expr;
      (** mutable because of [Typeclass.declare_method] *)
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attributes;
   }

and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

and package_type = {
  pack_path : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and row_field = {
  rf_desc : row_field_desc;
  rf_loc : Location.t;
  rf_attributes : attributes;
}

and row_field_desc =
    Ttag of string loc * bool * core_type list
  | Tinherit of core_type

and object_field = {
  of_desc : object_field_desc;
  of_loc : Location.t;
  of_attributes : attributes;
}

and object_field_desc =
  | OTtag of string loc * core_type
  | OTinherit of core_type

and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attributes;
    }

and type_declaration =
  {
    typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * (variance * injectivity)) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attributes;
   }

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attributes;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_vars: string loc list;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attributes;
    }

and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * (variance * injectivity)) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_loc: Location.t;
    tyext_attributes: attributes;
  }

and type_exception =
  {
    tyexn_constructor: extension_constructor;
    tyexn_loc: Location.t;
    tyexn_attributes: attribute list;
  }

and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type : Types.extension_constructor;
    ext_kind : extension_constructor_kind;
    ext_loc : Location.t;
    ext_attributes: attributes;
  }

and extension_constructor_kind =
    Text_decl of string loc list * constructor_arguments * core_type option
  | Text_rebind of Path.t * Longident.t loc

and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: Types.class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attributes;
    }

and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type
  | Tcty_open of open_description * class_type

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
  }

and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attributes;
  }

and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * (variance * injectivity)) list;
    ci_id_name : string loc;
    ci_id_class: Ident.t;
    ci_id_class_type : Ident.t;
    ci_id_object : Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl : Types.class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attributes;
   }

type implementation = {
  structure: structure;
  coercion: module_coercion;
  signature: Types.signature;
  shape: Shape.t;
}
(** A typechecked implementation including its module structure, its exported
    signature, and a coercion of the module against that signature.

    If an .mli file is present, the signature will come from that file and be
    the exported signature of the module.

    If there isn't one, the signature will be inferred from the module
    structure.
*)

(* Auxiliary functions over the a.s.t. *)

(** [as_computation_pattern p] is a computation pattern with description
    [Tpat_value p], which enforces a correct placement of pat_attributes
    and pat_extra metadata (on the inner value pattern, rather than on
    the computation pattern). *)
val as_computation_pattern: pattern -> computation general_pattern

val classify_pattern_desc: 'k pattern_desc -> 'k pattern_category
val classify_pattern: 'k general_pattern -> 'k pattern_category

type pattern_action =
  { f : 'k . 'k general_pattern -> unit }
val shallow_iter_pattern_desc:
    pattern_action -> 'k pattern_desc -> unit

type pattern_transformation =
  { f : 'k . 'k general_pattern -> 'k general_pattern }
val shallow_map_pattern_desc:
    pattern_transformation -> 'k pattern_desc -> 'k pattern_desc

val iter_general_pattern: pattern_action -> 'k general_pattern -> unit
val iter_pattern: (pattern -> unit) -> pattern -> unit

type pattern_predicate = { f : 'k . 'k general_pattern -> bool }
val exists_general_pattern: pattern_predicate -> 'k general_pattern -> bool
val exists_pattern: (pattern -> bool) -> pattern -> bool

val let_bound_idents: value_binding list -> Ident.t list
val let_bound_idents_full:
    value_binding list -> (Ident.t * string loc * Types.type_expr) list

(** Alpha conversion of patterns *)
val alpha_pat:
  (Ident.t * Ident.t) list -> 'k general_pattern -> 'k general_pattern

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc

val pat_bound_idents: 'k general_pattern -> Ident.t list
val pat_bound_idents_full:
  'k general_pattern -> (Ident.t * string loc * Types.type_expr) list

(** Splits an or pattern into its value (left) and exception (right) parts. *)
val split_pattern:
  computation general_pattern -> pattern option * pattern option
