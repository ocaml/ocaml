(* Abstract syntax tree after typing *)

open Asttypes

(* Type expressions for the core language *)

type type_expr =
    Tvar of type_variable
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list

and type_variable =
    { mutable tvar_level: int;
      mutable tvar_link: type_expr option }

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                       (* Type of the val *)
    val_prim: Primitive.description option }   (* Is this a primitive? *)

(* Constructor descriptions *)

type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int }               (* Number of non-const constructors *)

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)

(* Record label descriptions *)

type label_description =
  { lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation } (* Representation for this record *)

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* Value expressions for the core language *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern list
  | Tpat_record of (label_description * pattern) list
  | Tpat_or of pattern * pattern

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr }

and expression_desc =
    Texp_ident of Path.t * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression list
  | Texp_record of (label_description * expression) list
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_when of expression * expression

(* Type definitions *)

type type_declaration =
  { mutable type_params: type_expr list;
    type_arity: int;
    mutable type_kind: type_kind }

and type_kind =
    Type_abstract
  | Type_manifest of type_expr
  | Type_variant of (string * type_expr list) list
  | Type_record of (string * mutable_flag * type_expr) list

type exception_declaration = type_expr list

(* Type expressions for the module language *)

type module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * module_type * module_type

and signature = signature_item list

and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type
  | Tsig_modtype of Ident.t * modtype_declaration

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

(* Value expressions for the module language *)

type module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type }

and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * module_type * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of module_expr * module_type * module_coercion

and structure = structure_item list

and structure_item =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_exception of Ident.t * exception_declaration
  | Tstr_module of Ident.t * module_expr
  | Tstr_modtype of Ident.t * module_type
  | Tstr_open of Path.t

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion

(* Auxiliary functions over the a.s.t. *)

val pat_bound_idents: pattern -> Ident.t list
val let_bound_idents: (pattern * expression) list -> Ident.t list
