(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on core types *)

open Asttypes
open Types

exception Unify of (type_expr * type_expr) list
exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list
exception Cannot_expand
exception Cannot_apply
exception Recursive_abbrev

val init_def: int -> unit
        (* Set the initial variable level *)
val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
val begin_class_def: unit -> unit
val raise_nongen_level: unit -> unit
val reset_global_level: unit -> unit
val increase_global_level: unit -> unit
val restore_global_level: unit -> unit

val newty: type_desc -> type_expr
val newvar: unit -> type_expr
        (* Return a fresh variable *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val newobj: type_expr -> type_expr
val newconstr: Path.t -> type_expr list -> type_expr
val none: type_expr
        (* A dummy type expression *)

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val object_fields: type_expr -> type_expr
val flatten_fields:
        type_expr -> (string * field_kind * type_expr) list * type_expr
        (* Transform a field type into a list of pairs label-type *)
        (* The fields are sorted *)
val associate_fields:
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr * field_kind * type_expr) list *
        (string * field_kind * type_expr) list *
        (string * field_kind * type_expr) list
val opened_object: type_expr -> bool
val close_object: type_expr -> unit
val row_variable: type_expr -> type_expr
        (* Return the row variable of an open object type *)
val set_object_name:
        Ident.t -> type_expr -> type_expr list -> type_expr -> unit
val remove_object_name: type_expr -> unit
val hide_private_methods: type_expr -> unit

val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
val iterative_generalization: int -> type_expr list -> type_expr list
        (* Efficient repeated generalization of a type *)
val make_nongen: type_expr -> unit
        (* Make non-generalizable the given type *)
val correct_levels: type_expr -> type_expr
        (* Returns a copy with decreasing levels *)
val limited_generalize: type_expr -> type_expr -> unit
        (* Only generalize some part of the type
           Make the remaining of the type non-generalizable *)

val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
val instance_list: type_expr list -> type_expr list
        (* Take an instance of a list of type schemes *)
val instance_constructor:
        constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
val instance_parameterized_type:
        type_expr list -> type_expr -> type_expr list * type_expr
val instance_parameterized_type_2:
        type_expr list -> type_expr list -> type_expr ->
        type_expr list * type_expr list * type_expr
val instance_class:
        type_expr list -> class_type -> type_expr list * class_type
val apply:
        Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
        (* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
        the parameters [pi] and returns the corresponding instance of
        [t]. Exception [Cannot_apply] is raised in case of failure. *)

val expand_head: Env.t -> type_expr -> type_expr
val full_expand: Env.t -> type_expr -> type_expr

val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)
val filter_method: Env.t -> string -> private_flag -> type_expr -> type_expr
        (* A special case of unification (with {m : 'a; 'b}). *)
val check_filter_method: Env.t -> string -> private_flag -> type_expr -> unit
        (* A special case of unification (with {m : 'a; 'b}), returning unit. *)
val filter_self_method:
        Env.t -> string -> private_flag -> (Ident.t * type_expr) Meths.t ref ->
        type_expr -> Ident.t * type_expr
val moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)
type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of (type_expr * type_expr) list
  | CM_Class_type_mismatch of class_type * class_type
  | CM_Parameter_mismatch of (type_expr * type_expr) list
  | CM_Val_type_mismatch of string * (type_expr * type_expr) list
  | CM_Meth_type_mismatch of string * (type_expr * type_expr) list
  | CM_Non_mutable_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string
val match_class_types:
        Env.t -> class_type -> class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)
val equal: Env.t -> bool -> type_expr list -> type_expr list -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
val match_class_declarations:
        Env.t -> type_expr list -> class_type -> type_expr list ->
        class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)

val enlarge_type: Env.t -> type_expr -> type_expr
        (* Make a type larger *)
val subtype : Env.t -> type_expr -> type_expr -> unit -> unit
        (* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
           It accumulates the constraints the type variables must
           enforce and returns a function that inforce this
           constraints. *)

val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
val nondep_type_decl:
        Env.t -> Ident.t -> Ident.t -> bool -> type_declaration ->
        type_declaration
        (* Same for type declarations. *)
val nondep_class_declaration:
        Env.t -> Ident.t -> class_declaration -> class_declaration
        (* Same for class declarations. *)
val nondep_cltype_declaration:
        Env.t -> Ident.t -> cltype_declaration -> cltype_declaration
        (* Same for class type declarations. *)
val correct_abbrev: Env.t -> Ident.t -> type_expr list -> type_expr -> unit
val cyclic_abbrev: Env.t -> Ident.t -> type_expr -> bool

val closed_schema: type_expr -> bool
        (* Check whether the given type scheme contains no non-generic
           type variables *)

val closed_type_decl: type_declaration -> type_expr option
type closed_class_failure =
    CC_Method of type_expr * string * type_expr
  | CC_Value of type_expr * string * type_expr
val closed_class:
        type_expr list -> class_signature -> closed_class_failure option
        (* Check whether all type variables are bound *)

val unalias: type_expr -> type_expr
val signature_of_class_type: class_type -> class_signature
val self_type: class_type -> type_expr
val class_type_arity: class_type -> int
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
