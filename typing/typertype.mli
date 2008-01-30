(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type error =
    Unsupported
  | Cannot_have_full_path
  | Multiply_bound_type_variable

exception Error of Location.t * error

val get_rtype_path : unit -> Path.t
val get_rtype_type : unit -> Types.type_expr
val get_rtype_type_declaration : Env.t -> Types.type_expr
(* type information of Rtype.type_expr *)

type implicit_when = Parsetree.expression * Parsetree.expression

val pattern_of_type :
    bool-> 
      (Longident.t -> Path.t) -> Parsetree.core_type -> 
	Parsetree.pattern * implicit_when list
(* [pattern_of_type nonlinear longident_to_path core_type] regards
   [core_type] of Parsetree.core_type as a pattern expression and 
   returns a pattern. The boolean parameter [nonlinar] controls 
   whether non-linear occurrences of type variables such as [: 'a -> 'a :]
   are permitted or not. Additional to the pattern, the list of 
   such non-linear type variable occurrences are reported.
   Now, it also returns additional checks for data type descriptions
 *) 

val runtime_type_exprs : 
    Types.type_expr list -> Rtype.type_expr list *
	(Types.type_expr * Rtype.type_expr) list * 
	(Rtype.type_declaration * Path.t) list
(* [runtime_type_exprs types] returns a triple [rts, cache, dummy_decls],
   where [rts] are the run time representation of [types]. 
   [cache] keeps the track of type conversions occured recursively.
   [dummy_decls] contain the table of temporal run time type declarations
   of the data types inside [types]. These dummy declarations must be
   replaced by the real run time type declarations later, by
   Translcore.transl_type_exprs. *)

val runtime_type_declaration :
    string ->
      Ident.t list ->
	Types.type_declaration ->
	  Rtype.type_declaration * 
	    (Types.type_expr * Rtype.type_expr) list * 
	    (Rtype.type_declaration * Path.t) list
(* [runtime_type_declaration name recdefs typedecl] converts a type declaration
   [typedecl] of name [name] to its run time representation defined 
   with [recdefs]. Like [runtime_type_exprs], it also returns cache and 
   dummy declaration tables. *)

val recover_type_expr :
  (Rtype.Path.t * 'a) Rtype.raw_type_expr -> Types.type_expr
(* "try" to recover a real type expression from a given run time type.
   The result is not always correct, since the conversion [runtime_type_exprs]
   from the real type to the run time type is not one-to-one. 
   DO NOT TRUST THE RESULTS! *)

val path_is_in_scope : Env.t -> Rtype.Path.t -> bool

val report_error : Format.formatter -> error -> unit

