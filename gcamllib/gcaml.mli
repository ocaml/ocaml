(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Generic primitives *)

(* Generic primitives are generic values which cannot be defined using
   the generic let binding. However, we can define them by providing
   their compilation using new "generic val" toplevel declaration.

   generic val name : {'a, 'b, ..., 'n} => t = fun (ta, tb, ..., tn) -> ...

   defines a generic primitive [name] whose type is {'a, 'b, ..., 'n} => t.
   The type variables ['a], ['b], ... and ['n] must appear inside the [t].
   The abstractions [ta], [tb], ... and [tn] are variables which will be 
   bound to the type information information of the type variables ['a], ['b],
    ... and ['n]  at run time. The ordering of the instantiation information
   of type variables corresponds with the occurrence order of them inside [t].
   Note that this may be DIFFERENT from their ordering inside
   [{'a, 'b, ..., 'n}].

   BUG: types of generic primitives are not well verified. Illegal types for
   generic primitives may cause strange behaviour.
*)

(* Run time types

   Rtype.type_expr is run time type, ML value representation of types. 
   Its precise definitions and tools are available stdlib/rtype.mli.
   
   Run time type syntax sugar

   Since writing run time types using directly the constructors defined 
   Rtype is hard task, a syntactic sugar is available:

     [: int :]

   This expression represents the run time type of the ML type [int].

   Type variables inside [: :] notation is always generalized, and
   no relationship between the type variables inside type constraints.
   For example,
   
     ((1 : 'a), [: 'a list :])

   returns (1, [: 'a list :]), not (1, [: int list :]).

   Run time type patterns

   [: :] notation can also be used inside pattern matching. Inside pattern,
   type variables are NOT pattern variables, and they match only with
   variables. To write pattern variables inside [: :] notation, put ^ sign
   before them:

     match t with
     | [: ^x list :] -> e

   In the above code, if [t] carries run time type [: int list :], then
   inside the evaluation of [e], the pattern variable [x] will be bound
   to [: int :].

   Pattern syntactic sugar is not almighty enough. You may need to use
   the ordinal pattern matching using the type constructor defined Rtype
   to do complicated things.
*)

open Rtype

val typeof : {'a} => 'a -> type_expr
(** [typeof e] returns type of the argument [e]. *)

(* Dynamic value primitives *)

type dyn
(** Abstract data type for dynamic values *)

exception Coercion_failure of type_expr * type_expr
(** Exception raised when coercions of dynamic values fail. *)

val dyn : {'a} => 'a -> dyn
(** [dyn e] evaluates [e], then encapsulate its result into a dynamic value
   whose type is [dyn]. *)

val coerce : {'a} => dyn -> 'a
(** [coerce] provides an access to the content of dynamic values:
   [coerce d] takes a dynamic value [d], then checksde its encapsulated type
   information is more general than the type context. If it is, the [coerce d]
   returns the content of [d] as a value of the context type. Otherwise,
   it raises [Coercion_failure]. 

   BUG: At this moment, [coerce] can extract the contents of dynamic values
   only when the encapsulated type and the context type are exactly same.
   The following coercion is not possible, since dyn [] creates 
   an encapsulation of ([] : 'a list) and the context of coerce is int list:
     1 :: (coerce (dyn []))
*)

val type_of_dyn : dyn -> type_expr
val obj_of_dyn : dyn -> Obj.t
val dyn_of : type_expr -> Obj.t -> dyn
