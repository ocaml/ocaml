(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

type 'a view = 
    Eq of 'a * 'a 
  | Neq of 'a * 'a 
  | Builtin of bool * Hstring.t * 'a list

type t

val make : Term.t view -> t
val add_label : Hstring.t -> t -> unit

val label : t -> Hstring.t

val mk_pred : Term.t -> t

val vrai : t
val faux : t 

val neg_view : 'a view -> 'a view

val view : t -> Term.t view
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val neg : t -> t
val apply_subst : Term.subst -> t -> t

val terms_of : t -> Term.Set.t
val vars_of : t -> Symbols.Set.t


val print_view : 
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a view -> unit

val print : Format.formatter -> t -> unit
val print_list : Format.formatter -> t list-> unit

module SetEq : Set.S with type elt = t * Term.t * Term.t
module Map : Map.S with type key = t
module Set : Set.S with type elt = t

