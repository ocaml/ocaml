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

type t = 
    | Tint
    | Treal
    | Tbool
    | Tunit
    | Tvar of tvar
    | Tbitv of int
    | Text of t list * Hstring.t
    | Tfarray of t
        
and tvar = { v : int ; mutable value : t option }

type subst

val esubst : subst

exception TypeClash of t*t

val tunit : t

val text : t list -> string -> t

val shorten : t -> t

val fresh_var : unit -> tvar
val fresh_empty_text : unit -> t

val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int

val unify : t -> t -> unit
val matching : subst -> t -> t -> subst

val apply_subst : subst -> t -> t

(* Applique la seconde substitution sur la premiere 
   puis fais l'union des map avec prioritée à la première *)
val union_subst : subst -> subst -> subst

val compare_subst : subst -> subst -> int

val print : Format.formatter -> t -> unit
val printl : Format.formatter -> t list -> unit
