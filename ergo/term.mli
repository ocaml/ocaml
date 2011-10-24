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

type t
type view = {f: Symbols.t ; xs: t list; ty: Ty.t}

type subst = t Ergo_subst.t * Ty.subst

val view : t -> view
val make : Symbols.t -> t list -> Ty.t -> t

val shorten : t -> t

val vrai : t
val faux : t
val void : t

val int : string -> t
val real : string -> t
val bitv : string -> Ty.t -> t

val is_int : t -> bool
val is_real : t -> bool

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val vars_of : t -> Symbols.Set.t
val pred : t -> t

val apply_subst : subst -> t -> t
val print : Format.formatter -> t -> unit
val print_list : Format.formatter -> t list -> unit

val dummy : t 

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
 
val subterms : Set.t -> t -> Set.t
