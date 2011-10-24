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

type operator = 
    Plus | Minus | Mult | Div | Modulo | Concat | Extract | Get | Set

type is_ac = bool

type t = 
  | True 
  | False
  | Void
  | Name of Hstring.t * is_ac
  | Int of Hstring.t
  | Real of Hstring.t
  | Bitv of string
  | Op of operator
  | Var of Hstring.t

val name : ?ac:is_ac -> string -> t
val var : string -> t
val underscoring : t -> t
val int : string -> t
val real : string -> t

val is_ac : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val to_string : t -> string
val print : Format.formatter -> t -> unit

val dummy : t
val fresh : string -> t
  
val is_get : t -> bool 
val is_set : t -> bool 


module Map : Map.S with type key = t
module Set : Set.S with type elt = t

