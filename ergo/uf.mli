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

module type S = sig
  type t

  module R : Sig.X

  val empty :  t
  val add : t -> Term.t -> t * Literal.t list

  val add_semantic : t -> R.r -> t 

  val mem : t -> Term.t -> bool

  val find : t -> Term.t -> R.r
  val find_r : t -> R.r -> R.r

  val union : 
    t -> R.r -> R.r -> Explanation.t -> t * (R.r * (R.r * R.r) list * R.r) list
  
  val distinct : t -> Term.t -> Term.t -> Explanation.t -> t

  val equal : t -> Term.t -> Term.t -> bool
  val are_distinct : t -> Term.t -> Term.t -> bool
  val class_of : t -> Term.t -> Term.t list
    
  val explain : t -> Term.t -> Term.t -> Explanation.t
  val neq_explain : t -> Term.t -> Term.t -> Explanation.t
  
  val print : Format.formatter -> t -> unit

  val distinct_r : 
    t -> R.r -> R.r -> Explanation.t -> Explanation.t -> Explanation.t -> t
end

module Make ( X : Sig.X ) : S with module R = X
