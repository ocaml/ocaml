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

type gsubst = { sbt : Term.subst ; gen : int ; goal : bool}

type pat_info = {
  pat_age : int ; 
  pat_orig : Formula.t ; 
  pat_formula : Formula.t ; 
  pat_dep : Explanation.t ;
}

type term_info = {
  term_age : int ; 
  term_from_goal : bool ;
  term_orig : Formula.t option
}

module type X = sig
  type t

  val class_of : t -> Term.t -> Term.t list
  val query : Literal.t -> t -> bool
end

module type S = sig
  type t
  type uf

  val empty : t
  val add_term : term_info -> Term.t -> t -> t 
  val add_pat : pat_info * Term.t list -> t -> uf -> t
  val query : t -> uf -> (pat_info * gsubst list) list

end

module Make (X : X) : S with type uf = X.t
