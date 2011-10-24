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

type lemma = {
  vars: Symbols.Set.t;  (* toplevel quantified variables *)
  trs : Term.t list list; (* multi-triggers *)
  f : t;  (* the main lemma's formula *)
  name : string
}

and llet = {
  lvar: Symbols.t;
  lsubst : Term.t Ergo_subst.t;
  lsubst_ty : Ty.subst;
  lterm : Term.t;
  lf : t;
}

and ssko = {
  ssubst : Term.t Ergo_subst.t;
  ssubst_ty : Ty.subst;
  sf : t;
}

and view = 
    U of t list   (* unit clauses *)
  | C of t*t      (* a clause (t1 or t2) *)
  | Lit of Literal.t   (* an atom *)
  | Lem of lemma   (* a lemma *)
  | Sko of ssko  (* lazy substitution *)
  | Let of llet (* a binding of a term *)

val mk_not : t -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t
val mk_imp : t -> t -> t
val mk_if : Term.t -> t -> t -> t
val mk_iff : t -> t -> t
val mk_lit : Literal.t -> t
val mk_forall : Term.Set.t -> Term.Set.t -> Term.t list list -> t -> string -> t
val mk_exists : Term.Set.t -> Term.Set.t -> Term.t list list -> t -> string -> t
val mk_let : Term.Set.t -> Symbols.t -> Term.t -> t -> t

val add_label : Hstring.t -> t -> unit
val label : t -> Hstring.t

val view : t -> view
val size : t -> int

val print : Format.formatter -> t -> unit

val terms : t -> Term.Set.t
val free_vars : t -> Symbols.Set.t

val apply_subst : Term.subst -> t -> t 

val compare : t -> t -> int
    
module Set : Set.S with type elt = t
module Map : Map.S with type key = t

