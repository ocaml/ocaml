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

module T  : sig type t = Term.t end
module S  : sig type t = Symbols.t end
module ST : sig type elt = T.t type t = Term.Set.t end
module SA : sig type elt = Literal.t type t = Literal.Set.t end
  
type elt = ST.t * SA.t
    
module Make :
  functor (X : Sig.X) ->
sig
  
  type t 
  val empty : t
  val find : X.r -> t -> elt
  val add : X.r -> elt -> t -> t
  val mem : X.r -> t -> bool
  val print : t -> unit
  val up_add : t -> ST.elt -> X.r -> X.r list -> t
      
  val congr_add : t -> X.r list -> ST.t
  
  val up_close_up :t -> X.r -> X.r -> t
  val congr_close_up : t -> X.r -> X.r list -> elt
end
