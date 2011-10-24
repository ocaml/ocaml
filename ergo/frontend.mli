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

module Time : sig

  val start: unit -> unit

  val get: unit -> float

end

type output = Unsat | Inconsistent | Sat | Unknown

(* this is final result what we want *)
type result = Valid | RUnsat | RUnknown | IDontKnow

val open_file:
  Why_ptree.file -> (Why_ptree.tdecl * Why_typing.env) list list 

val print_result : result -> unit

val askErgosrc: Why_ptree.file -> result

