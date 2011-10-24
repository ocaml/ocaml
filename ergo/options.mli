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

val fmt : Format.formatter
(* val file : string ref *)

val parse_only : bool
val type_only : bool
val stopb : int
val age_limite : int
val notriggers : bool
val debug : bool
val debug_cc : bool
val debug_use : bool
val debug_uf : bool
val debug_fm : bool
val debug_arith : bool
val debug_bitv : bool
val debug_ac : bool
val debug_sat : bool
val debug_sat_simple : bool
val debug_typing : bool
val debug_constr : bool
val debug_pairs : bool
val debug_finite : bool
val debug_arrays : bool
val debug_combine : bool
val verbose : bool
val debug_dispatch : bool
val tracefile :string
val smtfile :bool ref
val satmode : bool ref
val bjmode : bool
val glouton : bool
val triggers_var : bool
val redondance : int
val astuce : bool
val select : int
(* val cin : in_channel *)
val no_rm_eq_existential : bool
val nocontracongru : bool
val finitetest : bool
val omega : bool
val arrays : bool
val pairs : bool
val term_like_pp : bool
val debug_types : bool
val all_models : bool
val smt_arrays : bool
val goal_directed : bool
val bouclage : int
