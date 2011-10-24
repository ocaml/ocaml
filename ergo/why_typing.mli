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

type env

val file : Why_ptree.file -> (Why_ptree.tdecl * env) list 

val split_goals : 
  (Why_ptree.tdecl * env) list -> (Why_ptree.tdecl * env) list list

val term : env -> (Symbols.t * Ty.t) list -> Why_ptree.lexpr -> Why_ptree.tterm

val print_file : Format.formatter -> Why_ptree.file -> unit

val print_tdecl : Format.formatter -> Why_ptree.tdecl -> unit
