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

open Hashcons

module S = 
  Hashcons.Make(struct include String 
		       let hash = Hashtbl.hash 
		       let equal = (=)     end)

type t = string Hashcons.hash_consed

let htable = S.create 17

let make s = S.hashcons htable s

let view s = s.node

let equal s1 s2 = s1.tag = s2.tag

let compare s1 s2 = compare s1.tag s2.tag

let hash s = s.tag

let empty = make ""
