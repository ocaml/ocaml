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

type t = Formula.Set.t option

let everything = None

let empty = Some (Formula.Set.empty)

let singleton l = Some (Formula.Set.singleton l)

let make sf = Some sf

let union d1 d2 = match d1,d2 with
    None , _ | _ , None -> None
  | Some s1 , Some s2 -> Some (Formula.Set.union s1 s2)

let remove f = function
    None -> None
  | Some s when Formula.Set.mem f s -> Some (Formula.Set.remove f s)
  | _ -> raise Not_found

let compare x y = match x,y with
  | Some sx, Some sy -> Formula.Set.compare sx sy
  | None, None -> 0
  | None, _ -> 1
  | _ -> -1
