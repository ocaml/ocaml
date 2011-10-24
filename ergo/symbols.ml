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

type operator = 
    Plus | Minus | Mult | Div | Modulo 
  | Concat | Extract | Get | Set

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

let name ?(ac=false) s = Name (Hstring.make s, ac)
let var s = Var (Hstring.make s)
let int i = Int (Hstring.make i)
let real r = Real (Hstring.make r)

let is_ac = function
    Name(_, true) -> true
  | _ -> false

let underscoring = function
    Var s -> Var (Hstring.make ("$"^Hstring.view s))
  | _ -> assert false

let compare s1 s2 =  match s1,s2 with
    Name (n1,ac1) , Name (n2,ac2) -> 
      let c = Pervasives.compare ac1 ac2 in
      if c = 0 then Hstring.compare n1 n2 else c
  | Name _ , _ ->  -1
  | _ , Name _ -> 1
  | Var n1 , Var n2 -> Hstring.compare n1 n2
  | Var _ , _ -> -1
  | _ , Var _ -> 1
  | Int i1 , Int i2 -> Hstring.compare i1 i2
  | Int _ , _ -> -1
  | _ , Int _ -> 1
  | _  -> Pervasives.compare s1 s2
  
let equal s1 s2 = compare s1 s2 = 0

let hash = function
  | Name (n,true) -> Hstring.hash n * 19 + 1
  | Name (n,false) -> Hstring.hash n * 19
  | Var n (*| Int n*) -> Hstring.hash n * 19 + 1
  | s -> Hashtbl.hash s
	
let to_string =  function
    Name (n,_) -> Hstring.view n
  | Var x -> (Hstring.view x)
  | Int n -> Hstring.view n
  | Real n -> Hstring.view n
  | Bitv s -> "[|"^s^"|]"
  | Op Plus -> "+" 
  | Op Minus -> "-" 
  | Op Mult -> "*"
  | Op Div -> "/"
  | Op Modulo -> "%"
  | True -> "true"
  | False -> "false"
  | Void -> "void"
  | _ -> "" (*assert false*)

let print fmt s = Format.fprintf fmt "%s" (to_string s)

let dummy = Name (Hstring.make "_one", false)

let fresh = 
  let cpt = ref 0 in
  fun s -> incr cpt; name (Format.sprintf "__%s%i" s (!cpt))

let is_get f = equal f (Op Get) 
let is_set f = equal f (Op Set)


module Map =
  Map.Make(struct type t' = t type t=t' let compare=compare end)

module Set = 
  Set.Make(struct type t' = t type t=t' let compare=compare end)

