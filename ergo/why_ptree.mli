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

(* type loc = Lexing.position * Lexing.position *)

type loc = Location.t

type constant =
  | ConstBitv of string
  | ConstInt of string
  | ConstReal of Num.num
  | ConstTrue
  | ConstFalse
  | ConstVoid

type pp_infix = 
  | PPand | PPor | PPimplies | PPiff 
  | PPlt | PPle | PPgt | PPge | PPeq | PPneq
  | PPadd | PPsub | PPmul | PPdiv | PPmod
	  
type pp_prefix = 
  | PPneg | PPnot

type ppure_type =
  | PPTint
  | PPTbool
  | PPTreal
  | PPTunit
  | PPTbitv of int
  | PPTvarid of string * loc
  | PPTexternal of ppure_type list * string * loc
  | PPTfarray of ppure_type

type lexpr = 
  { pp_loc : loc; pp_desc : pp_desc }

and pp_desc =
  | PPvar of string
  | PPapp of string * lexpr list
  | PPconst of constant
  | PPinfix of lexpr * pp_infix * lexpr
  | PPprefix of pp_prefix * lexpr
  | PPget of lexpr * lexpr
  | PPset of lexpr * lexpr * lexpr
  | PPextract of lexpr * lexpr * lexpr
  | PPconcat of lexpr * lexpr
  | PPif of lexpr * lexpr * lexpr
  | PPforall of string list* ppure_type * lexpr list list * lexpr
  | PPexists of string * ppure_type * lexpr
  | PPnamed of string * lexpr
  | PPlet of string * lexpr * lexpr

(* Declarations. *)

type plogic_type =
  | PPredicate of ppure_type list
  | PFunction of ppure_type list * ppure_type

type is_ac = Symbols.is_ac

type decl = 
  | Axiom of loc * string * lexpr
  | Goal of loc * string * lexpr
  | Logic of loc * is_ac * string list * plogic_type
  | Predicate_def of loc * string * (loc * string * ppure_type) list * lexpr
  | Function_def 
      of loc * string * (loc * string * ppure_type) list * ppure_type * lexpr
  | TypeDecl of loc * string list * string

type file = decl list

(*** typed ast *)

type tconstant =
  | Tint of string
  | Treal of Num.num
  | Tbitv of string
  | Ttrue
  | Tfalse
  | Tvoid

type tterm = 
    { tt_ty : Ty.t; tt_desc : tt_desc }
and tt_desc = 
  | TTconst of tconstant
  | TTvar of Symbols.t
  | TTinfix of tterm * Symbols.t * tterm
  | TTprefix of Symbols.t * tterm 
  | TTapp of Symbols.t * tterm list
  | TTget of tterm * tterm
  | TTset of tterm * tterm * tterm
  | TTextract of tterm * tterm * tterm
  | TTconcat of tterm * tterm
  | TTlet of Symbols.t * tterm * tterm

type tatom = 
  | TAtrue
  | TAfalse
  | TAeq of tterm list
  | TAneq of tterm list
  | TAle of tterm list
  | TAlt of tterm list
  | TApred of tterm
  | TAbuilt of Hstring.t * tterm list

type oplogic = OPand |OPor | OPimp | OPnot | OPif of tterm | OPiff 

type quant_form = {       
  (* quantified variables that appear in the formula *)
  qf_bvars : (Symbols.t * Ty.t) list ;

  qf_upvars : (Symbols.t * Ty.t) list ;

  qf_triggers : tterm list list ;
  qf_form : tform
}

and tform =
  | TFatom of tatom
  | TFop of oplogic * tform list
  | TFforall of quant_form
  | TFexists of quant_form
  | TFlet of (Symbols.t * Ty.t) list * Symbols.t * tterm * tform
  | TFnamed of Hstring.t * tform


type tdecl = 
  | TAxiom of loc * string * tform
  | TGoal of loc * string * tform
  | TLogic of loc * string list * plogic_type
  | TPredicate_def of loc * string * (string * ppure_type) list * tform
  | TFunction_def 
      of loc * string * (string * ppure_type) list * ppure_type * tform
  | TTypeDecl of loc * string list * string


(* Sat entry *)

type sat_decl_aux = 
  | Assume of Formula.t * bool 
  | PredDef of Formula.t
  | Query of string * Formula.t * Literal.t list

type sat_tdecl = {
  st_loc : loc;
  st_decl : sat_decl_aux
}
