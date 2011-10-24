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

open Why_ptree
open Format

type error = 
  | BitvExtract of int*int
  | BitvExtractRange of int*int
  | ClashType of string
  | ClashParam of string
  | TypeBadArityDecl
  | UnknownType of string
  | WrongArity of string * int
  | SymbAlreadyDefined of string 
  | SymbUndefined of string
  | NotAPropVar of string
  | NotAPredicate of string
  | Unification of Ty.t * Ty.t
  | ShouldBeApply of string
  | WrongNumberofArgs of string
  | ShouldHaveType of Ty.t * Ty.t
  | ShouldHaveTypeIntorReal of Ty.t
  | ShouldHaveTypeBitv of Ty.t
  | ArrayIndexShouldHaveTypeInt
  | ShouldHaveTypeArray
  | ShouldHaveTypeProp
  | Notrigger 
  | CannotGeneralize
  | SyntaxError

exception Error of error * loc
exception Warning of error * loc

let report fmt = function
  | BitvExtract(i,j) -> 
      fprintf fmt "bitvector extraction malformed (%d>%d)" i j
  | BitvExtractRange(n,j) -> 
      fprintf fmt "extraction out of range (%d>%d)" j n
  | ClashType s -> 
      fprintf fmt "the type %s is already defined" s
  | ClashParam s -> 
      fprintf fmt "parameter %s is bound twice" s
  | CannotGeneralize -> 
      fprintf fmt "cannot generalize the type of this expression"
  | TypeBadArityDecl -> 
      fprintf fmt "bad arity declaration"
  | UnknownType s -> 
      fprintf fmt "unknown type %s" s
  | WrongArity(s,n) -> 
      fprintf fmt "the type %s has %d arguments" s n
  | SymbAlreadyDefined s -> 
      fprintf fmt "the symbol %s is already defined" s
  | SymbUndefined s -> 
      fprintf fmt "undefined symbol %s" s
  | NotAPropVar s -> 
      fprintf fmt "%s is not a propositional variable" s
  | NotAPredicate s -> 
      fprintf fmt "%s is not a predicate" s
  | Unification(t1,t2) ->
      fprintf fmt "%a and %a cannot be unified" Ty.print t1 Ty.print t2
  | ShouldBeApply s -> 
      fprintf fmt "%s is a function symbol, it should be apply" s
  | WrongNumberofArgs s ->
      fprintf fmt "Wrong number of arguments when applying %s" s
  | ShouldHaveType(ty1,ty2) ->
      fprintf fmt "this expression has type %a but is here used with type %a"
	Ty.print ty1 Ty.print ty2
  | ShouldHaveTypeBitv t -> 
      fprintf fmt "this expression has type %a but it should be a bitvector"
	Ty.print t
  | ShouldHaveTypeIntorReal t ->
      fprintf fmt 
	"this expression has type %a but it should have type int or real"
	Ty.print t
  | ShouldHaveTypeArray ->
      fprintf fmt "this expression should have type farray"
  | ShouldHaveTypeProp -> 
      fprintf fmt "this expression should have type prop"
  | ArrayIndexShouldHaveTypeInt -> 
      fprintf fmt "index of arrays should hava type int"
  | Notrigger -> 
      fprintf fmt "No trigger for this lemma"
  | SyntaxError -> 
      fprintf fmt "syntax error"

let error e l = raise (Error(e,l))
let warning e l = raise (Warning(e,l))

let rec print_term fmt t = match t.tt_desc with
  | TTconst Ttrue -> 
      fprintf fmt "true"
  | TTconst Tfalse -> 
      fprintf fmt "false"
  | TTconst Tvoid -> 
      fprintf fmt "void"
  | TTconst (Tint n) -> 
      fprintf fmt "%s" n
  | TTconst (Treal n) -> 
      fprintf fmt "%s" (Num.string_of_num n)
  | TTconst Tbitv s -> 
      fprintf fmt "%s" s
  | TTvar s -> 
      fprintf fmt "%a" Symbols.print s
  | TTapp(s,l) -> 
      fprintf fmt "%a(%a)" Symbols.print s print_list l
  | TTinfix(t1,s,t2) -> 
      fprintf fmt "%a %a %a" print_term t1 Symbols.print s print_term t2
  | TTprefix (s, t') ->
      fprintf fmt "%a %a" Symbols.print s print_term t'
  | TTget (t1, t2) ->
      fprintf fmt "%a[%a]" print_term t1 print_term t2
  | TTset (t1, t2, t3) ->
      fprintf fmt "%a[%a<-%a]" print_term t1 print_term t2 print_term t3
  | TTextract (t1, t2, t3) ->
      fprintf fmt "%a^{%a,%a}" print_term t1 print_term t2 print_term t3
  | TTconcat (t1, t2) ->
      fprintf fmt "%a @ %a" print_term t1 print_term t2
  | TTlet (s, t1, t2) ->
      fprintf fmt "let %a=%a in %a" Symbols.print s print_term t1 print_term t2

and print_list fmt = List.iter (fprintf fmt "%a," print_term)
