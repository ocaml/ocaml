(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Path
open Asttypes
open Typedtree

type primitive =
    Pidentity
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Bitvect operations *)
  | Pbittest

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list

type let_kind = Strict | Alias | StrictOpt

type shared_code = (int * int) list

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list
  | Lfunction of Ident.t list * lambda
  | Llet of let_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of lambda * lambda * lambda list

and lambda_switch =
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_checked: bool }

let const_unit = Const_pointer 0

let lambda_unit = Lconst const_unit

let name_lambda arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in Llet(Strict, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar id as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create "let" in
      Llet(Strict, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args

module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let free_variables l =
  let fv = ref IdentSet.empty in
  let rec freevars = function
    Lvar id ->
      fv := IdentSet.add id !fv
  | Lconst sc -> ()
  | Lapply(fn, args) ->
      freevars fn; List.iter freevars args
  | Lfunction(params, body) ->
      freevars body;
      List.iter (fun param -> fv := IdentSet.remove param !fv) params
  | Llet(str, id, arg, body) ->
      freevars arg; freevars body; fv := IdentSet.remove id !fv
  | Lletrec(decl, body) ->
      freevars body;
      List.iter (fun (id, exp) -> freevars exp) decl;
      List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
  | Lprim(p, args) ->
      List.iter freevars args
  | Lswitch(arg, sw) ->
      freevars arg; 
      List.iter (fun (key, case) -> freevars case) sw.sw_consts;
      List.iter (fun (key, case) -> freevars case) sw.sw_blocks
  | Lstaticfail -> ()
  | Lcatch(e1, e2) ->
      freevars e1; freevars e2
  | Ltrywith(e1, exn, e2) ->
      freevars e1; freevars e2; fv := IdentSet.remove exn !fv
  | Lifthenelse(e1, e2, e3) ->
      freevars e1; freevars e2; freevars e3
  | Lsequence(e1, e2) ->
      freevars e1; freevars e2
  | Lwhile(e1, e2) ->
      freevars e1; freevars e2
  | Lfor(v, e1, e2, dir, e3) -> 
      freevars e1; freevars e2; freevars e3; fv := IdentSet.remove v !fv
  | Lassign(id, e) ->
      fv := IdentSet.add id !fv; freevars e
  | Lsend (met, obj, args) ->
      List.iter freevars (met::obj::args)
  in freevars l; !fv

(* Check if an action has a "when" guard *)

let rec is_guarded = function
    Lifthenelse(cond, body, Lstaticfail) -> true
  | Llet(str, id, lam, body) -> is_guarded body
  | _ -> false

let rec transl_path = function
    Pident id ->
      if Ident.global id then Lprim(Pgetglobal id, []) else Lvar id
  | Pdot(p, s, pos) ->
      Lprim(Pfield pos, [transl_path p])
  | Papply(p1, p2) ->
      fatal_error "Lambda.transl_path"
