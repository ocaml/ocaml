open Misc
open Path

open Asttypes

type primitive =
    Pidentity
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  | Pmakeblock of int
  | Ptagof
  | Pfield of int
  | Psetfield of int
  | Pccall of string * int
  | Pupdate
  | Praise
  | Psequand | Psequor | Pnot
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  | Pgetstringchar | Psetstringchar
  | Pvectlength | Pgetvectitem | Psetvectitem

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list
  | Lfunction of Ident.t * lambda
  | Llet of Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda * int) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * int * int * (int * lambda) list
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lshared of lambda * int option ref

let const_unit = Const_block(0, [])

let lambda_unit = Lconst const_unit

let share_lambda = function
    Lshared(_, _) as l -> l
  | l -> Lshared(l, ref None)

let name_lambda arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.new "let" in Llet(id, arg, fn id)

let free_variables l =
  let fv = ref Cset.empty in
  let rec freevars = function
    Lvar id ->
      fv := Cset.add id !fv
  | Lconst sc -> ()
  | Lapply(fn, args) ->
      freevars fn; List.iter freevars args
  | Lfunction(param, body) ->
      freevars body; fv := Cset.remove param !fv
  | Llet(id, arg, body) ->
      freevars arg; freevars body; fv := Cset.remove id !fv
  | Lletrec(decl, body) ->
      freevars body;
      List.iter (fun (id, exp, sz) -> freevars exp) decl;
      List.iter (fun (id, exp, sz) -> fv := Cset.remove id !fv) decl
  | Lprim(p, args) ->
      List.iter freevars args
  | Lswitch(arg, lo, hi, cases) ->
      freevars arg; List.iter (fun (key, case) -> freevars case) cases
  | Lstaticfail -> ()
  | Lcatch(e1, e2) ->
      freevars e1; freevars e2
  | Ltrywith(e1, exn, e2) ->
      freevars e1; freevars e2; fv := Cset.remove exn !fv
  | Lifthenelse(e1, e2, e3) ->
      freevars e1; freevars e2; freevars e3
  | Lsequence(e1, e2) ->
      freevars e1; freevars e2
  | Lwhile(e1, e2) ->
      freevars e1; freevars e2
  | Lfor(v, e1, e2, dir, e3) -> 
      freevars e1; freevars e2; freevars e3; fv := Cset.remove v !fv
  | Lshared(e, lblref) ->
      freevars e
  in freevars l; Cset.elements !fv

(* Check if an action has a "when" guard *)

let rec is_guarded = function
    Lifthenelse(cond, body, Lstaticfail) -> true
  | Lshared(lam, lbl) -> is_guarded lam
  | Llet(id, lam, body) -> is_guarded body
  | _ -> false

type compilenv = lambda Ident.tbl

let empty_env = Ident.empty

let add_env = Ident.add

let find_env = Ident.find_same

let transl_access env id =
  try
    find_env id env
  with Not_found ->
    if Ident.global id then Lprim(Pgetglobal id, []) else Lvar id

let rec transl_path = function
    Pident id ->
      if Ident.global id then Lprim(Pgetglobal id, []) else Lvar id
  | Pdot(p, s, pos) ->
      Lprim(Pfield pos, [transl_path p])

