open Path

open Asttypes

type primitive =
    Pidentity
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  | Pmakeblock of int
  | Pfield of int
  | Psetfield of int * bool
  | Pccall of string * int * bool
  | Praise
  | Psequand | Psequor | Pnot
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  | Pstringlength | Pgetstringchar | Psetstringchar
  | Psafegetstringchar | Psafesetstringchar
  | Pvectlength | Pgetvectitem | Psetvectitem of bool
  | Psafegetvectitem | Psafesetvectitem of bool
  | Ptranslate of (int * int * int) array

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list
  | Const_pointer of int

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list
  | Lfunction of Ident.t * lambda
  | Llet of Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * int * (int * lambda) list * int * (int * lambda) list
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lshared of lambda * int option ref

let const_unit = Const_base(Const_int 0)

let lambda_unit = Lconst const_unit

let share_lambda = function
    Lshared(_, _) as l -> l
  | l -> Lshared(l, ref None)

let name_lambda arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.new "let" in Llet(id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar id as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.new "let" in
      Llet(id, arg, name_list (Lvar id :: names) rem) in
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
  | Lfunction(param, body) ->
      freevars body; fv := IdentSet.remove param !fv
  | Llet(id, arg, body) ->
      freevars arg; freevars body; fv := IdentSet.remove id !fv
  | Lletrec(decl, body) ->
      freevars body;
      List.iter (fun (id, exp) -> freevars exp) decl;
      List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
  | Lprim(p, args) ->
      List.iter freevars args
  | Lswitch(arg, num_cases1, cases1, num_cases2, cases2) ->
      freevars arg; 
      List.iter (fun (key, case) -> freevars case) cases1;
      List.iter (fun (key, case) -> freevars case) cases2
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
  | Lshared(e, lblref) ->
      freevars e
  in freevars l; !fv

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

