(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation of pattern matching *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Parmatch
open Printf
open Printpat


let dbg = false

(*  See Peyton-Jones, ``The Implementation of functional programming
    languages'', chapter 5. *)
(*
  Well, it was true at the beginning of the world.
  Now, see Lefessant-Maranget ``Optimizing Pattern-Matching'' ICFP'2001
*)

type row = pattern list
type 'a case = 'a * Lambda.block
type 'a cases = 'a case list

(*
   Compatibility predicate that considers potential rebindings of constructors
   of an extension type.

   "may_compat p q" returns false when p and q never admit a common instance;
   returns true when they may have a common instance.
*)

module MayCompat =
  Parmatch.Compat (struct let equal = Types.may_equal_constr end)
let may_compat = MayCompat.compat
and may_compats = MayCompat.compats

(*
   Many functions on the various data structures of the algorithm :
     - Pattern matrices.
     - Default environments: mapping from matrices to exit numbers.
     - Contexts:  matrices whose column are partitioned into
       left and right.
     - Jump summaries: mapping from exit numbers to contexts
*)


let string_of_lam lam =
  Printlambda.lambda Format.str_formatter lam ;
  Format.flush_str_formatter ()

let string_of_block { block_loc = _; expr; } =
  string_of_lam expr

let bind_with_value_kind let_kind id_and_value_kind defining_expr body =
  let expr =
    Lambda.bind_with_value_kind let_kind id_and_value_kind
      defining_expr body.expr
  in
  Lambda.block body.block_loc expr

let omega pat_loc = { omega with pat_loc }
let omegas pat_loc n = List.map (fun o -> { o with pat_loc }) (omegas n)
let omega_list = List.map (fun p -> omega p.pat_loc)

let all_record_args lbls = match lbls with
| (_,{lbl_all=lbl_all},pat)::_ ->
    let loc = pat.pat_loc in
    let t =
      Array.map
        (fun lbl -> mkloc (Longident.Lident "?temp?") loc, lbl, omega loc)
        lbl_all in
    List.iter
      (fun ((_, lbl,_) as x) ->  t.(lbl.lbl_pos) <- x)
      lbls ;
    Array.to_list t
|  _ -> fatal_error "Parmatch.all_record_args"

type matrix = row list

let add_omega_column pss =
  List.map (fun ps ->
      let loc =
        match ps with
        | [] -> Location.none
        | p::_ -> p.pat_loc
      in
      (omega loc) :: ps)
    pss

type ctx = {left:row ; right:row}

let pretty_ctx ctx =
  List.iter
    (fun {left=left ; right=right} ->
      Format.eprintf "LEFT:%a RIGHT:%a\n" pretty_line left pretty_line right)
    ctx

let le_ctx c1 c2 =
  le_pats c1.left c2.left &&
  le_pats c1.right c2.right

let lshift {left=left ; right=right} = match right with
| x::xs -> {left=x::left ; right=xs}
| _ ->  assert false

let lforget {left=left ; right=right} = match right with
| x::xs -> {left=omega x.pat_loc::left ; right=xs}
|  _ -> assert false

let rec small_enough n = function
  | [] -> true
  | _::rem ->
      if n <= 0 then false
      else small_enough (n-1) rem

let ctx_lshift ctx =
  if small_enough (!Clflags.match_context_rows - 1) ctx then
    List.map lshift ctx
  else (* Context pruning *) begin
    get_mins le_ctx (List.map lforget ctx)
  end

let  rshift {left=left ; right=right} = match left with
| p::ps -> {left=ps ; right=p::right}
| _ -> assert false

let ctx_rshift ctx = List.map rshift ctx

let rec nchars n ps =
  if n <= 0 then [],ps
  else match ps with
  | p::rem ->
    let chars, cdrs = nchars (n-1) rem in
    p::chars,cdrs
  | _ -> assert false

let  rshift_num n {left=left ; right=right} =
  let shifted,left = nchars n left in
  {left=left ; right = shifted@right}

let ctx_rshift_num n ctx = List.map (rshift_num n) ctx

(* Recombination of contexts (eg: (_,_)::p1::p2::rem ->  (p1,p2)::rem)
  All mutable fields are replaced by '_', since side-effects in
  guards can alter these fields *)

let combine {left=left ; right=right} = match left with
| p::ps -> {left=ps ; right=set_args_erase_mutable p right}
| _ -> assert false

let ctx_combine ctx = List.map combine ctx

let ncols = function
  | [] -> 0
  | ps::_ -> List.length ps


exception NoMatch
exception OrPat

let filter_matrix matcher pss =

  let rec filter_rec = function
    | (p::ps)::rem ->
        begin match p.pat_desc with
        | Tpat_alias (p,_,_) ->
            filter_rec ((p::ps)::rem)
        | Tpat_var _ ->
            filter_rec ((omega p.pat_loc::ps)::rem)
        | _ ->
            begin
              let rem = filter_rec rem in
              try
                matcher p ps::rem
              with
              | NoMatch -> rem
              | OrPat   ->
                match p.pat_desc with
                | Tpat_or (p1,p2,_) -> filter_rec [(p1::ps) ;(p2::ps)]@rem
                | _ -> assert false
            end
        end
    | [] -> []
    | _ ->
        pretty_matrix Format.err_formatter pss ;
        fatal_error "Matching.filter_matrix" in
  filter_rec pss

let make_default matcher env =
  let rec make_rec = function
    | [] -> []
    | ([[]],i)::_ -> [[[]],i]
    | (pss,i)::rem ->
        let rem = make_rec rem in
        match filter_matrix matcher pss with
        | [] -> rem
        | ([]::_) -> ([[]],i)::rem
        | pss -> (pss,i)::rem in
  make_rec env

let ctx_matcher p =
  let p = normalize_pat p in
  match p.pat_desc with
  | Tpat_construct (_, cstr,omegas) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_construct (_, cstr',args)
(* NB:  may_constr_equal considers (potential) constructor rebinding *)
        when Types.may_equal_constr cstr cstr' ->
          p,args@rem
      | Tpat_any -> p,omegas @ rem
      | _ -> raise NoMatch)
  | Tpat_constant cst ->
      (fun q rem -> match q.pat_desc with
      | Tpat_constant cst' when const_compare cst cst' = 0 ->
          p,rem
      | Tpat_any -> p,rem
      | _ -> raise NoMatch)
  | Tpat_variant (lab,Some omega,_) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_variant (lab',Some arg,_) when lab=lab' ->
          p,arg::rem
      | Tpat_any -> p,omega::rem
      | _ -> raise NoMatch)
  | Tpat_variant (lab,None,_) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_variant (lab',None,_) when lab=lab' ->
          p,rem
      | Tpat_any -> p,rem
      | _ -> raise NoMatch)
  | Tpat_array omegas ->
      let len = List.length omegas in
      (fun q rem -> match q.pat_desc with
      | Tpat_array args when List.length args = len -> p,args @ rem
      | Tpat_any -> p, omegas @ rem
      | _ -> raise NoMatch)
  | Tpat_tuple omegas ->
      let len = List.length omegas  in
      (fun q rem -> match q.pat_desc with
      | Tpat_tuple args when List.length args = len -> p,args @ rem
      | Tpat_any -> p, omegas @ rem
      | _ -> raise NoMatch)
  | Tpat_record (((_, lbl, _) :: _) as l,_) -> (* Records are normalized *)
      let len = Array.length lbl.lbl_all in
      (fun q rem -> match q.pat_desc with
      | Tpat_record (((_, lbl', _) :: _) as l',_)
        when Array.length lbl'.lbl_all = len ->
          let l' = all_record_args l' in
          p, List.fold_right (fun (_, _,p) r -> p::r) l' rem
      | Tpat_any -> p,List.fold_right (fun (_, _,p) r -> p::r) l rem
      | _ -> raise NoMatch)
  | Tpat_lazy omega ->
      (fun q rem -> match q.pat_desc with
      | Tpat_lazy arg -> p, (arg::rem)
      | Tpat_any      -> p, (omega::rem)
      | _             -> raise NoMatch)
 | _ -> fatal_error "Matching.ctx_matcher"




let filter_ctx q ctx =

  let matcher = ctx_matcher q in

  let rec filter_rec = function
    | ({right=p::ps} as l)::rem ->
        begin match p.pat_desc with
        | Tpat_or (p1,p2,_) ->
            filter_rec ({l with right=p1::ps}::{l with right=p2::ps}::rem)
        | Tpat_alias (p,_,_) ->
            filter_rec ({l with right=p::ps}::rem)
        | Tpat_var _ ->
            filter_rec ({l with right=omega p.pat_loc::ps}::rem)
        | _ ->
            begin let rem = filter_rec rem in
            try
              let to_left, right = matcher p ps in
              {left=to_left::l.left ; right=right}::rem
            with
            | NoMatch -> rem
            end
        end
    | [] -> []
    | _ ->  fatal_error "Matching.filter_ctx" in

  filter_rec ctx

let select_columns pss ctx =
  let n = ncols pss in
  List.fold_right
    (fun ps r ->
      List.fold_right
        (fun {left=left ; right=right} r ->
          let transfert, right = nchars n right in
          try
            {left = lubs transfert ps @ left ; right=right}::r
          with
          | Empty -> r)
        ctx r)
    pss []

let ctx_lub p ctx =
  List.fold_right
    (fun {left=left ; right=right} r ->
      match right with
      | q::rem ->
          begin try
            {left=left ; right = lub p q::rem}::r
          with
          | Empty -> r
          end
      | _ -> fatal_error "Matching.ctx_lub")
    ctx []

let ctx_match ctx pss =
  List.exists
    (fun {right=qs} ->  List.exists (fun ps -> may_compats qs ps)  pss)
    ctx

module Jumps : sig
  type t

  val print_stderr : t -> unit

  val empty : t
  val is_empty : t -> bool

  val singleton : int -> ctx list -> t
  val add : int -> ctx list -> t -> t

  val union : t -> t -> t
  val union_list : t list -> t

  val extract : int -> t -> ctx list * t
  val remove : int -> t -> t

  val map : (ctx list -> ctx list) -> t -> t
end = struct
  type t = (int * ctx list) list

  let print_stderr t =
    match t with
    | [] -> ()
    | _ ->
        List.iter
          (fun (i, ctx) ->
            Printf.fprintf stderr "jump for %d\n" i;
            pretty_ctx ctx)
          t

  let rec extract i ctx =
    match ctx with
    | [] -> [], []
    | ((j, pss) as x :: rem) as all ->
        if i = j then pss, rem
        else if j < i then [], all
        else
          let r, rem = extract i rem in
          r, (x :: rem)

  let rec remove i ctx =
    match ctx with
    | [] -> []
    | (j, _) :: rem when i = j -> rem
    | x :: rem -> x :: remove i rem

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let singleton i ctx =
    match ctx with
    | [] -> []
    | ctx -> [i, ctx]

  let add i pss jumps =
    match pss with
    | [] -> jumps
    | _ ->
        let rec add = function
          | [] -> [i, pss]
          | ((j, qss) as x :: rem) as all ->
              if j > i then x :: add rem
              else if j < i then (i, pss) :: all
              else (i, (get_mins le_ctx (pss @ qss))) :: rem
        in
        add jumps

  let rec union t1 t2 =
    match t1, t2 with
    | [], _ -> t2
    | _, [] -> t1
    | ((i1, pss1) as x1 :: rem1), ((i2, pss2) as x2 :: rem2) ->
        if i1 = i2 then
          (i1, get_mins le_ctx (pss1 @ pss2)) :: union rem1 rem2
        else if i1 > i2 then
          x1 :: union rem1 t2
        else
          x2 :: union t1 rem2

  let rec merge = function
    | t1 :: t2 :: rem -> (union t1 t2) :: merge rem
    | ts -> ts

  let rec union_list ts =
    match ts with
    | [] -> []
    | [t] -> t
    | _ -> union_list (merge ts)

  let map f t =
    List.map (fun (i, pss) -> i, f pss) t
end

(* Pattern matching before any compilation *)

type args = (lambda * let_kind * Location.t) list

type pattern_matching =
  { mutable cases : row cases;
    args : args;
    default : (matrix * int) list}

(* Pattern matching after application of both the or-pat rule and the
   mixture rule *)

type pm_or_compiled =
  {body : pattern_matching ;
   handlers :
     (matrix * int * (Ident.t * Lambda.value_kind) list * pattern_matching)
       list;
   or_matrix : matrix ;
   loc : Location.t;
  }

type pm_half_compiled =
  | PmOr of pm_or_compiled
  | PmVar of pm_var_compiled
  | Pm of pattern_matching

and pm_var_compiled =
    {inside : pm_half_compiled ; var_arg : lambda ; }

type pm_half_compiled_info =
    {me : pm_half_compiled ;
     matrix : matrix ;
     top_default : (matrix * int) list ; }

let pretty_cases cases =
  List.iter
    (fun (ps, _act) ->
      List.iter
        (fun p -> Format.eprintf " %a%!" top_pretty p)
        ps ;
      Format.eprintf "\n")
    cases

let pretty_def def =
  Format.eprintf "+++++ Defaults +++++\n" ;
  List.iter
    (fun (pss,i) -> Format.eprintf "Matrix for %d\n%a" i pretty_matrix pss)
    def ;
  Format.eprintf "+++++++++++++++++++++\n"

let pretty_pm pm =
  pretty_cases pm.cases ;
  if pm.default <> [] then
    pretty_def pm.default


let rec pretty_precompiled = function
  | Pm pm ->
      Format.eprintf "++++ PM ++++\n" ;
      pretty_pm pm
  | PmVar x ->
      Format.eprintf "++++ VAR ++++\n" ;
      pretty_precompiled x.inside
  | PmOr x ->
      Format.eprintf "++++ OR ++++\n" ;
      pretty_pm x.body ;
      pretty_matrix Format.err_formatter x.or_matrix ;
      List.iter
        (fun (_,i,_,pm) ->
          eprintf "++ Handler %d ++\n" i ;
          pretty_pm pm)
        x.handlers

let pretty_precompiled_res first nexts =
  pretty_precompiled first ;
  List.iter
    (fun (e, pmh) ->
      eprintf "** DEFAULT %d **\n" e ;
      pretty_precompiled pmh)
    nexts



(* Identifying some semantically equivalent lambda-expressions,
   Our goal here is also to
   find alpha-equivalent (simple) terms *)

(* However, as shown by PR#6359 such sharing may hinders the
   lambda-code invariant that all bound idents are unique,
   when switches are compiled to test sequences.
   The definitive fix is the systematic introduction of exit/catch
   in case action sharing is present.
*)

module StoreExp =
  Switch.Store
    (struct
      type t = Lambda.block
      type key = Lambda.lambda
      let compare_key = Stdlib.compare
      let make_key act = Lambda.make_key act.expr
    end)

let make_exit block_loc cont : Lambda.block =
  Lambda.block block_loc (Lstaticraise (cont, []))

(* Introduce a catch, if worth it *)
let make_catch d (k : Lambda.block -> _) =
  match d.expr with
  | Lstaticraise (_,[]) -> k d
  | _ ->
    let cont = next_raise_count () in
    Lstaticcatch (k (make_exit d.block_loc cont),(cont,[]),d)

(* Introduce a catch, if worth it, delayed version *)
let rec as_simple_exit block =
  as_simple_exit_expr block.expr

and as_simple_exit_expr expr =
  match expr with
  | Lstaticraise (i,[]) -> Some i
  | Llet (Alias,_k,_,_,e) -> as_simple_exit_expr e
  | _ -> None

let make_catch_delayed (handler : Lambda.block) =
  match as_simple_exit handler with
  | Some i -> i, (fun act -> act)
  | None ->
      let i = next_raise_count () in
  (*
      Printf.eprintf "SHARE LAMBDA: %i\n%s\n" i (string_of_lam handler);
  *)
      i,
      (fun body ->
        match body.expr with
        | Lstaticraise (j,_) -> if i=j then handler else body
        | _ ->
          Lambda.block body.block_loc
            (Lstaticcatch (body.expr, (i, []), handler)))

let raw_action l =
  match make_key l with | Some l -> l | None -> l

let tr_raw act =
  match make_key act.expr with
  | Some expr -> Lambda.block act.block_loc expr
  | None -> raise Exit

let same_actions = function
  | [] -> None
  | [_, act] -> Some act
  | (_, act0) :: rem ->
      try
        let raw_act0 = tr_raw act0 in
        let rec s_rec = function
          | [] -> Some act0
          | (_, act)::rem ->
              if raw_act0 = tr_raw act then
                s_rec rem
              else
                None in
        s_rec rem
      with
      | Exit -> None


(* Test for swapping two clauses *)

let up_ok_action act1 act2 =
  try
    let raw1 = tr_raw act1
    and raw2 = tr_raw act2 in
    raw1 = raw2
  with
  | Exit -> false

let up_ok (ps, act_p) (l : row cases) =
  List.for_all
    (fun (qs, act_q) ->
      up_ok_action act_p act_q || not (may_compats ps qs))
    l

(*
   The simplify function normalizes the first column of the match
     - records are expanded so that they possess all fields
     - aliases are removed and replaced by bindings in actions.
   However or-patterns are simplified differently,
     - aliases are not removed
     - or-patterns (_|p) are changed into _
*)

exception Var of pattern

let simplify_or p =
  let rec simpl_rec p = match p with
    | {pat_desc = Tpat_any|Tpat_var _} -> raise (Var p)
    | {pat_desc = Tpat_alias (q,id,s)} ->
        begin try
          {p with pat_desc = Tpat_alias (simpl_rec q,id,s)}
        with
        | Var q -> raise (Var {p with pat_desc = Tpat_alias (q,id,s)})
        end
    | {pat_desc = Tpat_or (p1,p2,o)} ->
        let q1 = simpl_rec p1 in
        begin try
          let q2 = simpl_rec p2 in
          {p with pat_desc = Tpat_or (q1, q2, o)}
        with
        | Var q2 -> raise (Var {p with pat_desc = Tpat_or (q1, q2, o)})
        end
    | {pat_desc = Tpat_record (lbls,closed)} ->
        let all_lbls = all_record_args lbls in
        {p with pat_desc=Tpat_record (all_lbls, closed)}
    | _ -> p in
  try
    simpl_rec p
  with
  | Var p -> p

let simplify_cases (args : args) cls : row cases =
  match args with
  | [] -> assert false
  | (arg, _mut, _arg_loc)::_ ->
    let rec simplify (cases : row cases) : row cases =
      match cases with
      | [] -> []
      | ((pat :: patl, action) as cl) :: rem ->
          begin match pat.pat_desc with
          | Tpat_var (id, _) ->
              let k = Typeopt.value_kind pat.pat_env pat.pat_type in
              (omega pat.pat_loc :: patl,
                bind_with_value_kind Alias (id, k) arg action) ::
              simplify rem
          | Tpat_any ->
              cl :: simplify rem
          | Tpat_alias(p, id,_) ->
              let k = Typeopt.value_kind pat.pat_env pat.pat_type in
              simplify ((p :: patl,
                           bind_with_value_kind Alias (id, k) arg action)
                        :: rem)
          | Tpat_record ([],_) ->
              (omega pat.pat_loc :: patl, action) :: simplify rem
          | Tpat_record (lbls, closed) ->
              let all_lbls = all_record_args lbls in
              let full_pat =
                {pat with pat_desc=Tpat_record (all_lbls, closed)} in
              (full_pat::patl, action) :: simplify rem
          | Tpat_or _ ->
              let pat_simple  = simplify_or pat in
              begin match pat_simple.pat_desc with
              | Tpat_or _ ->
                  (pat_simple :: patl, action) :: simplify rem
              | _ ->
                  simplify ((pat_simple::patl, action) :: rem)
              end
          | _ -> cl :: simplify rem
          end
      | _ -> assert false
    in
    simplify cls

(* Once matchings are simplified one can easily find
   their nature *)

let rec what_is_cases loc (cases : row cases) = match cases with
| ({pat_desc=Tpat_any} :: _, _act) :: rem -> what_is_cases loc rem
| (({pat_desc=(Tpat_var _|Tpat_or (_,_,_)|Tpat_alias (_,_,_))}::_), _act)::_
  -> assert false (* applies to simplified matchings only *)
| (p::_, _act)::_ -> p
| [] -> omega loc
| _ -> assert false



(* A few operations on default environments *)
let as_matrix cases =
  get_mins le_pats (List.map (fun (ps, _act) -> ps) cases)

let cons_default matrix raise_num default =
  match matrix with
  | [] -> default
  | _ -> (matrix,raise_num)::default

let default_compat p def =
  List.fold_right
    (fun (pss,i) r ->
      let qss =
        List.fold_right
          (fun qs r -> match qs with
            | q::rem when may_compat p q -> rem::r
            | _ -> r)
          pss [] in
      match qss with
      | [] -> r
      | _  -> (qss,i)::r)
    def []

(* Or-pattern expansion, variables are a complication w.r.t. the article *)

exception Cannot_flatten

let mk_alpha_env arg aliases ids =
  List.map
    (fun id -> id,
      if List.mem id aliases then
        match arg with
        | Some v -> v
        | _      -> raise Cannot_flatten
      else
        Ident.create_local (Ident.name id))
    ids

let rec explode_or_pat arg patl mk_action rem vars aliases pat : row cases =
  match pat with
  | {pat_desc = Tpat_or (p1,p2,_)} ->
      explode_or_pat
        arg patl mk_action
        (explode_or_pat arg patl mk_action rem vars aliases p2)
        vars aliases p1
  | {pat_desc = Tpat_alias (p,id, _)} ->
      explode_or_pat arg patl mk_action rem vars (id::aliases) p
  | {pat_desc = Tpat_var (x, _); pat_loc; _} ->
      let env = mk_alpha_env arg (x::aliases) vars in
      (omega pat_loc::patl, mk_action pat_loc (List.map snd env)) :: rem
  | p ->
      let env = mk_alpha_env arg aliases vars in
      (alpha_pat env p::patl, mk_action p.pat_loc (List.map snd env)) :: rem

let pm_free_variables { cases; _ } =
  List.fold_right
    (fun (_, act) r -> Ident.Set.union (free_variables act.expr) r)
    cases Ident.Set.empty


(* Basic grouping predicates *)
let pat_as_constr = function
  | {pat_desc=Tpat_construct (_, cstr,_)} -> cstr
  | _ -> fatal_error "Matching.pat_as_constr"

let pat_as_constr_with_field_pats = function
  | {pat_desc=Tpat_construct (_, cstr, field_pats)} -> cstr, field_pats
  | _ -> fatal_error "Matching.pat_as_constr_with_field_pats"

let group_const_int = function
  | {pat_desc= Tpat_constant Const_int _ } -> true
  | _                                      -> false

let group_const_char = function
  | {pat_desc= Tpat_constant Const_char _ } -> true
  | _                                      -> false

let group_const_string = function
  | {pat_desc= Tpat_constant Const_string _ } -> true
  | _                                      -> false

let group_const_float = function
  | {pat_desc= Tpat_constant Const_float _ } -> true
  | _                                      -> false

let group_const_int32 = function
  | {pat_desc= Tpat_constant Const_int32 _ } -> true
  | _                                      -> false

let group_const_int64 = function
  | {pat_desc= Tpat_constant Const_int64 _ } -> true
  | _                                      -> false

let group_const_nativeint = function
  | {pat_desc= Tpat_constant Const_nativeint _ } -> true
  | _                                      -> false

and group_constructor = function
  | {pat_desc = Tpat_construct (_,_,_)} -> true
  | _ -> false

and group_variant = function
  | {pat_desc = Tpat_variant (_, _, _)} -> true
  | _ -> false

and group_var = function
  | {pat_desc=Tpat_any} -> true
  | _ -> false

and group_tuple = function
  | {pat_desc = (Tpat_tuple _|Tpat_any)} -> true
  | _ -> false

and group_record = function
  | {pat_desc = (Tpat_record _|Tpat_any)} -> true
  | _ -> false

and group_array = function
  | {pat_desc=Tpat_array _} -> true
  | _ -> false

and group_lazy = function
  | {pat_desc = Tpat_lazy _} -> true
  | _ -> false

let get_group p = match p.pat_desc with
| Tpat_any -> group_var
| Tpat_constant Const_int _ -> group_const_int
| Tpat_constant Const_char _ -> group_const_char
| Tpat_constant Const_string _ -> group_const_string
| Tpat_constant Const_float _ -> group_const_float
| Tpat_constant Const_int32 _ -> group_const_int32
| Tpat_constant Const_int64 _ -> group_const_int64
| Tpat_constant Const_nativeint _ -> group_const_nativeint
| Tpat_construct _ -> group_constructor
| Tpat_tuple _ -> group_tuple
| Tpat_record _ -> group_record
| Tpat_array _ -> group_array
| Tpat_variant (_,_,_) -> group_variant
| Tpat_lazy _ -> group_lazy
|  _ -> fatal_error "Matching.get_group"

(* Check anticipated failure... *)
let rec is_guarded_expr (expr : Lambda.lambda) =
  match expr with
  | Lifthenelse(_cond, _ifso, { expr = Lstaticraise (0,[]); _ }, _loc) ->
      true
  | Llet(_str, _k, _id, _lam, body) -> is_guarded_expr body
  | Levent(lam, _ev) -> is_guarded_expr lam
  | _ -> false

let is_guarded (action : Lambda.block) =
  is_guarded_expr action.expr

(* ...and substitute its final value. *)
let rec patch_guarded_expr ~(patch : Lambda.block) (expr : Lambda.lambda) =
  match expr with
  | Lifthenelse (cond, ifso, { expr = Lstaticraise (0,[]); }, loc) ->
      Lifthenelse (cond, ifso, patch, loc)
  | Llet(str, k, id, lam, body) ->
      Llet (str, k, id, lam, patch_guarded_expr ~patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded_expr ~patch lam, ev)
  | _ -> fatal_error "Matching.patch_guarded"

let patch_guarded ~(patch : Lambda.block) (action : Lambda.block) =
  Lambda.block action.block_loc (patch_guarded_expr ~patch action.expr)

let is_or p = match p.pat_desc with
| Tpat_or _ -> true
| _ -> false



(* Conditions for appending to the Or matrix *)
let conda p q = not (may_compat p q)
and condb act ps qs =  not (is_guarded act) && Parmatch.le_pats qs ps

let or_ok p ps (l : row cases) =
  List.for_all
    (function
      | ({pat_desc=Tpat_or _} as q::qs, act) ->
          conda p q || condb act ps qs
      | _ -> true)
    l

(* Insert or append a pattern in the Or matrix *)

let equiv_pat p q = le_pat p q && le_pat q p

let rec get_equiv p (l : row cases) = match l with
  | (q::_, _act) as cl::rem ->
      if equiv_pat p q then
        let others,rem = get_equiv p rem in
        cl::others,rem
      else
        [],l
  | _ -> [],l


let insert_or_append p ps act ors (no : row cases) =
  let rec attempt seen = function
    | (q::qs, act_q) as cl::rem ->
        if is_or q then begin
          if may_compat p q then
            if
              Typedtree.pat_bound_idents p = [] &&
              Typedtree.pat_bound_idents q = [] &&
              equiv_pat p q
            then (* attempt insert, for equivalent orpats with no variables *)
              let _, not_e = get_equiv q rem in
              if
                or_ok p ps not_e && (* check append condition for head of O *)
                List.for_all        (* check insert condition for tail of O *)
                  (fun cl -> match cl with
                  | (q::_, _act) -> not (may_compat p q)
                  | _            -> assert false)
                  seen
              then (* insert *)
                List.rev_append seen ((p::ps, act)::cl::rem), no
              else (* fail to insert or append *)
                ors,(p::ps, act)::no
            else if condb act_q ps qs then (* check condition (b) for append *)
              attempt (cl::seen) rem
            else
              ors,(p::ps, act)::no
          else (* p # q, go on with append/insert *)
            attempt (cl::seen) rem
        end else (* q is not an or-pat, go on with append/insert *)
          attempt (cl::seen) rem
    | _  -> (* [] in fact *)
        (p::ps, act)::ors,no in (* success in appending *)
  attempt [] ors

(* Reconstruct default information from half_compiled  pm list *)

let rec rebuild_matrix pmh = match pmh with
  | Pm pm -> as_matrix pm.cases
  | PmOr {or_matrix=m} -> m
  | PmVar x -> add_omega_column (rebuild_matrix x.inside)

let rec rebuild_default nexts def = match nexts with
| [] -> def
| (e, pmh)::rem ->
    (add_omega_column (rebuild_matrix pmh), e)::
    rebuild_default rem def

let rebuild_nexts arg nexts k =
  List.fold_right
    (fun (e, pm) k -> (e, PmVar {inside=pm ; var_arg=arg})::k)
    nexts k


(*
  Split a matching.
    Splitting is first directed by or-patterns, then by
    tests (e.g. constructors)/variable transitions.

    The approach is greedy, every split function attempts to
    raise rows as much as possible in the top matrix,
    then splitting applies again to the remaining rows.

    Some precompilation of or-patterns and
    variable pattern occurs. Mostly this means that bindings
    are performed now,  being replaced by let-bindings
    in actions (cf. simplify_cases).

    Additionally, if the match argument is a variable, matchings whose
    first column is made of variables only are split further
    (cf. precompile_var).

*)


let rec split_or (loc : Location.t) argo cls (args : args) def =

  let cls : row cases = simplify_cases args cls in

  let rec do_split before ors (no : row cases) = function
    | [] ->
        cons_next
          (List.rev before) (List.rev ors) (List.rev no)
    | ((p::ps, act) as cl)::rem ->
        if up_ok cl no then
          if is_or p then
            let ors, no = insert_or_append p ps act ors no in
            do_split before ors no rem
          else begin
            if up_ok cl ors then
              do_split (cl::before) ors no rem
            else if or_ok p ps ors then
              do_split before (cl::ors) no rem
            else
              do_split before ors (cl::no) rem
          end
        else
          do_split before ors (cl::no) rem
    | _ -> assert false

  and cons_next yes yesor = function
    | [] ->
        precompile_or loc argo yes yesor args def []
    | rem ->
        let {me=next ; matrix=matrix ; top_default=def},nexts =
          do_split [] [] [] rem in
        let idef = next_raise_count () in
        precompile_or loc
          argo yes yesor args
          (cons_default matrix idef def)
          ((idef,next)::nexts) in

  do_split [] [] [] cls

(* Ultra-naive splitting, close to semantics, used for extension,
   as potential rebind prevents any kind of optimisation *)

and split_naive loc cls args def k =

  let rec split_exc cstr0 yes = function
    | [] ->
        let yes = List.rev yes in
        { me = Pm {cases=yes; args=args; default=def;} ;
          matrix = as_matrix yes ;
          top_default=def},
        k
    | (p::_, _act as cl)::rem ->
        if group_constructor p then
          let cstr = pat_as_constr p in
          if cstr = cstr0 then split_exc cstr0 (cl::yes) rem
          else
            let yes = List.rev yes in
            let {me=next ; matrix=matrix ; top_default=def}, nexts =
              split_exc cstr [cl] rem in
            let idef = next_raise_count () in
            let def = cons_default matrix idef def in
            { me = Pm {cases=yes; args=args; default=def} ;
              matrix = as_matrix yes ;
              top_default = def; },
            (idef,next)::nexts
        else
          let yes = List.rev yes in
          let {me=next ; matrix=matrix ; top_default=def}, nexts =
              split_noexc [cl] rem in
            let idef = next_raise_count () in
            let def = cons_default matrix idef def in
            { me = Pm {cases=yes; args=args; default=def} ;
              matrix = as_matrix yes ;
              top_default = def; },
            (idef,next)::nexts
    | _ -> assert false

  and split_noexc yes = function
    | [] -> precompile_var loc args (List.rev yes) def k
    | (p::_, _act as cl)::rem ->
        if group_constructor p then
          let yes= List.rev yes in
          let {me=next; matrix=matrix; top_default=def;},nexts =
            split_exc (pat_as_constr p) [cl] rem in
          let idef = next_raise_count () in
          precompile_var loc
            args yes
            (cons_default matrix idef def)
            ((idef,next)::nexts)
        else split_noexc (cl::yes) rem
    | _ -> assert false in

  match cls with
  | [] -> assert false
  | (p::_, _ as cl)::rem ->
      if group_constructor p then
        split_exc (pat_as_constr p) [cl] rem
      else
        split_noexc [cl] rem
  | _ -> assert false

and split_constr loc (cls : row cases) args def k =
  let ex_pat = what_is_cases loc cls in
  let loc = ex_pat.pat_loc in
  match ex_pat.pat_desc with
  | Tpat_any -> precompile_var loc args cls def k
  | Tpat_construct (_,{cstr_tag=Cstr_extension _},_) ->
      split_naive loc cls args def k
  | _ ->
      let group = get_group ex_pat in

      let rec split_ex yes no = function
        | [] ->
            let yes = List.rev yes and no = List.rev no in
            begin match no with
            | [] ->
                {me = Pm {cases=yes ; args=args ; default=def} ;
                  matrix = as_matrix yes ;
                  top_default = def},
                k
            | cl::rem ->
                begin match yes with
                | [] ->
                    (* Could not success in raising up a constr matching up *)
                    split_noex [cl] [] rem
                | _ ->
                    let {me=next ; matrix=matrix ; top_default=def}, nexts =
                      split_noex [cl] [] rem in
                    let idef = next_raise_count () in
                    let def = cons_default matrix idef def in
                    {me = Pm {cases=yes ; args=args ; default=def} ;
                      matrix = as_matrix yes ;
                      top_default = def },
                    (idef, next)::nexts
                end
            end
        | (p::_, _act) as cl::rem ->
            if group p && up_ok cl no then
              split_ex (cl::yes) no rem
            else
              split_ex yes (cl::no) rem
        | _ -> assert false

      and split_noex yes no = function
        | [] ->
            let yes = List.rev yes and no = List.rev no in
            begin match no with
            | [] -> precompile_var loc args yes def k
            | cl::rem ->
                let {me=next ; matrix=matrix ; top_default=def}, nexts =
                  split_ex [cl] [] rem in
                let idef = next_raise_count () in
                precompile_var loc
                  args yes
                  (cons_default matrix idef def)
                  ((idef,next)::nexts)
            end
        | [ps, _act as cl]
            when List.for_all group_var ps && yes <> [] ->
       (* This enables an extra division in some frequent cases :
          last row is made of variables only *)
              split_noex yes (cl::no) []
        | (p::_, _act) as cl::rem ->
            if not (group p) && up_ok cl no then
              split_noex (cl::yes) no rem
            else
              split_noex yes (cl::no) rem
        | _ -> assert false in

      match cls with
      | ((p::_, _act) as cl)::rem ->
          if group p then split_ex [cl] [] rem
          else split_noex [cl] [] rem
      | _ ->  assert false

and precompile_var loc (args : args) cls def k =
  match args with
  | []  -> assert false
  | _::((Lvar v as av, _mut, _arg_loc) as arg)::rargs ->
      begin match cls with
      | [_] -> (* as split as it can *)
          dont_precompile_var args cls def k
      | _ ->
          (* Precompile *)
          let var_cls =
            List.map (fun (ps, act) ->
                match ps with
                | _::ps -> ps,act
                | _     -> assert false)
              cls
          and var_def = make_default (fun _ rem -> rem) def in
          let {me=first ; matrix=matrix}, nexts =
            split_or loc (Some v) var_cls (arg::rargs) var_def in
          (* Compute top information *)
          match nexts with
          | [] -> (* If you need *)
              dont_precompile_var args cls def k
          | _  ->
              let rfirst =
                {me = PmVar {inside=first ; var_arg = av} ;
                  matrix = add_omega_column matrix ;
                  top_default = rebuild_default nexts def ; }
              and rnexts = rebuild_nexts av nexts k in
              rfirst, rnexts
      end
  | _ ->
      dont_precompile_var args cls def k

and dont_precompile_var args cls def k =
  {me =  Pm {cases = cls ; args = args ; default = def } ;
    matrix=as_matrix cls ;
    top_default=def},k

and precompile_or loc argo cls ors args def k =
  match ors with
  | [] -> split_constr loc cls args def k
  | _  ->
    let rec do_cases (cases : row cases) : row cases * _ =
      match cases with
      | ({pat_desc=Tpat_or _} as orp::patl, action)::rem ->
          let others,rem = get_equiv orp rem in
          let orpm =
            {cases =
              (patl, action)::
              List.map
                (function
                  | (_::ps, action) -> ps, action
                  | _ -> assert false)
                others ;
              args = (match args with _::r -> r | _ -> assert false) ;
             default = default_compat orp def} in
          let pm_fv = pm_free_variables orpm in
          let vars =
            Typedtree.pat_bound_idents_full orp
            |> List.filter (fun (id, _, _) -> Ident.Set.mem id pm_fv)
            |> List.map (fun (id,_,ty) -> id,Typeopt.value_kind orp.pat_env ty)
          in
          let or_num = next_raise_count () in
          let new_patl = omega_list patl in
          let mk_new_action loc vs =
            Lambda.block loc
              (Lstaticraise (or_num, List.map (fun v -> Lvar v) vs))
          in
          let body,handlers = do_cases rem in
          explode_or_pat
            argo new_patl mk_new_action body (List.map fst vars) [] orp,
          let mat = [[orp]] in
          ((mat, or_num, vars , orpm):: handlers)
      | cl::rem ->
          let new_ord,new_to_catch = do_cases rem in
          cl::new_ord,new_to_catch
      | [] -> [],[]
    in
    let end_body, handlers = do_cases ors in
    let matrix = as_matrix (cls@ors)
    and body = {cases=cls@end_body ; args=args ; default=def} in
    {me = PmOr {body=body ; handlers=handlers ; or_matrix=matrix; loc} ;
      matrix=matrix ;
      top_default=def},
    k

let split_precompile loc argo pm =
  let {me=next}, nexts = split_or loc argo pm.cases pm.args pm.default  in
  if dbg && (nexts <> [] || (match next with PmOr _ -> true | _ -> false))
  then begin
    Format.eprintf "** SPLIT **\n" ;
    pretty_pm pm ;
    pretty_precompiled_res  next nexts
  end ;
  next, nexts


(* General divide functions *)

let add_line patl_action pm = pm.cases <- patl_action :: pm.cases; pm

type cell =
  {pm : pattern_matching ;
  ctx : ctx list ;
  pat : pattern}

let add make_matching_fun division eq_key key patl_action args =
  try
    let (_,cell) = List.find (fun (k,_) -> eq_key key k) division in
    cell.pm.cases <- patl_action :: cell.pm.cases;
    division
  with Not_found ->
    let cell = make_matching_fun args in
    cell.pm.cases <- [patl_action] ;
    (key, cell) :: division

let divide make eq_key get_key get_args ctx pm =
  let rec divide_rec (cases : row cases) =
    match cases with
    | (p::patl, action) :: rem ->
        let this_match = divide_rec rem in
        add (make p pm.default ctx)
          this_match eq_key (get_key p)
            (get_args p patl, action) pm.args
    | _ -> []
  in
  divide_rec pm.cases

let divide_line make_ctx make get_args pat ctx pm =
  let rec divide_rec = function
    | (p::patl, action) :: rem ->
        let this_match = divide_rec rem in
        add_line (get_args p patl, action) this_match
    | _ -> make pm.default pm.args
  in
  { pm = divide_rec pm.cases;
    ctx = make_ctx ctx;
    pat = pat;
  }


(* Then come various functions,
   There is one set of functions per matching style
   (constants, constructors etc.)

   - matcher functions are arguments to make_default (for default handlers)
   They may raise NoMatch or OrPat and perform the full
   matching (selection + arguments).


   - get_args and get_key are for the compiled matrices, note that
   selection and getting arguments are separated.

   - make_ _matching combines the previous functions for producing
   new  ``pattern_matching'' records.
*)



let rec matcher_const (cst_loc, cst) p rem = match p.pat_desc with
| Tpat_or (p1,p2,_) ->
    begin try
      matcher_const (cst_loc, cst) p1 rem with
    | NoMatch -> matcher_const (cst_loc, cst) p2 rem
    end
| Tpat_constant c1 when const_compare c1 cst = 0 -> rem
| Tpat_any    -> rem
| _ -> raise NoMatch

let get_key_constant caller = function
  | {pat_desc= Tpat_constant cst; pat_loc} -> pat_loc, cst
  | p ->
      Format.eprintf "BAD: %s" caller ;
      pretty_pat p ;
      assert false

let get_args_constant _ rem = rem

let make_constant_matching p def ctx = function
    [] -> fatal_error "Matching.make_constant_matching"
  | (_ :: argl) ->
      let def =
        make_default
          (matcher_const (get_key_constant "make" p)) def
      and ctx =
        filter_ctx p  ctx in
      {pm = {cases = []; args = argl ; default = def} ;
        ctx = ctx ;
        pat = normalize_pat p}




let divide_constant ctx m =
  divide
    make_constant_matching
    (fun (_c_loc, c) (_d_loc, d) -> const_compare c d = 0)
    (get_key_constant "divide")
    get_args_constant
    ctx m

(* Matching against a constructor *)


let make_field_args binding_kind arg ~first_pos field_pat_locs (argl : args) =
  let field_arg ofs loc =
    Lprim (Pfield (first_pos + ofs), [arg], loc), binding_kind, loc
  in
  (List.mapi field_arg field_pat_locs) @ argl

let get_key_constr = function
  | {pat_desc=Tpat_construct (_, cstr,_); pat_loc; _} -> pat_loc, cstr.cstr_tag
  | _ -> assert false

let equal_key_constr (_pat_loc1, (tag1 : Types.constructor_tag))
      (_pat_loc2, (tag2 : Types.constructor_tag)) =
  tag1 = tag2

let get_key_constr_no_location pat =
  let _loc, tag = get_key_constr pat in
  tag

let get_args_constr p rem = match p with
| {pat_desc=Tpat_construct (_, _, args)} -> args @ rem
| _ -> assert false

(* NB: matcher_constr applies to default matrices.

       In that context, matching by constructors of extensible
       types degrades to arity checking, due to potential rebinding.
       This comparison is performed by Types.may_equal_constr.
*)

let matcher_constr cstr = match cstr.cstr_arity with
| 0 ->
    let rec matcher_rec q rem = match q.pat_desc with
    | Tpat_or (p1,p2,_) ->
        begin
          try matcher_rec p1 rem
          with NoMatch -> matcher_rec p2 rem
        end
    | Tpat_construct (_, cstr',[])
      when Types.may_equal_constr cstr cstr' -> rem
    | Tpat_any -> rem
    | _ -> raise NoMatch in
    matcher_rec
| 1 ->
    let rec matcher_rec q rem = match q.pat_desc with
    | Tpat_or (p1,p2,_) ->
        let r1 = try Some (matcher_rec p1 rem) with NoMatch -> None
        and r2 = try Some (matcher_rec p2 rem) with NoMatch -> None in
        begin match r1,r2 with
        | None, None -> raise NoMatch
        | Some r1, None -> r1
        | None, Some r2 -> r2
        | Some (a1::_), Some (a2::_) ->
            {a1 with
             pat_loc = q.pat_loc;
             pat_desc = Tpat_or (a1, a2, None)}::
            rem
        | _, _ -> assert false
        end
    | Tpat_construct (_, cstr', [arg])
      when Types.may_equal_constr cstr cstr' -> arg::rem
    | Tpat_any -> omega q.pat_loc::rem
    | _ -> raise NoMatch in
    matcher_rec
| _ ->
    fun q rem -> match q.pat_desc with
    | Tpat_or (_,_,_) -> raise OrPat
    | Tpat_construct (_,cstr',args)
      when  Types.may_equal_constr cstr cstr' -> args @ rem
    | Tpat_any -> omegas q.pat_loc cstr.cstr_arity @ rem
    | _        -> raise NoMatch

let make_constr_matching p def ctx = function
    [] -> fatal_error "Matching.make_constr_matching"
  | ((arg, _mut, arg_loc) :: argl) ->
      let cstr, field_pats = pat_as_constr_with_field_pats p in
      let field_pat_locs = List.map (fun pat -> pat.pat_loc) field_pats in
      let newargs : args =
        if cstr.cstr_inlined <> None then
          (arg, Alias, arg_loc) :: argl
        else match cstr.cstr_tag with
          Cstr_constant _ | Cstr_block _ ->
            make_field_args Alias arg ~first_pos:0 field_pat_locs argl
        | Cstr_unboxed -> (arg, Alias, arg_loc) :: argl
        | Cstr_extension _ ->
            make_field_args Alias arg ~first_pos:1 field_pat_locs argl
      in
      {pm=
        {cases = []; args = newargs;
          default = make_default (matcher_constr cstr) def} ;
        ctx =  filter_ctx p ctx ;
        pat=normalize_pat p}


let divide_constructor ctx pm =
  divide
    make_constr_matching
    equal_key_constr get_key_constr get_args_constr
    ctx pm

(* Matching against a variant *)

let equal_key_variant (_pat_loc1, (tag1 : Types.constructor_tag))
      (_pat_loc2, (tag2 : Types.constructor_tag)) =
  tag1 = tag2

let rec matcher_variant_const lab p rem = match p.pat_desc with
| Tpat_or (p1, p2, _) ->
    begin
      try
        matcher_variant_const lab p1 rem
      with
      | NoMatch -> matcher_variant_const lab p2 rem
    end
| Tpat_variant (lab1,_,_) when lab1=lab -> rem
| Tpat_any -> rem
| _   -> raise NoMatch


let make_variant_matching_constant p lab def ctx = function
    [] -> fatal_error "Matching.make_variant_matching_constant"
  | (_ :: argl) ->
      let def = make_default (matcher_variant_const lab) def
      and ctx = filter_ctx p ctx in
      {pm={ cases = []; args = argl ; default=def} ;
        ctx=ctx ;
        pat = normalize_pat p}

let matcher_variant_nonconst lab p rem = match p.pat_desc with
| Tpat_or (_,_,_) -> raise OrPat
| Tpat_variant (lab1,Some arg,_) when lab1=lab -> arg::rem
| Tpat_any -> omega p.pat_loc::rem
| _   -> raise NoMatch


let make_variant_matching_nonconst p lab def ctx = function
    [] -> fatal_error "Matching.make_variant_matching_nonconst"
  | ((arg, _mut, _arg_loc) :: argl) ->
      let def = make_default (matcher_variant_nonconst lab) def
      and ctx = filter_ctx p ctx in
      {pm=
        {cases = [];
         args = (Lprim(Pfield 1, [arg], p.pat_loc), Alias, p.pat_loc) :: argl;
         default=def} ;
        ctx=ctx ;
        pat = normalize_pat p}

let divide_variant row ctx {cases = cl; args = al; default=def} =
  let row = Btype.row_repr row in
  let rec divide (cases : row cases) =
    match cases with
    | ({pat_desc = Tpat_variant(lab, pato, _); pat_loc} as p:: patl, action)
          :: rem ->
        let variants = divide rem in
        if try Btype.row_field_repr (List.assoc lab row.row_fields) = Rabsent
        with Not_found -> true
        then
          variants
        else begin
          let tag = Btype.hash_variant lab in
          match pato with
            None ->
              add (make_variant_matching_constant p lab def ctx) variants
                equal_key_variant (pat_loc, Cstr_constant tag)
                (patl, action) al
          | Some pat ->
              add (make_variant_matching_nonconst p lab def ctx) variants
                equal_key_variant (pat_loc, Cstr_block tag)
                (pat :: patl, action) al
        end
    | _ -> []
  in
  divide cl

(*
  Three ``no-test'' cases
  *)

(* Matching against a variable *)

let get_args_var _ rem = rem


let make_var_matching def = function
  | [] ->  fatal_error "Matching.make_var_matching"
  | _::argl ->
      {cases=[] ;
        args = argl ;
        default= make_default get_args_var def}

let divide_var loc ctx pm =
  divide_line ctx_lshift make_var_matching get_args_var (omega loc) ctx pm

(* Matching and forcing a lazy value *)

let get_arg_lazy p rem = match p with
| {pat_desc = Tpat_any} -> omega p.pat_loc :: rem
| {pat_desc = Tpat_lazy arg} -> arg :: rem
| _ ->  assert false

let matcher_lazy p rem = match p.pat_desc with
| Tpat_or (_,_,_)     -> raise OrPat
| Tpat_any
| Tpat_var _          -> omega p.pat_loc :: rem
| Tpat_lazy arg       -> arg :: rem
| _                   -> raise NoMatch

(* Inlining the tag tests before calling the primitive that works on
   lazy blocks. This is also used in translcore.ml.
   No other call than Obj.tag when the value has been forced before.
*)

let prim_obj_tag =
  Primitive.simple ~name:"caml_obj_tag" ~arity:1 ~alloc:false

let get_mod_field loc modname field =
  let mod_ident = Ident.create_persistent modname in
  let env = Env.add_persistent_structure mod_ident Env.initial_safe_string in
  match Env.open_pers_signature modname env with
  | exception Not_found -> fatal_error ("Module "^modname^" unavailable.")
  | env -> begin
      match Env.lookup_value (Longident.Lident field) env with
      | exception Not_found ->
          fatal_error ("Primitive "^modname^"."^field^" not found.")
      | (path, _) -> transl_value_path loc env path
    end

let code_force_lazy_block loc =
  get_mod_field loc "CamlinternalLazy" "force_lazy_block"
let code_force_lazy loc =
  get_mod_field loc "CamlinternalLazy" "force"
;;

(* inline_lazy_force inlines the beginning of the code of Lazy.force. When
   the value argument is tagged as:
   - forward, take field 0
   - lazy, call the primitive that forces (without testing again the tag)
   - anything else, return it

   Using Lswitch below relies on the fact that the GC does not shortcut
   Forward(val_out_of_heap).
*)

let inline_lazy_force_cond arg loc =
  let idarg = Ident.create_local "*lzarg*" in
  let varg = Lvar idarg in
  let tag = Ident.create_local "*tag*" in
  let force_fun = code_force_lazy_block loc in
  let make_block expr = Lambda.block loc expr in
  Llet(Strict, Pgenval, idarg, arg,
       Llet(Alias, Pgenval, tag, Lprim(Pccall prim_obj_tag, [varg], loc),
            Lifthenelse(
              (* if (tag == Obj.forward_tag) then varg.(0) else ... *)
              Lprim(Pintcomp Ceq,
                    [Lvar tag;
                     Lconst(Const_base(Const_int Obj.forward_tag), loc)],
                    loc),
              make_block (Lprim(Pfield 0, [varg], loc)),
              make_block (Lifthenelse(
                (* ... if (tag == Obj.lazy_tag) then Lazy.force varg else ... *)
                Lprim(Pintcomp Ceq,
                      [Lvar tag;
                       Lconst(Const_base(Const_int Obj.lazy_tag), loc)],
                      loc),
                make_block (Lapply{ap_should_be_tailcall=false;
                       ap_loc=loc;
                       ap_func=force_fun;
                       ap_args=[varg];
                       ap_inlined=Default_inline;
                       ap_specialised=Default_specialise;
                      }),
                (* ... arg *)
                make_block varg,
                loc)),
              loc)))

let inline_lazy_force_switch arg loc =
  let idarg = Ident.create_local "*lzarg*" in
  let varg = Lvar idarg in
  let force_fun = code_force_lazy_block loc in
  let make_block expr = Lambda.block loc expr in
  Llet(Strict, Pgenval, idarg, arg,
       Lifthenelse(
         Lprim(Pisint, [varg], loc),
         make_block varg,
         make_block ((Lswitch
            (varg,
             { sw_numconsts = 0; sw_consts = [];
               sw_numblocks = 256;  (* PR#6033 - tag ranges from 0 to 255 *)
               sw_blocks =
                 [ (Obj.forward_tag, make_block (Lprim(Pfield 0, [varg], loc)));
                   (Obj.lazy_tag,
                    make_block (
                      Lapply{ap_should_be_tailcall=false;
                             ap_loc=loc;
                             ap_func=force_fun;
                             ap_args=[varg];
                             ap_inlined=Default_inline;
                             ap_specialised=Default_specialise;
                            }))
                 ];
               sw_failaction = Some (make_block varg) },
             loc))),
         loc))

let inline_lazy_force arg loc =
  if !Clflags.afl_instrument then
    (* Disable inlining optimisation if AFL instrumentation active,
       so that the GC forwarding optimisation is not visible in the
       instrumentation output.
       (see https://github.com/stedolan/crowbar/issues/14) *)
    Lapply{ap_should_be_tailcall = false;
           ap_loc=loc;
           ap_func=code_force_lazy loc;
           ap_args=[arg];
           ap_inlined=Default_inline;
           ap_specialised=Default_specialise;
          }
  else
    if !Clflags.native_code then
      (* Lswitch generates compact and efficient native code *)
      inline_lazy_force_switch arg loc
    else
      (* generating bytecode: Lswitch would generate too many rather big
         tables (~ 250 elts); conditionals are better *)
      inline_lazy_force_cond arg loc

let make_lazy_matching loc def = function
    [] -> fatal_error "Matching.make_lazy_matching"
  | (arg, _mut, _arg_loc) :: argl ->
      { cases = [];
        args =
          (inline_lazy_force arg loc, Strict, loc) :: argl;
        default = make_default matcher_lazy def }

let divide_lazy p ctx pm =
  divide_line
    (filter_ctx p)
    (make_lazy_matching p.pat_loc)
    get_arg_lazy
    p ctx pm

(* Matching against a tuple pattern *)


let get_args_tuple arity p rem = match p with
| {pat_desc = Tpat_any} -> omegas p.pat_loc arity @ rem
| {pat_desc = Tpat_tuple args} ->
    args @ rem
| _ ->  assert false

let matcher_tuple arity p rem = match p.pat_desc with
| Tpat_or (_,_,_)     -> raise OrPat
| Tpat_any
| Tpat_var _ -> omegas p.pat_loc arity @ rem
| Tpat_tuple args when List.length args = arity -> args @ rem
| _ ->  raise NoMatch

let make_tuple_matching arity field_pat_locs default = function
  | [] -> fatal_error "Matching.make_tuple_matching"
  | (arg, _mut, _arg_loc) :: argl ->
      let make_arg pos loc = Lprim (Pfield pos, [arg], loc), Alias, loc in
      let args = (List.mapi make_arg field_pat_locs) @ argl in
      { cases = [];
        args;
        default = make_default (matcher_tuple arity) default;
      }

let divide_tuple arity p ctx pm =
  let field_pat_locs =
    match p.pat_desc with
    | Tpat_tuple args -> List.map (fun pat -> pat.pat_loc) args
    | _ -> Misc.fatal_error "Only [Tpat_any] or [Tpat_tuple] expected"
  in
  divide_line
    (filter_ctx p)
    (make_tuple_matching arity field_pat_locs)
    (get_args_tuple arity) p ctx pm

(* Matching against a record pattern *)


let record_matching_line num_fields lbl_pat_list =
  let loc =
    match lbl_pat_list with
    | [] -> Location.none
    | (_, _, pat)::_ -> pat.pat_loc
  in
  let patv = Array.make num_fields (omega loc) in
  List.iter (fun (_, lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
  Array.to_list patv

let get_args_record num_fields p rem = match p with
| {pat_desc=Tpat_any} ->
    record_matching_line num_fields [] @ rem
| {pat_desc=Tpat_record (lbl_pat_list,_)} ->
    record_matching_line num_fields lbl_pat_list @ rem
| _ -> assert false

let matcher_record num_fields p rem = match p.pat_desc with
| Tpat_or (_,_,_) -> raise OrPat
| Tpat_any
| Tpat_var _      ->
  record_matching_line num_fields [] @ rem
| Tpat_record ([], _) when num_fields = 0 -> rem
| Tpat_record ((_, lbl, _) :: _ as lbl_pat_list, _)
  when Array.length lbl.lbl_all = num_fields ->
    record_matching_line num_fields lbl_pat_list @ rem
| _ -> raise NoMatch

let make_record_matching loc all_labels def = function
    [] -> fatal_error "Matching.make_record_matching"
  | ((arg, _mut, _arg_loc) :: argl) ->
      let rec make_args pos =
        if pos >= Array.length all_labels then argl else begin
          let lbl = all_labels.(pos) in
          let access =
            match lbl.lbl_repres with
            | Record_regular | Record_inlined _ ->
              Lprim (Pfield lbl.lbl_pos, [arg], loc)
            | Record_unboxed _ -> arg
            | Record_float -> Lprim (Pfloatfield lbl.lbl_pos, [arg], loc)
            | Record_extension _ -> Lprim (Pfield (lbl.lbl_pos + 1), [arg], loc)
          in
          let str =
            match lbl.lbl_mut with
              Immutable -> Alias
            | Mutable -> StrictOpt in
          (access, str, loc) :: make_args(pos + 1)
        end in
      let nfields = Array.length all_labels in
      let def= make_default (matcher_record nfields) def in
      {cases = []; args = make_args 0 ; default = def}


let divide_record all_labels p ctx pm =
  let get_args = get_args_record (Array.length all_labels) in
  divide_line
    (filter_ctx p)
    (make_record_matching p.pat_loc all_labels)
    get_args
    p ctx pm

(* Matching against an array pattern *)

let get_key_array = function
  | {pat_desc=Tpat_array patl; pat_loc} -> pat_loc, List.length patl
  | _ -> assert false

let equal_key_array (_pat_loc1, (len1 : int)) (_pat_loc2, (len2 : int)) =
  len1 = len2

let get_args_array p rem = match p with
| {pat_desc=Tpat_array patl} -> patl@rem
| _ -> assert false

let matcher_array len p rem = match p.pat_desc with
| Tpat_or (_,_,_) -> raise OrPat
| Tpat_array args when List.length args=len -> args @ rem
| Tpat_any -> omegas p.pat_loc len @ rem
| _ -> raise NoMatch

let make_array_matching kind p def ctx = function
  | [] -> fatal_error "Matching.make_array_matching"
  | ((arg, _mut, _arg_loc) :: argl) ->
      let _pat_loc, len = get_key_array p in
      let rec make_args pos =
        if pos >= len
        then argl
        else (Lprim(Parrayrefu kind,
                    [arg; Lconst(Const_base(Const_int pos), p.pat_loc)],
                    p.pat_loc),
              StrictOpt,
              p.pat_loc) :: make_args (pos + 1) in
      let def = make_default (matcher_array len) def
      and ctx = filter_ctx p ctx in
      {pm={cases = []; args = make_args 0 ; default = def} ;
        ctx=ctx ;
        pat = normalize_pat p}

let divide_array kind ctx pm =
  divide
    (make_array_matching kind)
    equal_key_array get_key_array get_args_array ctx pm


(*
   Specific string test sequence
   Will be called by the bytecode compiler, from bytegen.ml.
   The strategy is first dichotomic search (we perform 3-way tests
   with compare_string), then sequence of equality tests
   when there are less then T=strings_test_threshold static strings to match.

  Increasing T entails (slightly) less code, decreasing T
  (slightly) favors runtime speed.
  T=8 looks a decent tradeoff.
*)

(* Utilities *)

let strings_test_threshold = 8

let prim_string_notequal =
  Pccall(Primitive.simple
           ~name:"caml_string_notequal"
           ~arity:2
           ~alloc:false)

let prim_string_compare =
  Pccall(Primitive.simple
           ~name:"caml_string_compare"
           ~arity:2
           ~alloc:false)

let bind_sw arg k = match arg with
| Lvar _ -> k arg
| _ ->
    let id = Ident.create_local "*switch*" in
    Llet (Strict,Pgenval,id,arg,k (Lvar id))


(* Sequential equality tests *)

let make_string_test_sequence arg sw ~(default : Lambda.block option) =
  let d, sw = match default with
  | None ->
      begin match sw with
      | (_,d)::sw -> d.expr, sw
      | [] -> assert false
      end
  | Some d -> d.expr, sw in
  bind_sw arg
    (fun arg ->
      List.fold_right
        (fun (s, (act : Lambda.block)) k ->
          let loc = act.block_loc in
          let make_block expr = Lambda.block loc expr in
          Lifthenelse
            (Lprim
               (prim_string_notequal,
                [arg; Lconst (Const_immstring s, loc)], loc),
             make_block k,
             make_block act.expr,
             loc))
        sw d)

let rec split k xs = match xs with
| [] -> assert false
| x0::xs ->
    if k <= 1 then [],x0,xs
    else
      let xs,y0,ys = split (k-2) xs in
      x0::xs,y0,ys

let zero_lam loc = Lconst (Const_base (Const_int 0), loc)

let three_way_test loc arg lt eq gt =
  let make_block expr = Lambda.block loc expr in
  Lifthenelse
    (Lprim (Pintcomp Clt, [arg; zero_lam loc], loc),
     make_block lt,
     make_block (
       Lifthenelse(
         Lprim (Pintcomp Clt, [zero_lam loc; arg], loc),
         make_block gt,
         eq,
         loc)),
     loc)

(* Dichotomic tree *)

let rec do_make_string_test_tree arg (sw : string cases) delta
      ~(default : Lambda.block option) =
  let len = List.length sw in
  if len <= strings_test_threshold+delta then
    make_string_test_sequence arg sw ~default
  else
    let lt,(s,act),gt = split len sw in
    let loc = act.block_loc in
    bind_sw
      (Lprim
         (prim_string_compare,
          [arg; Lconst (Const_immstring s, loc)], loc))
      (fun r ->
        three_way_test loc r
          (do_make_string_test_tree arg lt delta ~default)
          act
          (do_make_string_test_tree arg gt delta ~default))

(* Entry point *)
let expand_stringswitch arg sw ~(default : Lambda.block option) =
  match default with
  | None ->
      bind_sw arg (fun arg ->
        do_make_string_test_tree arg sw 0 ~default:None)
  | Some default ->
      bind_sw arg (fun arg ->
        make_catch default (fun default ->
          let default = Some default in
          do_make_string_test_tree arg sw 1 ~default))

(**********************)
(* Generic test trees *)
(**********************)

(* Sharing *)

(* Add handler, if shared *)
let handle_shared () =
  let hs = ref (fun x -> x) in
  let handle_shared act =
    match act with
    | Switch.Single act -> act
    | Switch.Shared act ->
        let i,h = make_catch_delayed act in
        let ohs = !hs in
        hs := (fun act -> h (ohs act));
        make_exit act.block_loc i
  in
  hs, handle_shared

let share_actions_tree sw ~(default : Lambda.block option) =
  let store = StoreExp.mk_store () in
  (* Default action is always shared *)
  let default =
    match default with
    | None -> None
    | Some default -> Some (store.Switch.act_store_shared () default)
  in
  (* Store all other actions *)
  let sw =
    List.map (fun (cst,act) -> cst,store.Switch.act_store () act) sw
  in
  (* Retrieve all actions, including potential default *)
  let acts = store.Switch.act_get_shared () in
  (* Array of actual actions *)
  let hs,handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in
  (* Reconstruct default and switch list *)
  let default =
    match default with
    | None -> None
    | Some default -> Some (acts.(default))
  in
  let sw = List.map (fun (cst, j) -> cst, acts.(j)) sw in
  !hs,sw,default

type const_with_loc = Location.t * Asttypes.constant

module Tests = struct
  type t = const_with_loc cases

  (* Note: dichotomic search requires sorted input with no duplicates *)
  let rec uniq_lambda_list sw = match sw with
    | [] | [_] -> sw
    | (((_c1_loc, c1), _) as p1)::(((_c2_loc, c2),_)::sw2 as sw1) ->
        if const_compare c1 c2 = 0 then uniq_lambda_list (p1::sw2)
        else p1::uniq_lambda_list sw1

  let sort l =
    let l =
      List.stable_sort (fun ((_loc, x),_) ((_loc, y),_) ->
          const_compare x y)
        l
    in
    uniq_lambda_list l

  let rec cut n l =
    if n = 0 then [],l
    else match l with
    | [] -> raise (Invalid_argument "cut")
    | a::l -> let l1,l2 = cut (n-1) l in a::l1, l2
end

let rec do_tests_default ~(default : Lambda.block) tst arg (tests : Tests.t)
      : Lambda.block =
  match tests with
  | [] -> default
  | ((const_loc, const), act)::rem ->
      let expr =
        Lifthenelse
          (Lprim (tst, [arg; Lconst (Const_base const, const_loc)], const_loc),
           do_tests_default ~default tst arg rem,
           act,
           const_loc)
      in
      Lambda.block const_loc expr

let rec do_tests_no_default tst arg (tests : Tests.t) : Lambda.block =
  match tests with
  | [] -> fatal_error "Matching.do_tests_nofail"
  | [_, act] -> act
  | ((const_loc, const), act)::rem ->
      let expr =
        Lifthenelse
          (Lprim (tst, [arg; Lconst (Const_base const, const_loc)], const_loc),
           do_tests_no_default tst arg rem,
           act,
           const_loc)
      in
      Lambda.block const_loc expr

let make_test_sequence loc ~(default : Lambda.block option) tst lt_tst arg
      (tests : Tests.t) : Lambda.block =
  let tests = Tests.sort tests in
  let hs, tests, default = share_actions_tree tests ~default in
  let rec make_test_sequence tests =
    if List.length tests >= 4 && lt_tst <> Pignore then
      split_sequence tests
    else match default with
    | None -> do_tests_no_default tst arg tests
    | Some default -> do_tests_default ~default tst arg tests
  and split_sequence tests =
    let list1, list2 = Tests.cut (List.length tests / 2) tests in
    let expr =
      Lifthenelse(Lprim(lt_tst,
                        [arg;
                         Lconst(Const_base (snd (fst (List.hd list2))), loc)],
                        loc),
                  make_test_sequence list1,
                  make_test_sequence list2,
                  loc)
    in
    Lambda.block loc expr
  in
  hs (make_test_sequence tests)

module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq
  let neint = Pintcomp Cne
  let leint = Pintcomp Cle
  let ltint = Pintcomp Clt
  let geint = Pintcomp Cge
  let gtint = Pintcomp Cgt

  type act = Lambda.block

  let loc = Location.none

  let make_prim p args =
    let args = List.map (fun act -> act.expr) args in
    Lambda.block loc (Lprim (p, args, loc))

  let make_offset arg n =
    let expr =
      let arg = arg.expr in
      match n with
      | 0 -> arg
      | _ -> Lprim (Poffsetint n, [arg], loc)
    in
    Lambda.block loc expr

  let bind (arg : act) (body : act -> act) : act =
    let newvar, newarg =
      match arg.expr with
      | Lvar v -> v, arg
      | _ ->
          let newvar = Ident.create_local "*switcher*" in
          let expr = Lvar newvar in
          let block = Lambda.block arg.block_loc expr in
          newvar, block
    in
    let expr = bind Alias newvar arg.expr ((body newarg).expr) in
    Lambda.block arg.block_loc expr

  let make_const i =
    Lambda.block loc (Lconst (Const_base (Const_int i), loc))

  let make_isout_expr h arg =
    Lprim (Pisout, [h.expr; arg.expr], loc)

  let make_isout h arg =
    Lambda.block loc (make_isout_expr h arg)

  let make_isin h arg =
    Lambda.block loc (Lprim (Pnot, [make_isout_expr h arg], loc))

  let make_if cond ifso ifnot =
    Lambda.block loc (Lifthenelse (cond.expr, ifso, ifnot, loc))

  let make_switch loc arg cases acts =
    let l = ref [] in
    for i = Array.length cases-1 downto 0 do
      let act = acts.(cases.(i)) in
      l := (i, act) :: !l
    done;
    let expr =
      Lswitch(arg.expr,
              {sw_numconsts = Array.length cases; sw_consts = !l;
               sw_numblocks = 0; sw_blocks = [];
               sw_failaction = None},
              loc)
    in
    Lambda.block loc expr

  let make_catch act =
    let block = Lambda.block loc act.expr in
    make_catch_delayed block

  let make_exit act = make_exit loc act
end

(* Action sharing for Lswitch argument *)
let share_actions_sw _loc sw =
(* Attempt sharing on all actions *)
  let store = StoreExp.mk_store () in
  let default =
    match sw.sw_failaction with
    | None -> None
    | Some default ->
        (* The default case is compiled using exit, whatever happens. *)
        Some (store.Switch.act_store_shared () default)
  in
  let consts =
    List.map (fun (key, act) -> key, store.Switch.act_store () act)
      sw.sw_consts
  and blocks =
    List.map (fun (key, act) -> key, store.Switch.act_store () act)
      sw.sw_blocks
  in
  let acts = store.Switch.act_get_shared () in
  let hs, handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in
  let default =
    match default with
    | None -> None
    | Some act_index -> Some acts.(act_index)
  in
  let make_arms arms =
    List.map (fun (key, act_index) -> key, acts.(act_index)) arms
  in
  let sw =
    { sw with
      sw_consts = make_arms consts;
      sw_blocks = make_arms blocks;
      sw_failaction = default;
    }
  in
  !hs, sw

(* Reintroduce fail action in switch argument,
   for the sake of avoiding carrying over huge switches *)

let reintroduce_fail sw =
  match sw.sw_failaction with
  | None ->
      let t = Hashtbl.create 17 in
      let seen (_, act) =
        match as_simple_exit act with
        | Some i ->
            let old = try fst (Hashtbl.find t i) with Not_found -> 0 in
            Hashtbl.replace t i (old+1, act.block_loc)
        | None -> ()
      in
      List.iter seen sw.sw_consts ;
      List.iter seen sw.sw_blocks ;
      let i_max = ref (-1, Location.none)
      and max = ref (-1) in
      Hashtbl.iter
        (fun i (c, loc) ->
          if c > !max then begin
            i_max := (i, loc) ;
            max := c
          end) t ;
      if !max < 3 then sw
      else
        let default, default_loc = !i_max in
        let remove arms =
          List.filter (fun (_, act) ->
              match as_simple_exit act with
              | Some j -> j <> default
              | None -> true)
            arms
        in
        {sw with
         sw_consts = remove sw.sw_consts;
         sw_blocks = remove sw.sw_blocks;
         sw_failaction = Some (make_exit default_loc default);
        }
  | Some _ -> sw

module Switcher = Switch.Make(SArg)
open Switch

let rec last def = function
  | [] -> def
  | [x,_] -> x
  | _::rem -> last def rem

let get_edges low high l = match l with
| [] -> low, high
| (x,_)::_ -> x, last high l


let as_interval_canfail fail low high l =
  let store = StoreExp.mk_store () in

  let do_store _tag act =

    let i =  store.act_store () act in
(*
    eprintf "STORE [%s] %i %s\n" tag i (string_of_lam act) ;
*)
    i in

  let rec nofail_rec cur_low cur_high cur_act = function
    | [] ->
        if cur_high = high then
          [cur_low,cur_high,cur_act]
        else
          [(cur_low,cur_high,cur_act) ; (cur_high+1,high, 0)]
    | ((i,act_i)::rem) as all ->
        let act_index = do_store "NO" act_i in
        if cur_high+1= i then
          if act_index=cur_act then
            nofail_rec cur_low i cur_act rem
          else if act_index=0 then
            (cur_low,i-1, cur_act)::fail_rec i i rem
          else
            (cur_low, i-1, cur_act)::nofail_rec i i act_index rem
        else if act_index = 0 then
          (cur_low, cur_high, cur_act)::
          fail_rec (cur_high+1) (cur_high+1) all
        else
          (cur_low, cur_high, cur_act)::
          (cur_high+1,i-1,0)::
          nofail_rec i i act_index rem

  and fail_rec cur_low cur_high = function
    | [] -> [(cur_low, cur_high, 0)]
    | (i,act_i)::rem ->
        let index = do_store "YES" act_i in
        if index=0 then fail_rec cur_low i rem
        else
          (cur_low,i-1,0)::
          nofail_rec i i index rem in

  let init_rec = function
    | [] -> [low,high,0]
    | (i,act_i)::rem ->
        let index = do_store "INIT" act_i in
        if index=0 then
          fail_rec low i rem
        else
          if low < i then
            (low,i-1,0)::nofail_rec i i index rem
          else
            nofail_rec i i index rem in

  assert (do_store "FAIL" fail = 0) ; (* fail has action index 0 *)
  let r = init_rec l in
  Array.of_list r,  store

let as_interval_nofail l =
  let store = StoreExp.mk_store () in
  let rec some_hole = function
    | []|[_] -> false
    | (i,_)::((j,_)::_ as rem) ->
        j > i+1 || some_hole rem in
  let rec i_rec cur_low cur_high cur_act = function
    | [] ->
        [cur_low, cur_high, cur_act]
    | (i,act)::rem ->
        let act_index = store.act_store () act in
        if act_index = cur_act then
          i_rec cur_low i cur_act rem
        else
          (cur_low, cur_high, cur_act)::
          i_rec i i act_index rem in
  let inters = match l with
  | (i,act)::rem ->
      let act_index =
        (* In case there is some hole and that a switch is emitted,
           action 0 will be used as the action of unreachable
           cases (cf. switch.ml, make_switch).
           Hence, this action will be shared *)
        if some_hole rem then
          store.act_store_shared () act
        else
          store.act_store () act in
      assert (act_index = 0) ;
      i_rec i i act_index rem
  | _ -> assert false in

  Array.of_list inters, store


let sort_int_lambda_list l =
  List.sort
    (fun (i1,_) (i2,_) ->
      if i1 < i2 then -1
      else if i2 < i1 then 1
      else 0)
    l

let as_interval ~default low high l =
  let l = sort_int_lambda_list l in
  get_edges low high l,
  (match default with
  | None -> as_interval_nofail l
  | Some act -> as_interval_canfail act low high l)

let call_switcher loc ~default arg low high int_lambda_list =
  let edges, (cases, actions) =
    as_interval ~default low high int_lambda_list in
  Switcher.zyva loc edges (Lambda.block loc arg) cases actions


let rec list_as_pat = function
  | [] -> fatal_error "Matching.list_as_pat"
  | [pat] -> pat
  | pat::rem ->
      {pat with pat_desc = Tpat_or (pat,list_as_pat rem,None)}


let complete_pats_constrs = function
  | p::_ as pats ->
      List.map
        (pat_of_constr p)
        (complete_constrs p (List.map get_key_constr_no_location pats))
  | _ -> assert false


(* The following two ``failaction'' functions compute the trap handler
   to jump to in case of failure of elementary tests.
*)

let mk_failaction_neg loc partial ctx def =
  match partial with
  | Partial ->
      begin match def with
      | (_, idef)::_ ->
        let default = Lambda.block loc (Lstaticraise (idef, [])) in
        Some default, Jumps.singleton idef ctx
      | [] ->
        (* Act as Total, this means
           If no appropriate default matrix exists,
           then this switch cannot fail *)
         None, Jumps.empty
      end
  | Total ->
      None, Jumps.empty

(* In line with the article and simpler than before *)

let mk_failaction_pos loc partial seen ctx defs =
  if dbg then begin
    Format.eprintf "**POS**\n" ;
    pretty_def defs ;
    ()
  end ;
  let rec scan_def env to_test defs = match to_test,defs with
  | ([],_)|(_,[]) ->
      List.fold_left
        (fun  (klist,jumps) (pats,i)->
          let action = Lambda.block loc (Lstaticraise (i,[])) in
          let klist =
            List.fold_right
              (fun pat r -> (get_key_constr pat, action)::r)
              pats klist
          and jumps =
            Jumps.add i (ctx_lub (list_as_pat pats) ctx) jumps in
          klist,jumps)
        ([],Jumps.empty) env
  | _,(pss,idef)::rem ->
      let now, later =
        List.partition
          (fun (_p,p_ctx) -> ctx_match p_ctx pss) to_test in
      match now with
      | [] -> scan_def env to_test rem
      | _  -> scan_def ((List.map fst now,idef)::env) later rem in

  let fail_pats = complete_pats_constrs seen in
  if List.length fail_pats < !Clflags.match_context_rows then begin
    let fail,jmps =
      scan_def
        []
        (List.map
           (fun pat -> pat, ctx_lub pat ctx)
           fail_pats)
        defs in
    if dbg then begin
      eprintf "POSITIVE JUMPS [%i]:\n" (List.length fail_pats);
      Jumps.print_stderr jmps
    end ;
    None,fail,jmps
  end else begin (* Too many non-matched constructors -> reduced information *)
    if dbg then eprintf "POS->NEG!!!\n%!" ;
    let fail,jumps =  mk_failaction_neg loc partial ctx defs in
    if dbg then
      eprintf "FAIL: %s\n"
        (match fail with
        | None -> "<none>"
        | Some block -> string_of_block block) ;
    fail,[],jumps
  end

type 'a combine_cases = ((Location.t * 'a) * Lambda.block) list

let combine_constant loc (arg : Lambda.lambda) cst partial ctx def
    ((cases : Asttypes.constant combine_cases), total, _pats)
    : Lambda.block * Jumps.t =
  let default, local_jumps = mk_failaction_neg loc partial ctx def in
  let lambda1 =
    match cst with
    | Const_int _ ->
        let cases_by_int =
          List.map (function
              | (_pat_loc, Const_int n), act -> n, act
              | _ -> assert false)
            cases
        in
        call_switcher loc ~default arg min_int max_int cases_by_int
    | Const_char _ ->
        let cases_by_int =
          List.map (function
              | (_pat_loc, Const_char c), act -> Char.code c, act
              | _ -> assert false)
            cases
        in
        call_switcher loc ~default arg 0 255 cases_by_int
    | Const_string _ ->
        (* Note: as the bytecode compiler may resort to dichotomic search,
           the clauses of [Lstringswitch] are sorted with duplicates removed.
           This partly applies to the native code compiler, which requires
           no duplicates. *)
        let cases = Tests.sort cases in
        let sw =
          List.map
            (fun ((_cloc, c), act) -> match c with
            | Const_string (s, _) -> s, act
            | _ -> assert false)
            cases
        in
        let hs, sw, default = share_actions_tree sw ~default in
        let block = Lambda.block loc (Lstringswitch (arg, sw, default, loc)) in
        hs block
    | Const_float _ ->
        make_test_sequence loc
          ~default
          (Pfloatcomp CFneq) (Pfloatcomp CFlt)
          arg cases
    | Const_int32 _ ->
        make_test_sequence loc
          ~default
          (Pbintcomp(Pint32, Cne)) (Pbintcomp(Pint32, Clt))
          arg cases
    | Const_int64 _ ->
        make_test_sequence loc
          ~default
          (Pbintcomp(Pint64, Cne)) (Pbintcomp(Pint64, Clt))
          arg cases
    | Const_nativeint _ ->
        make_test_sequence loc
          ~default
          (Pbintcomp(Pnativeint, Cne)) (Pbintcomp(Pnativeint, Clt))
          arg cases
  in
  lambda1, Jumps.union local_jumps total

let split_cases tag_lambda_list =
  let rec split_rec = function
      [] -> ([], [])
    | ((_pat_loc, cstr), (act : Lambda.block)) :: rem ->
        let (consts, nonconsts) = split_rec rem in
        match cstr with
          Cstr_constant n -> ((n, act) :: consts, nonconsts)
        | Cstr_block n    -> (consts, (n, act) :: nonconsts)
        | Cstr_unboxed    -> (consts, (0, act) :: nonconsts)
        | Cstr_extension _ -> assert false in
  let const, nonconst = split_rec tag_lambda_list in
  sort_int_lambda_list const,
  sort_int_lambda_list nonconst

let split_extension_cases (cases : Types.constructor_tag combine_cases) =
  let rec split_rec = function
      [] -> ([], [])
    | ((_pat_loc, cstr), (act : Lambda.block)) :: rem ->
        (* Individual locations of the patterns are currently ignored for
           extension constructor matching. *)
        let (consts, nonconsts) = split_rec rem in
        match cstr with
          Cstr_extension(path, true) -> ((path, act) :: consts, nonconsts)
        | Cstr_extension(path, false) -> (consts, (path, act) :: nonconsts)
        | _ -> assert false
  in
  split_rec cases

let combine_constructor loc arg ex_pat cstr partial ctx def
      ((cases : Types.constructor_tag combine_cases), total1, pats)
      : Lambda.block * Jumps.t =
  if cstr.cstr_consts < 0 then begin
    (* Special cases for extensions *)
    let fail, local_jumps =
      mk_failaction_neg loc partial ctx def in
    let block =
      let consts, nonconsts = split_extension_cases cases in
      let default, consts, nonconsts =
        match fail with
        | None ->
            begin match consts, nonconsts with
            | _, (_key, act)::rem -> act, consts, rem
            | (_key, act)::rem, _ -> act, rem, nonconsts
            | _ -> assert false
            end
        | Some fail -> fail, consts, nonconsts
      in
      let nonconst_lambda =
        match nonconsts with
        | [] -> default
        | _ ->
            let tag = Ident.create_local "tag" in
            let tests =
              List.fold_right
                (fun (path, act) rem ->
                   let loc = act.block_loc in
                   let ext = transl_extension_path loc ex_pat.pat_env path in
                   let expr =
                     Lifthenelse(Lprim(Pintcomp Ceq, [Lvar tag; ext], loc),
                                 act, rem, loc)
                   in
                   Lambda.block loc expr)
                nonconsts
                default
            in
            let expr =
              Llet(Alias, Pgenval,tag, Lprim(Pfield 0, [arg], loc), tests.expr)
            in
            Lambda.block loc expr
      in
      List.fold_right (fun (path, act) rem ->
          let ext = transl_extension_path loc ex_pat.pat_env path in
          let expr =
            Lifthenelse(Lprim(Pintcomp Ceq, [arg; ext], loc),
                        act, rem, loc)
          in
          Lambda.block loc expr)
        consts
        nonconst_lambda
    in
    block, Jumps.union local_jumps total1
  end else begin
    (* Regular concrete type *)
    let ncases = List.length cases
    and nconstrs = cstr.cstr_consts + cstr.cstr_nonconsts in
    let sig_complete = ncases = nconstrs in
    let fail_opt,fails,local_jumps =
      if sig_complete then None,[],Jumps.empty
      else
        mk_failaction_pos loc partial pats ctx def
    in
    let cases = fails @ cases in
    let (consts, nonconsts) = split_cases cases in
    let block : Lambda.block =
      match fail_opt, same_actions cases with
      | None, Some act -> act (* Identical actions, no failure *)
      | _ ->
          match
            (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts)
          with
          | (1, 1, [0, act1], [0, act2]) ->
              (* Typically, match on lists, will avoid isint primitive in that
                 case *)
              Lambda.block loc (Lifthenelse (arg, act2, act1, loc))
          | (n,0,_,[])  -> (* The type defines constant constructors only *)
              call_switcher loc ~default:fail_opt arg 0 (n-1) consts
          | (n, _, _, _) ->
              let act0  =
                (* = Some act when all non-const constructors match to act *)
                match fail_opt,nonconsts with
                | Some a,[] -> Some a
                | Some _,_ ->
                    if List.length nonconsts = cstr.cstr_nonconsts then
                      same_actions nonconsts
                    else None
                | None,_ -> same_actions nonconsts
              in
              match act0 with
              | Some act ->
                  let expr =
                    Lifthenelse
                      (Lprim (Pisint, [arg], loc),
                      call_switcher loc ~default:fail_opt arg 0 (n-1) consts,
                      act,
                      loc)
                  in
                  Lambda.block loc expr
              | None ->
                  (* Emit a switch, as bytecode implements this sophisticated
                     instruction *)
                  let sw =
                    {sw_numconsts = cstr.cstr_consts; sw_consts = consts;
                     sw_numblocks = cstr.cstr_nonconsts; sw_blocks = nonconsts;
                     sw_failaction = fail_opt} in
                  let hs, sw = share_actions_sw loc sw in
                  let sw = reintroduce_fail sw in
                  let switch = Lambda.block loc (Lswitch (arg, sw, loc)) in
                  hs switch
    in
    block, Jumps.union local_jumps total1
  end

let make_test_sequence_variant_constant loc ~default
      (arg : Lambda.lambda) cases_by_int : Lambda.block =
  let _, (cases, actions) = as_interval ~default min_int max_int cases_by_int in
  let arg = Lambda.block loc arg in
  Switcher.test_sequence arg cases actions

let call_switcher_variant_constant loc ~default (arg : Lambda.lambda)
      cases_by_int : Lambda.block =
  call_switcher loc ~default arg min_int max_int cases_by_int

let call_switcher_variant_constr loc ~default (arg : Lambda.lambda)
      cases_by_int : Lambda.block =
  let v = Ident.create_local "*variant*" in
  let switcher =
    call_switcher loc ~default (Lvar v) min_int max_int cases_by_int
  in
  let expr =
    Llet (Alias, Pgenval, v, Lprim (Pfield 0, [arg], loc), switcher.expr)
  in
  Lambda.block loc expr

let combine_variant loc row arg partial ctx def
                    (tag_lambda_list, total1, _pats) : Lambda.block * Jumps.t =
  let row = Btype.row_repr row in
  let num_constr = ref 0 in
  if row.row_closed then
    List.iter
      (fun (_, f) ->
        match Btype.row_field_repr f with
          Rabsent | Reither(true, _::_, _, _) -> ()
        | _ -> incr num_constr)
      row.row_fields
  else
    num_constr := max_int;
  let test_int_or_block loc arg if_int if_block =
    let expr =
      Lifthenelse (Lprim (Pisint, [arg], loc), if_int, if_block, loc)
    in
    Lambda.block loc expr
  in
  let sig_complete = List.length tag_lambda_list = !num_constr
  and one_action = same_actions tag_lambda_list in
  let default, local_jumps =
    if sig_complete || (match partial with Total -> true | _ -> false)
    then
      None, Jumps.empty
    else
      mk_failaction_neg loc partial ctx def
  in
  let consts, nonconsts = split_cases tag_lambda_list in
  let block : Lambda.block =
    match default, one_action with
    | None, Some act -> act
    | _,_ ->
        match (consts, nonconsts) with
        | ([_, act1], [_, act2]) when Option.is_none default ->
            test_int_or_block loc arg act1 act2
        | (_, []) -> (* One can compare integers and pointers *)
            make_test_sequence_variant_constant loc ~default arg consts
        | ([], _) ->
            let switcher =
              call_switcher_variant_constr loc ~default arg nonconsts
            in
            (* One must not dereference integers *)
            begin match default with
            | None -> switcher
            | Some default ->
                test_int_or_block default.block_loc arg default switcher
            end
        | (_, _) ->
            let lam_const =
              call_switcher_variant_constant loc ~default arg consts
            and lam_nonconst =
              call_switcher_variant_constr loc ~default arg nonconsts
            in
            test_int_or_block loc arg lam_const lam_nonconst
  in
  block, Jumps.union local_jumps total1

let combine_array loc arg kind partial ctx def
    (len_lambda_list, total1, _pats) : Lambda.block * Jumps.t =
  let len_lambda_list =
    List.map (fun ((_pat_loc, len), block) -> len, block)
      len_lambda_list
  in
  let default, local_jumps = mk_failaction_neg loc partial ctx def in
  let block : Lambda.block =
    let newvar = Ident.create_local "*len*" in
    let switch =
      call_switcher loc
        ~default (Lvar newvar)
        0 max_int len_lambda_list in
    let expr =
      bind Alias newvar (Lprim(Parraylength kind, [arg], loc)) switch.expr
    in
    Lambda.block loc expr
  in
  block, Jumps.union local_jumps total1

(* Insertion of debugging events *)

let rec event_branch_expr repr (lam : Lambda.lambda) =
  begin match lam, repr with
    (_, None) ->
      lam
  | (Levent(lam', ev), Some r) ->
      incr r;
      Levent(lam', {lev_loc = ev.lev_loc;
                    lev_kind = ev.lev_kind;
                    lev_repr = repr;
                    lev_env = ev.lev_env})
  | (Llet(str, k, id, lam, body), _) ->
      Llet(str, k, id, lam, event_branch_expr repr body)
  | Lstaticraise _,_ -> lam
  | (_, Some _) ->
      Printlambda.lambda Format.str_formatter lam ;
      fatal_error
        ("Matching.event_branch: "^Format.flush_str_formatter ())
  end

let event_branch repr (block : Lambda.block) =
  Lambda.block block.block_loc (event_branch_expr repr block.expr)

(*
   This exception is raised when the compiler cannot produce code
   because control cannot reach the compiled clause,

   Unused is raised initially in compile_test.

   compile_list (for compiling switch results) catch Unused

   comp_match_handlers (for compiling split matches)
   may reraise Unused


*)

exception Unused

let compile_list compile_fun division =
  let rec c_rec totals = function
  | [] -> [], Jumps.union_list totals, []
  | (key, cell) :: rem ->
      begin match cell.ctx with
      | [] -> c_rec totals rem
      | _  ->
          try
            let (block1, total1) = compile_fun cell.ctx cell.pm in
            let c_rem, total, new_pats =
              c_rec
                (Jumps.map ctx_combine total1::totals) rem in
            ((key, block1)::c_rem), total, (cell.pat::new_pats)
          with
          | Unused -> c_rec totals rem
      end in
  c_rec [] division

let compile_orhandlers compile_fun (block1 : Lambda.block) total1 ctx to_catch
      : Lambda.block * Jumps.t =
  let rec do_rec (r : Lambda.block) (total_r : Jumps.t) = function
    | [] -> r, total_r
    | (mat, i, vars, pm)::rem ->
        begin try
          let ctx = select_columns mat ctx in
          let handler_i, total_i = compile_fun ctx pm in
          let loc_i = handler_i.block_loc in
          match raw_action r.expr with
          | Lstaticraise (j, args) ->
              if i=j then
                let r =
                  List.fold_right2 (bind_with_value_kind Alias)
                    vars args handler_i
                in
                let total_r =
                  Jumps.map (ctx_rshift_num (ncols mat)) total_i
                in
                r, total_r
              else
                do_rec r total_r rem
          | _ ->
              let r =
                Lambda.block loc_i (Lstaticcatch (r.expr, (i, vars), handler_i))
              in
              let total_r =
                Jumps.union
                  (Jumps.remove i total_r)
                  (Jumps.map (ctx_rshift_num (ncols mat)) total_i)
              in
              do_rec r total_r rem
        with
        | Unused ->
            let handler =
              Lambda.block r.block_loc (lambda_unit r.block_loc)
            in
            let r =
              Lambda.block r.block_loc
                (Lstaticcatch (r.expr, (i, vars), handler))
            in
            do_rec r total_r rem
        end
  in
  do_rec block1 total1 to_catch

let compile_test ~fail_loc compile_fun partial divide combine ctx to_match
      : Lambda.block * Jumps.t =
  let division = divide ctx to_match in
  let c_div = compile_list compile_fun division in
  match c_div with
  | [],_,_ ->
     begin match mk_failaction_neg fail_loc partial ctx to_match.default with
     | None,_ -> raise Unused
     | Some l,total -> l,total
     end
  | _ ->
      combine ctx to_match.default c_div

(* Attempt to avoid some useless bindings by lowering them *)

(* Approximation of v present in lam *)
let rec approx_present v = function
  | Lconst _ -> false
  | Lstaticraise (_,args) ->
      List.exists (fun lam -> approx_present v lam) args
  | Lprim (_,args,_) ->
      List.exists (fun lam -> approx_present v lam) args
  | Llet (Alias, _k, _, l1, l2) ->
      approx_present v l1 || approx_present v l2
  | Lvar vv -> Ident.same v vv
  | _ -> true

let rec lower_bind v arg lam =
  match lam with
  | Lifthenelse (cond, ifso, ifnot, loc) ->
      let pcond = approx_present v cond
      and pso = approx_present v ifso.expr
      and pnot = approx_present v ifnot.expr in
      begin match pcond, pso, pnot with
      | false, false, false -> lam
      | false, true, false ->
          Lifthenelse (cond, lower_bind_block v arg ifso, ifnot, loc)
      | false, false, true ->
          Lifthenelse (cond, ifso, lower_bind_block v arg ifnot, loc)
      | _,_,_ -> bind Alias v arg lam
      end
  | Lswitch (ls, ({sw_consts = [i, act]; sw_blocks = []} as sw), loc)
        when not (approx_present v ls) ->
      let act = lower_bind_block v arg act in
      Lswitch (ls, {sw with sw_consts = [i, act]}, loc)
  | Lswitch (ls, ({sw_consts = []; sw_blocks = [i, act]} as sw), loc)
        when not (approx_present v ls) ->
      let act = lower_bind_block v arg act in
      Lswitch (ls, {sw with sw_blocks = [i, act]}, loc)
  | Llet (Alias, k, vv, lv, l) ->
      if approx_present v lv then
        bind Alias v arg lam
      else
        Llet (Alias, k, vv, lv, lower_bind v arg l)
  | _ ->
      bind Alias v arg lam
and lower_bind_block v arg (block : Lambda.block) : Lambda.block =
  let expr = lower_bind v arg block.expr in
  Lambda.block block.block_loc expr

let bind_check str v arg (block : Lambda.block) =
  let expr =
    match str, arg with
    | _, Lvar _ -> bind str v arg block.expr
    | Alias, _ -> lower_bind v arg block.expr
    | _, _ -> bind str v arg block.expr
  in
  Lambda.block block.block_loc expr

let comp_exit block_loc ctx m =
  match m.default with
  | (_, i)::_ ->
    let block = Lambda.block block_loc (Lstaticraise (i, [])) in
    block, Jumps.singleton i ctx
  | _ -> fatal_error "Matching.comp_exit"

let rec comp_match_handlers comp_fun partial ctx arg ~first_match
      ~next_matches : Lambda.block * Jumps.t =
  match next_matches with
  | [] -> comp_fun partial ctx arg first_match
  | rem ->
      let rec c_rec (body : Lambda.block) total_body rem : Lambda.block * _ =
        match rem with
        | [] -> body, total_body
        (* Hum, -1 means never taken
        | (-1,pm)::rem -> c_rec body total_body rem *)
        | (i,pm)::rem ->
            let ctx_i,total_rem = Jumps.extract i total_body in
            begin match ctx_i with
            | [] -> c_rec body total_body rem
            | _ ->
                try
                  let li, total_i =
                    comp_fun
                      (match rem with [] -> partial | _ -> Partial)
                      ctx_i arg pm
                  in
                  let new_body =
                    Lambda.block body.block_loc
                      (Lstaticcatch (body.expr, (i, []), li))
                  in
                  c_rec new_body (Jumps.union total_i total_rem) rem
                with
                | Unused ->
                    let unit =
                      Lambda.block body.block_loc (lambda_unit body.block_loc)
                    in
                    let new_body =
                      Lambda.block body.block_loc
                        (Lstaticcatch (body.expr, (i, []), unit))
                    in
                    c_rec new_body total_rem rem
            end
      in
      try
         let first_lam, total =
           comp_fun Partial ctx arg first_match
         in
         c_rec first_lam total rem
      with Unused ->
        match next_matches with
        | [] -> raise Unused
        | (_, first_match)::next_matches ->
            comp_match_handlers comp_fun partial ctx arg
              ~first_match ~next_matches

(* To find reasonable names for variables *)

let rec name_pattern default = function
    (pat :: _, _act) :: rem ->
      begin match pat.pat_desc with
        Tpat_var (id, _) -> id
      | Tpat_alias(_, id, _) -> id
      | _ -> name_pattern default rem
      end
  | _ -> Ident.create_local default

let arg_to_var arg cls = match arg with
| Lvar v -> v,arg
| _ ->
    let v = name_pattern "*match*" cls in
    v,Lvar v


(*
  The main compilation function.
   Input:
      repr=used for inserting debug events
      partial=exhaustiveness information from Parmatch
      ctx=a context
      m=a pattern matching

   Output: a lambda block, a jump summary {..., exit number -> context, .. }
*)

let rec compile_match loc repr partial ctx (m : pattern_matching)
      : Lambda.block * Jumps.t =
  match m with
  | { cases = []; args = [] } -> comp_exit loc ctx m
  | { cases = ([], action) :: next_matches } ->
      if is_guarded action then begin
        let action_for_next_matches, total =
          compile_match loc None partial ctx { m with cases = next_matches }
        in
        let action =
          event_branch repr
            (patch_guarded ~patch:action_for_next_matches action)
        in
        action, total
      end else begin
        let action = event_branch repr action in
        action, Jumps.empty
      end
  | { args = (arg, str, arg_loc)::argl } ->
      let v, newarg = arg_to_var arg m.cases in
      let first_match, next_matches =
        split_precompile loc (Some v)
          { m with args = (newarg, Alias, arg_loc) :: argl }
      in
      let action, total =
        let comp_fun =
          if dbg then do_compile_matching_pr else do_compile_matching
        in
        comp_match_handlers (comp_fun loc repr)
          partial ctx newarg ~first_match ~next_matches
      in
      let action = bind_check str v arg action in
      action, total
  | _ -> assert false

(* verbose version of do_compile_matching, for debug *)

and do_compile_matching_pr loc repr partial ctx arg x =
  Format.eprintf "COMPILE: %s\nMATCH\n"
    (match partial with Partial -> "Partial" | Total -> "Total") ;
  pretty_precompiled x ;
  Format.eprintf "CTX\n" ;
  pretty_ctx ctx ;
  let (_, jumps) as r =  do_compile_matching loc repr partial ctx arg x in
  Format.eprintf "JUMPS\n" ;
  Jumps.print_stderr jumps ;
  r

and do_compile_matching loc repr partial ctx arg pmh :
      Lambda.block * Jumps.t =
match pmh with
| Pm pm ->
  let pat = what_is_cases loc pm.cases in
  begin match pat.pat_desc with
  | Tpat_any ->
      compile_no_test pat.pat_loc
        (divide_var pat.pat_loc) ctx_rshift repr partial ctx pm
  | Tpat_tuple patl ->
      compile_no_test pat.pat_loc
        (divide_tuple (List.length patl) (normalize_pat pat))
        ctx_combine repr partial ctx pm
  | Tpat_record ((_, lbl,_)::_,_) ->
      compile_no_test pat.pat_loc
        (divide_record lbl.lbl_all (normalize_pat pat))
        ctx_combine repr partial ctx pm
  | Tpat_constant cst ->
      compile_test ~fail_loc:pat.pat_loc
        (compile_match pat.pat_loc repr partial) partial
        divide_constant
        (combine_constant pat.pat_loc arg cst partial)
        ctx pm
  | Tpat_construct (_, cstr, _) ->
      compile_test ~fail_loc:pat.pat_loc
        (compile_match pat.pat_loc repr partial) partial
        divide_constructor
        (combine_constructor pat.pat_loc arg pat cstr partial)
        ctx pm
  | Tpat_array _ ->
      let kind = Typeopt.array_pattern_kind pat in
      compile_test ~fail_loc:pat.pat_loc
        (compile_match pat.pat_loc repr partial) partial
        (divide_array kind) (combine_array pat.pat_loc arg kind partial)
        ctx pm
  | Tpat_lazy _ ->
      compile_no_test pat.pat_loc
        (divide_lazy (normalize_pat pat))
        ctx_combine repr partial ctx pm
  | Tpat_variant(_, _, row) ->
      compile_test ~fail_loc:pat.pat_loc
        (compile_match pat.pat_loc repr partial) partial
        (divide_variant !row)
        (combine_variant pat.pat_loc !row arg partial)
        ctx pm
  | _ -> assert false
  end
| PmVar {inside=pmh ; var_arg=arg} ->
    let loc_and_lam, total =
      do_compile_matching loc repr partial (ctx_lshift ctx) arg pmh
    in
    loc_and_lam, Jumps.map ctx_rshift total
| PmOr {body=body ; handlers=handlers; loc} ->
    let loc_and_lam, total = compile_match loc repr partial ctx body in
    compile_orhandlers (compile_match loc repr partial)
      loc_and_lam total ctx handlers

and compile_no_test loc divide up_ctx repr partial ctx to_match
      : Lambda.block * Jumps.t =
  let {pm=this_match ; ctx=this_ctx } = divide ctx to_match in
  let block, total = compile_match loc repr partial this_ctx this_match in
  block, Jumps.map up_ctx total




(* The entry points *)

(*
   If there is a guard in a matching or a lazy pattern,
   then set exhaustiveness info to Partial.
   (because of side effects, assume the worst).

   Notice that exhaustiveness information is trusted by the compiler,
   that is, a match flagged as Total should not fail at runtime.
   More specifically, for instance if match y with x::_ -> x is flagged
   total (as it happens during JoCaml compilation) then y cannot be []
   at runtime. As a consequence, the static Total exhaustiveness information
   have to be downgraded to Partial, in the dubious cases where guards
   or lazy pattern execute arbitrary code that may perform side effects
   and change the subject values.
LM:
   Lazy pattern was PR#5992, initial patch by lpw25.
   I have  generalized the patch, so as to also find mutable fields.
*)

let find_in_pat pred =
  let rec find_rec p =
    pred p.pat_desc ||
    begin match p.pat_desc with
    | Tpat_alias (p,_,_) | Tpat_variant (_,Some p,_) | Tpat_lazy p ->
        find_rec p
    | Tpat_tuple ps|Tpat_construct (_,_,ps) | Tpat_array ps ->
        List.exists find_rec ps
    | Tpat_record (lpats,_) ->
        List.exists
          (fun (_, _, p) -> find_rec p)
          lpats
    | Tpat_or (p,q,_) ->
        find_rec p || find_rec q
    | Tpat_constant _ | Tpat_var _
    | Tpat_any | Tpat_variant (_,None,_) -> false
    | Tpat_exception _ -> assert false
  end in
  find_rec

let is_lazy_pat = function
  | Tpat_lazy _ -> true
  | Tpat_alias _ | Tpat_variant _ | Tpat_record _
  | Tpat_tuple _|Tpat_construct _ | Tpat_array _
  | Tpat_or _ | Tpat_constant _ | Tpat_var _ | Tpat_any
      -> false
  | Tpat_exception _ -> assert false

let is_lazy p = find_in_pat is_lazy_pat p

let have_mutable_field p =
  match p with
  | Tpat_record (lps,_) ->
      List.exists
        (fun (_,lbl,_) ->
          match lbl.Types.lbl_mut with
          | Mutable -> true
          | Immutable -> false)
        lps
  | Tpat_alias _ | Tpat_variant _ | Tpat_lazy _
  | Tpat_tuple _|Tpat_construct _ | Tpat_array _
  | Tpat_or _
  | Tpat_constant _ | Tpat_var _ | Tpat_any
    -> false
  | Tpat_exception _ -> assert false

let is_mutable p = find_in_pat have_mutable_field p

(* Downgrade Total when
   1. Matching accesses some mutable fields;
   2. And there are  guards or lazy patterns.
*)

let check_partial is_mutable is_lazy cases = function
  | Partial -> Partial
  | Total ->
      if
        cases = [] ||  (* allow empty case list *)
        List.exists
          (fun (pat_or_pats, act) ->
            is_mutable pat_or_pats && (is_guarded act || is_lazy pat_or_pats))
          cases
      then Partial
      else Total

let check_partial_list (cases : row cases) =
  check_partial (List.exists is_mutable) (List.exists is_lazy) cases

let check_partial (cases : pattern cases) =
  check_partial is_mutable is_lazy cases

(* have toplevel handler when appropriate *)

let start_ctx loc n = [{left=[] ; right = omegas loc n}]

let check_total total lambda i handler_fun =
  if Jumps.is_empty total then lambda
  else Lstaticcatch (lambda, (i,[]), handler_fun ())

let compile_matching loc repr (handler_fun : unit -> Lambda.block)
      arg (cases : pattern cases) partial : Lambda.lambda =
  let partial = check_partial cases partial in
  let cases = List.map (fun (pat, act) -> [pat], act) cases in
  match partial with
  | Partial ->
      let raise_num = next_raise_count () in
      let pm =
        { cases;
          args = [arg, Strict, loc];
          default = [[[omega loc]], raise_num];
        }
      in
      begin try
        let action, total =
          compile_match loc repr partial (start_ctx loc 1) pm
        in
        check_total total action.expr raise_num handler_fun
      with
      | Unused -> assert false (* ; handler_fun() *)
      end
  | Total ->
      let pm =
        { cases;
          args = [arg, Strict, loc];
          default = [];
        }
      in
      let action, total =
        compile_match loc repr partial (start_ctx loc 1) pm
      in
      assert (Jumps.is_empty total);
      action.expr

let partial_function loc () : Lambda.block =
  let slot =
    transl_extension_path loc
      Env.initial_safe_string Predef.path_match_failure
  in
  let fname, line, char = Location.get_pos_info loc.Location.loc_start in
  let expr =
    Lprim(Praise (Raise_regular None), [Lprim(Pmakeblock(0, Immutable, None),
            [slot; Lconst(Const_block(0,
                     [Const_base(Const_string (fname, None));
                      Const_base(Const_int line);
                      Const_base(Const_int char)]), loc)], loc)], loc)
  in
  Lambda.block loc expr

let for_function loc repr param (cases : pattern cases) partial =
  compile_matching loc repr (partial_function loc) param cases partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith param (cases : pattern cases) =
  let loc =
    (* The location is supposed to point just after the "with". *)
    match cases with
    | (_pat, block)::_ -> block.block_loc
    | [] -> Location.none
  in
  let handler =
    compile_matching loc None
      (fun () ->
        let expr =
          (* The [Location.none] ensures that the try-with itself does not
             appear as a "reraise" frame in any backtrace. *)
          Lprim(Praise (Raise_reraise (Some Location.none)), [param], loc)
        in
        Lambda.block loc expr)
      param cases Partial
  in
  Lambda.block loc handler

let simple_for_let loc param pat body =
  compile_matching loc None (partial_function loc) param [pat, body] Partial


(* Optimize binding of immediate tuples

   The goal of the implementation of 'for_let' below, which replaces
   'simple_for_let', is to avoid tuple allocation in cases such as
   this one:

     let (x,y) =
        let foo = ... in
        if foo then (1, 2) else (3,4)
     in bar

   The compiler easily optimizes the simple `let (x,y) = (1,2) in ...`
   case (call to Matching.for_multiple_match from Translcore), but
   didn't optimize situations where the rhs tuples are hidden under
   a more complex context.

   The idea comes from Alain Frisch who suggested and implemented
   the following compilation method, based on Lassign:

     let x = dummy in let y = dummy in
     begin
      let foo = ... in
      if foo then
        (let x1 = 1 in let y1 = 2 in x <- x1; y <- y1)
      else
        (let x2 = 3 in let y2 = 4 in x <- x2; y <- y2)
     end;
     bar

   The current implementation from Gabriel Scherer uses Lstaticcatch /
   Lstaticraise instead:

     catch
       let foo = ... in
       if foo then
         (let x1 = 1 in let y1 = 2 in exit x1 y1)
       else
        (let x2 = 3 in let y2 = 4 in exit x2 y2)
     with x y ->
       bar

   The catch/exit is used to avoid duplication of the let body ('bar'
   in the example), on 'if' branches for example; it is useless for
   linear contexts such as 'let', but we don't need to be careful to
   generate nice code because Simplif will remove such useless
   catch/exit.
*)

let rec map_return loc (f : Lambda.block -> Lambda.block) = function
  | Llet (str, k, id, l1, l2) -> Llet (str, k, id, l1, map_return loc f l2)
  | Lletrec (l1, l2) -> Lletrec (l1, map_return loc f l2)
  | Lifthenelse (lcond, lthen, lelse, loc) ->
      Lifthenelse (lcond, map_return_block f lthen, map_return_block f lelse,
        loc)
  | Lsequence (l1, l2) -> Lsequence (l1, map_return loc f l2)
  | Levent (l, ev) -> Levent (map_return loc f l, ev)
  | Ltrywith (l1, id, l2) ->
      Ltrywith (map_return loc f l1, id, map_return_block f l2)
  | Lstaticcatch (l1, b, l2) ->
      Lstaticcatch (map_return loc f l1, b, map_return_block f l2)
  | Lstaticraise _ | Lprim(Praise _, _, _) as l -> l
  | l -> (f (Lambda.block loc l)).expr

and map_return_block f (block : Lambda.block) : Lambda.block =
  let loc = block.block_loc in
  Lambda.block loc (map_return loc f block.expr)

(* The 'opt' reference indicates if the optimization is worthy.

   It is shared by the different calls to 'assign_pat' performed from
   'map_return'. For example with the code
     let (x, y) = if foo then z else (1,2)
   the else-branch will activate the optimization for both branches.

   That means that the optimization is activated if *there exists* an
   interesting tuple in one hole of the let-rhs context. We could
   choose to activate it only if *all* holes are interesting. We made
   that choice because being optimistic is extremely cheap (one static
   exit/catch overhead in the "wrong cases"), while being pessimistic
   can be costly (one unnecessary tuple allocation).
*)

(* CR mshinwell: I'm unsure whether the flow of locations from [for_let] through
   here (including [push_sublet], etc.) is right. *)
let assign_pat opt nraise catch_ids pat block =
  let rec collect acc pat block =
    match pat.pat_desc, block.expr with
    | Tpat_tuple patl, Lprim(Pmakeblock _, lams, makeblock_loc) ->
        opt := true;
        List.fold_left2 (fun acc pat action ->
            (* CR-someday mshinwell: Each component of the tuple should have
               its own location, not [makeblock_loc]. *)
            collect acc pat (Lambda.block makeblock_loc action))
          acc
          patl lams
    | Tpat_tuple patl, Lconst(Const_block(_, scl), loc) ->
        opt := true;
        List.fold_left2 (fun acc pat sc ->
            collect acc pat (Lambda.block loc (Lconst (sc, loc))))
          acc
          patl scl
    | _ ->
        (* pattern idents will be bound in staticcatch (let body), so we
           refresh them here to guarantee binders  uniqueness *)
        let pat_ids = pat_bound_idents pat in
        let fresh_ids = List.map (fun id -> id, Ident.rename id) pat_ids in
        (fresh_ids, alpha_pat fresh_ids pat, block) :: acc
  in
  (* sublets were accumulated by 'collect' with the leftmost tuple
     pattern at the bottom of the list; to respect right-to-left
     evaluation order for tuples, we must evaluate sublets
     top-to-bottom. To preserve tail-rec, we will fold_left the
     reversed list. *)
  let rev_sublets = List.rev (collect [] pat block) in
  let exit =
    (* build an Ident.tbl to avoid quadratic refreshing costs *)
    let add t (id, fresh_id) = Ident.add id fresh_id t in
    let add_ids acc (ids, _pat, _action) = List.fold_left add acc ids in
    let tbl = List.fold_left add_ids Ident.empty rev_sublets in
    let fresh_var id = Lvar (Ident.find_same id tbl) in
    let expr = Lstaticraise (nraise, List.map fresh_var catch_ids) in
    Lambda.block pat.pat_loc expr
  in
  let push_sublet (result : Lambda.block) (_ids, pat, action) =
    let expr =
      simple_for_let pat.pat_loc action.expr pat
        (Lambda.block action.block_loc result.expr)
    in
    Lambda.block pat.pat_loc expr
  in
  List.fold_left push_sublet exit rev_sublets

let for_let ~param pat ~body : Lambda.lambda =
  match pat.pat_desc with
  | Tpat_any ->
      (* This eliminates a useless variable (and stack slot in bytecode)
         for "let _ = ...". See #6865. *)
      Lsequence(param, body.expr)
  | Tpat_var (id, _) ->
      (* fast path, and keep track of simple bindings to unboxable numbers *)
      let k = Typeopt.value_kind pat.pat_env pat.pat_type in
      Llet(Strict, k, id, param, body.expr)
  | _ ->
      let opt = ref false in
      let nraise = next_raise_count () in
      let catch_ids = pat_bound_idents_full pat in
      let ids_with_kinds =
        List.map (fun (id, _, typ) -> id, Typeopt.value_kind pat.pat_env typ)
          catch_ids
      in
      let ids = List.map (fun (id, _, _) -> id) catch_ids in
      let loc = pat.pat_loc in
      let bind = map_return loc (assign_pat opt nraise ids pat) param in
      if !opt then Lstaticcatch(bind, (nraise, ids_with_kinds), body)
      else simple_for_let loc param pat body

(* Handling of tupled functions and matchings *)

(* Easy case since variables are available *)
let for_tupled_function loc paraml (cases : row cases) partial =
  let partial = check_partial_list cases partial in
  let raise_num = next_raise_count () in
  let omegas = [List.map (fun _ -> omega loc) paraml] in
  let pm =
    { cases;
      args = List.map (fun id -> (Lvar id, Strict, loc)) paraml ;
      default = [omegas, raise_num]
    } in
  try
    let action, total =
      compile_match loc None partial
        (start_ctx loc (List.length paraml)) pm
    in
    check_total total action.expr raise_num (partial_function loc)
  with
  | Unused -> (partial_function loc ()).expr

let flatten_pattern size p = match p.pat_desc with
| Tpat_tuple args -> args
| Tpat_any -> omegas p.pat_loc size
| _ -> raise Cannot_flatten

let rec flatten_pat_line size p k = match p.pat_desc with
| Tpat_any ->  omegas p.pat_loc size::k
| Tpat_tuple args -> args::k
| Tpat_or (p1,p2,_) ->  flatten_pat_line size p1 (flatten_pat_line size p2 k)
| Tpat_alias (p,_,_) -> (* Note: if this 'as' pat is here, then this is a
                           useless binding, solves PR#3780 *)
    flatten_pat_line size p k
| _ -> fatal_error "Matching.flatten_pat_line"

let flatten_cases size cases =
  List.map
    (fun (ps, action) -> match ps with
    | [p] -> flatten_pattern size p, action
    | _ -> fatal_error "Matching.flatten_case")
    cases

let flatten_matrix size pss =
  List.fold_right
    (fun ps r -> match ps with
    | [p] -> flatten_pat_line size p r
    | _   -> fatal_error "Matching.flatten_matrix")
    pss []

let flatten_def size def =
  List.map
    (fun (pss,i) -> flatten_matrix size pss,i)
    def

let flatten_pm size args pm =
    {args = args ; cases = flatten_cases size pm.cases ;
     default = flatten_def size pm.default}

let flatten_precompiled size args pmh =
  match pmh with
  | Pm pm -> Pm (flatten_pm size args pm)
  | PmOr {body=b ; handlers=hs ; or_matrix=m; loc} ->
      PmOr
        {body=flatten_pm size args b ;
         handlers=
           List.map
            (fun (mat,i,vars,pm) -> flatten_matrix size mat,i,vars,pm)
            hs ;
         or_matrix=flatten_matrix size m ;
         loc;
        }
  | PmVar _ -> assert false

(*
   compiled_flattened is a ``comp_fun'' argument to comp_match_handlers.
   Hence it needs a fourth argument, which it ignores
*)

let compile_flattened loc repr partial ctx _ pmh = match pmh with
| Pm pm -> compile_match loc repr partial ctx pm
| PmOr {body=b ; handlers=hs; loc} ->
    let lam, total = compile_match loc repr partial ctx b in
    compile_orhandlers (compile_match loc repr partial) lam total ctx hs
| PmVar _ -> assert false

let do_for_multiple_match loc paraml cases partial =
  let outer_loc = loc in
  let repr = None in
  let partial = check_partial cases partial in
  let cases = List.map (fun (pat, act) -> [pat], act) cases in
  let raise_num,pm1 =
    match partial with
    | Partial ->
        let raise_num = next_raise_count () in
        raise_num,
        { cases;
          args = [Lprim(Pmakeblock(0, Immutable, None), paraml, loc), Strict,
                  loc];
          default = [[[omega loc]],raise_num] }
    | _ ->
        -1,
        { cases;
          args = [Lprim(Pmakeblock(0, Immutable, None), paraml, loc), Strict,
                  loc];
          default = [] }
  in
  try
    try
      (* Once for checking that compilation is possible *)
      let next, nexts = split_precompile loc None pm1 in

      let size = List.length paraml
      and idl = List.map (fun _ -> Ident.create_local "*match*") paraml in
      let args =  List.map (fun id -> Lvar id, Alias, loc) idl in

      let flat_next = flatten_precompiled size args next
      and flat_nexts =
        List.map
          (fun (e, pm) -> e, flatten_precompiled size args pm)
          nexts
      in
      let action, total =
        comp_match_handlers
          (compile_flattened loc repr)
          partial (start_ctx loc size) ()
          ~first_match:flat_next ~next_matches:flat_nexts
      in
      List.fold_right2 (bind Strict) idl paraml
        (match partial with
        | Partial ->
            check_total total action.expr raise_num
              (partial_function outer_loc)
        | Total ->
            assert (Jumps.is_empty total);
            action.expr)
    with Cannot_flatten ->
      let action, total =
        compile_match loc None partial (start_ctx loc 1) pm1
      in
      begin match partial with
      | Partial ->
          check_total total action.expr raise_num
            (partial_function outer_loc)
      | Total ->
          assert (Jumps.is_empty total);
          action.expr
      end
  with Unused ->
    assert false (* ; partial_function loc () *)

(* PR#4828: Believe it or not, the 'paraml' argument below
   may not be side effect free. *)

let param_to_var param = match param with
| Lvar v -> v,None
| _ -> Ident.create_local "*match*",Some param

let bind_opt (v,eo) k = match eo with
| None -> k
| Some e ->  Lambda.bind Strict v e k

let for_multiple_match loc paraml cases partial =
  let v_paraml = List.map param_to_var paraml in
  let paraml = List.map (fun (v,_) -> Lvar v) v_paraml in
  let expr =
    List.fold_right bind_opt v_paraml
      (do_for_multiple_match loc paraml cases partial)
  in
  Lambda.block loc expr
