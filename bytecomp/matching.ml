(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compilation of pattern matching *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch

(*  See Peyton-Jones, ``The Implementation of functional programming
    languages'', chapter 5. *)

type pattern_matching =
  { mutable cases : (pattern list * lambda) list;
    args : (lambda * let_kind) list }

(* To group lines of patterns with identical keys *)

let add_line patl_action pm =
  pm.cases <- patl_action :: pm.cases; pm

let add make_matching_fun division key patl_action args =
  try
    let pm = List.assoc key division in
    pm.cases <- patl_action :: pm.cases;
    division
  with Not_found ->
    let pm = make_matching_fun args in
    pm.cases <- patl_action :: pm.cases;
    (key, pm) :: division

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    (pat :: patl, action) :: rem ->
      begin match pat.pat_desc with
        Tpat_var id -> id
      | Tpat_alias(p, id) -> id
      | _ -> name_pattern default rem
      end
  | _ -> Ident.create default


(* To remove aliases and bind named components *)

let any_pat =
  { pat_desc = Tpat_any; pat_loc = Location.none;
    pat_type = Ctype.none; pat_env = Env.empty }

exception Var of pattern


let simplify_or p =
  let rec simpl_rec = function
    | {pat_desc = Tpat_any|Tpat_var _} as p -> raise (Var p)
    | {pat_desc = Tpat_alias (q,id)} as p -> 
        begin try
          simpl_rec q
        with
        | Var q -> raise (Var {p with pat_desc = Tpat_alias (q,id)})
        end
    | {pat_desc = Tpat_or (p1,p2)} ->
        simpl_rec p1 ; simpl_rec p2
    | _ -> () in
  try
    simpl_rec p ; p
  with
  | Var p -> p

      
let simplify_matching m = match m.args with
| [] -> m
| (arg, _) :: _ ->
      let rec simplify = function
        (pat :: patl, action as patl_action) :: rem ->
          begin match pat.pat_desc with
          | Tpat_var id ->
              (any_pat :: patl, bind Alias id arg action) :: simplify rem
          | Tpat_alias(p, id) ->
              simplify ((p :: patl, bind Alias id arg action) :: rem)
          | Tpat_record [] ->
              (any_pat :: patl, action) :: simplify rem
          | Tpat_or (_,_) ->
              let pat_simple  = simplify_or pat in
              begin match pat_simple.pat_desc with
              | Tpat_or (_,_) ->
                  (pat_simple :: patl, action) ::
                  simplify rem
              | _ ->
                  simplify ((pat_simple::patl,action) :: rem)
              end
          | _ ->
              patl_action :: simplify rem
          end
      | cases -> cases in
    {m with cases = simplify m.cases }

let rec what_is_or = function
  | {pat_desc = Tpat_or (p,_)} -> what_is_or p
  | {pat_desc = (Tpat_alias (p,_))} -> what_is_or p        
  | {pat_desc=(Tpat_var _|Tpat_any)} -> fatal_error "Matching.what_is_or"
  | p -> p

let rec upper_left_pattern pm = match pm.cases with
| ({pat_desc=Tpat_or (pat,_)} :: _, _) :: _ -> what_is_or pat
| (pat :: _, _) :: _ -> pat
| _ -> assert false

(* Optimize breaks *)


let rec group_or group = function
  | {pat_desc = Tpat_or (p1, p2)} -> group_or group p1 && group_or group p2
  | {pat_desc = Tpat_alias (p,_)}  -> group_or group p
  | p -> group p


let rec extract_vars r p = match p.pat_desc with
| Tpat_var id -> IdentSet.add id r
| Tpat_alias (p, id) ->
    extract_vars (IdentSet.add id r) p
| Tpat_tuple pats ->
    List.fold_left extract_vars r pats
| Tpat_record lpats ->
    List.fold_left
      (fun r (_,p) -> extract_vars r p)
      r lpats
| Tpat_construct (_,pats) ->
    List.fold_left extract_vars r pats
| Tpat_array pats ->
    List.fold_left extract_vars r pats
| Tpat_variant (_,Some p, _) -> extract_vars r p
| Tpat_or (p,_) -> extract_vars r p
| Tpat_constant _|Tpat_any|Tpat_variant (_,None,_) -> r

exception Cannot_flatten

let mk_alpha_env arg aliases ids =
  List.map
    (fun id -> id,
      if List.mem id aliases then
        match arg with
        | Some v -> v
        | _      -> raise Cannot_flatten
      else 
        Ident.create (Ident.name id))
    ids


let rec explode_or_pat arg patl mk_action rem vars aliases = function
  | {pat_desc = Tpat_or (p1,p2)} ->
      explode_or_pat
        arg patl mk_action
        (explode_or_pat arg patl mk_action rem vars aliases p1)
        vars aliases p2
  | {pat_desc = Tpat_alias (p,id)} ->
      explode_or_pat arg patl mk_action rem vars (id::aliases) p
  | p ->
      let env = mk_alpha_env arg aliases vars in      
      (alpha_pat env p::patl,mk_action (List.map snd env))::rem


let more group ({cases=cl ; args = al} as m) = match al with
| (Lvar arg,_)::_ ->
    let rec more_rec yes no = function
      | (pat::_ as patl, action) as full :: rem ->
          if
            group pat &&
            not
              (List.exists
                 (fun (qs,_) -> compats qs patl)
                 no)
          then begin
            more_rec (full::yes) no rem
          end else
            more_rec  yes (full::no) rem
      | [] -> yes, List.rev no
      | _ -> assert false in
    let yes,no = more_rec [] [] cl in

    let rec add_or prev = function
      | ({pat_desc=Tpat_or (_,_)} as p::patl, action)::rem
         when group_or group p
         && not (List.exists (fun q -> Parmatch.compat q p) prev) ->
           let vars =
             IdentSet.elements
               (IdentSet.inter
                  (extract_vars IdentSet.empty p)
                  (free_variables action)) in
           begin match action with
           | Lstaticraise (_,[]) | Lstaticfail
               when
             vars = [] &&
             List.for_all
               (function {pat_desc=Tpat_any} -> true
                 | _ -> false)
               patl ->
                 let new_yes,new_to_catch,new_others =
                   add_or (p::prev) rem in                  
                 explode_or_pat
                   (Some arg) patl (fun _ -> action) new_yes vars [] p,
                 new_to_catch,
                 new_others
           | _ ->
               let raise_num = next_raise_count () in
               let new_patl = Parmatch.omega_list patl in

(*  Compilation assigne, pas bo
               let mk_new_action vs =
                 List.fold_right2
                   (fun dest src lambda ->
                     Lsequence (Lassign (dest, Lvar src),lambda))
                   vars vs
                   (Lstaticraise (raise_num,[])) in
*)
               let mk_new_action vs =
                 Lstaticraise
                   (raise_num, List.map (fun v -> Lvar v) vs) in

               let new_yes,new_to_catch,new_others =
                 add_or (p::prev) rem in
               explode_or_pat
                 (Some arg) new_patl mk_new_action new_yes vars [] p,
               ((raise_num, vars ,
                 {cases=[patl, action] ; args = List.tl al})::
                new_to_catch),
               new_others
           end
      | rem ->
          yes,
          [],
          {cases=rem ; args = al} in
    let yes,to_catch,others = add_or [] no in
    List.rev yes, to_catch, others
| _ -> assert false    
        
(* General divide functions *)
let divide group make get_key get_args ({args=al} as pm) =
  let rec divide_rec = function
    | (p::patl,action) :: rem
      when group p ->
        let this_match = divide_rec rem in
        add (make p) this_match (get_key p) (get_args p patl,action) al
    | cl -> [] in
  let yes, to_catch, others = more group pm in
  divide_rec yes, to_catch, others

let divide_line group make get_args ({args=al} as pm) =
  let rec divide_rec = function
    | (p::patl,action) :: rem
       when group p ->
         let this_match = divide_rec rem in
         add_line (get_args p patl, action) this_match
    | cl -> make al in
  let yes, to_catch, others = more group pm in
  divide_rec yes, to_catch, others

(* Matching against a constant *)

let group_constant = function
  | {pat_desc= Tpat_constant _} -> true
  | _                           -> false

let make_constant_matching _ = function
    [] -> fatal_error "Matching.make_constant_matching"
  | (arg :: argl) -> {cases = []; args = argl}

let get_key_constant = function
  | {pat_desc= Tpat_constant cst} -> cst
  | _ -> assert false

let get_args_constant _ rem = rem

let divide_constant m =
  divide
    group_constant make_constant_matching
    get_key_constant get_args_constant
    m

(* Matching against a constructor *)
let group_constructor = function
  | {pat_desc = Tpat_construct (_, _)} -> true
  | _ -> false

let make_field_args binding_kind arg first_pos last_pos argl =
  let rec make_args pos =
    if pos > last_pos
    then argl
    else (Lprim(Pfield pos, [arg]), binding_kind) :: make_args (pos + 1)
  in make_args first_pos

let get_key_constr = function
  | {pat_desc=Tpat_construct (cstr,_)} -> cstr.cstr_tag
  | _ -> assert false

let get_args_constr p rem = match p with
  | {pat_desc=Tpat_construct (_,args)} -> args @ rem
  | _ -> assert false

let pat_as_constr = function
  | {pat_desc=Tpat_construct (cstr,_)} -> cstr
  | _ -> assert false

let make_constr_matching p = function
    [] -> fatal_error "Matching.make_constr_matching"
  | ((arg, mut) :: argl) ->
      let cstr = pat_as_constr p in
      let newargs =
        match cstr.cstr_tag with
          Cstr_constant _ | Cstr_block _ ->
            make_field_args Alias arg 0 (cstr.cstr_arity - 1) argl
        | Cstr_exception _ ->
            make_field_args Alias arg 1 cstr.cstr_arity argl in
      {cases = []; args = newargs}


let divide_constructor pm =
  divide
    group_constructor make_constr_matching
    get_key_constr get_args_constr
    pm

(* Matching against a variant *)
let group_variant = function
  | {pat_desc = Tpat_variant (_, _, _)} -> true
  | _ -> false

let make_variant_matching_constant = function
    [] -> fatal_error "Matching.make_variant_matching_constant"
  | ((arg, mut) :: argl) ->
      { cases = []; args = argl }

let make_variant_matching_nonconst = function
    [] -> fatal_error "Matching.make_variant_matching_nonconst"
  | ((arg, mut) :: argl) ->
      {cases = []; args = (Lprim(Pfield 1, [arg]), Alias) :: argl}

let divide_variant row ({cases = cl; args = al} as pm) =
  let row = Btype.row_repr row in
  let rec divide = function
      ({pat_desc = Tpat_variant(lab, pato, _)} :: patl, action) :: rem ->
        let variants = divide rem in
        if try Btype.row_field_repr (List.assoc lab row.row_fields) = Rabsent
           with Not_found -> true
        then
          variants
        else begin
          let tag = Btype.hash_variant lab in
          match pato with
            None ->
              add make_variant_matching_constant variants
                   (Cstr_constant tag) (patl, action) al               
          | Some pat ->
              add make_variant_matching_nonconst variants
                   (Cstr_block tag) (pat :: patl, action) al
        end
    | cl -> []
  in
  let yes, to_catch, others = more group_variant pm in
  divide yes, to_catch, others

(* Matching against a variable *)
let group_var = function
  | {pat_desc=Tpat_any} -> true
  | _ -> false

let get_args_var _ rem = rem

let divide_var pm =
  divide_line
    group_var (make_constant_matching Tpat_any)
    get_args_var pm

(* Matching against a tuple pattern *)
let group_tuple = function
  | {pat_desc = (Tpat_tuple _|Tpat_any)} -> true
  | _ -> false

let make_tuple_matching num_comps = function
    [] -> fatal_error "Matching.make_tuple_matching"
  | (arg, mut) :: argl ->
      let rec make_args pos =
        if pos >= num_comps
        then argl
        else (Lprim(Pfield pos, [arg]), Alias) :: make_args (pos + 1) in
      {cases = []; args = make_args 0}


let get_args_tuple arity p rem = match p with
  | {pat_desc = Tpat_any} ->
      replicate_list any_pat arity @ rem
  | {pat_desc = Tpat_tuple args} ->
      args @ rem
  | _ -> assert false


let divide_tuple arity pm =
  divide_line
    group_tuple (make_tuple_matching arity)
    (get_args_tuple arity)
    pm

(* Matching against a record pattern *)

let group_record = function
  | {pat_desc = (Tpat_record _|Tpat_any)} -> true
  | _ -> false

let record_matching_line num_fields lbl_pat_list =
  let patv = Array.create num_fields any_pat in
  List.iter (fun (lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
  Array.to_list patv

let get_args_record num_fields p rem = match p with
| {pat_desc=Tpat_any} ->
    record_matching_line num_fields [] @ rem
| {pat_desc=Tpat_record lbl_pat_list} ->
    record_matching_line num_fields lbl_pat_list @ rem
| _ -> assert false

  
let make_record_matching all_labels = function
    [] -> fatal_error "Matching.make_record_matching"
  | ((arg, mut) :: argl) ->
      let rec make_args pos =
        if pos >= Array.length all_labels then argl else begin
          let lbl = all_labels.(pos) in
          let access =
            match lbl.lbl_repres with
              Record_regular -> Pfield lbl.lbl_pos
            | Record_float -> Pfloatfield lbl.lbl_pos in
          let str =
            match lbl.lbl_mut with
              Immutable -> Alias
            | Mutable -> StrictOpt in
          (Lprim(access, [arg]), str) :: make_args(pos + 1)
        end in
      {cases = []; args = make_args 0}

let divide_record all_labels pm =
  divide_line
    group_record
    (make_record_matching all_labels)
    (get_args_record (Array.length all_labels))
    pm

(* Matching against an array pattern *)
let group_array = function
  | {pat_desc=Tpat_array _} -> true
  | _ -> false

let get_key_array = function
  | {pat_desc=Tpat_array patl} -> List.length patl
  | _ -> assert false

let get_args_array p rem = match p with
  | {pat_desc=Tpat_array patl} -> patl@rem
  | _ -> assert false

let make_array_matching kind len = function
    [] -> fatal_error "Matching.make_array_matching"
  | ((arg, mut) :: argl) ->
      let rec make_args pos =
        if pos >= len
        then argl
        else (Lprim(Parrayrefu kind, [arg; Lconst(Const_base(Const_int pos))]),
              StrictOpt) :: make_args (pos + 1) in
      {cases = []; args = make_args 0}

let divide_array kind pm =
  divide
    group_array
    (fun p -> make_array_matching kind (get_key_array p))
    get_key_array get_args_array pm
  
(* To combine sub-matchings together *)

exception Not_simple

let rec raw_rec env = function
  | Llet(Alias,x,ex, body) -> raw_rec ((x,ex)::env) body
  | Lstaticfail as l -> l
  | Lvar id as l ->
      begin try List.assoc id env with
      | Not_found -> l
      end
  | Lprim (Pfield i,args) ->
      Lprim (Pfield i, List.map (raw_rec env) args)
  | Lconst _ as l -> l
  | Lstaticraise (i,args) ->
        Lstaticraise (i, List.map (raw_rec env) args)
  | _ -> raise Not_simple

let raw_action l = try raw_rec [] l with Not_simple -> l

let same_actions = function
  | [] -> None
  | [_,act] -> Some act
  | (_,act0) :: rem ->
      try
        let raw_act0 = raw_rec [] act0 in
        let rec s_rec = function
          | [] -> Some act0
          | (_,act)::rem ->
              if raw_act0 = raw_rec [] act then
                s_rec rem
              else
                None in
        s_rec rem
      with
      | Not_simple -> None

let equal_action act1 act2 =
  try
    let raw1 = raw_rec [] act1
    and raw2 = raw_rec [] act2 in
    raw1 = raw2
  with
  | Not_simple -> false


let sort_lambda_list l =
  List.sort
    (fun (x,_) (y,_) -> x - y)
    l


let add_catch (lambda1,total1)  (c_catch,(lambda_default,total_default)) =
  let rec do_rec r total_r = function
    | [] ->
        if total_r then
          (r,true)
        else begin match lambda_default with
        | Lstaticfail -> r,total_r
        | _ -> Lcatch (r,lambda_default),total_default
        end
    | (i,vars,(handler_i,total_i))::rem ->
(* Compilation assign, pas bo        
        do_rec
          (List.fold_right
             (fun v lambda ->
               bind StrictOpt v (Lconst const_unit) lambda)
             vars (Lstaticcatch (r,(i,[]), handler_i)))
          (total_i && total_r) rem in
*)
        match raw_action r with
        | Lstaticraise (j,args) ->
            if j <> i then
              do_rec r total_r rem
            else if args=[] then
              do_rec handler_i total_i rem
            else
              do_rec
                (Lstaticcatch (r,(i,vars), handler_i))
                (total_i && total_r) rem
        | _ ->
              do_rec
                (Lstaticcatch (r,(i,vars), handler_i))
                (total_i && total_r) rem in

  do_rec lambda1 total1 c_catch

let combine_line (lambda1, total1) c_catch =
  add_catch (lambda1, total1)  c_catch

let rec cut n l =
    if n = 0 then [],l
    else match l with
      [] -> raise (Invalid_argument "cut")
    | a::l -> let l1,l2 = cut (n-1) l in a::l1, l2

let make_test_sequence nofail check tst lt_tst arg const_lambda_list =
  let rec make_test_sequence const_lambda_list =
    if List.length const_lambda_list >= 4 & lt_tst <> Praise then
      split_sequence const_lambda_list
    else
      List.fold_right
        (fun (c, act) rem ->
         if rem = Lstaticfail && (not check || nofail) then act else
         Lifthenelse(Lprim(tst, [arg; Lconst(Const_base c)]), rem, act))
        const_lambda_list
        Lstaticfail
  and split_sequence const_lambda_list =
    let list1, list2 =
          cut (List.length const_lambda_list / 2) const_lambda_list in
    Lifthenelse(Lprim(lt_tst,[arg; Lconst(Const_base (fst(List.hd list2)))]),
                make_test_sequence list1, make_test_sequence list2)
  in make_test_sequence
      (Sort.list (fun (c1,_) (c2,_) -> c1 < c2) const_lambda_list)


let make_offset x arg = if x=0 then arg else Lprim(Poffsetint(x), [arg])

let make_switch_offset nofail check arg min_key max_key int_lambda_list =  
  let numcases = max_key - min_key + 1 in
  let cases =
    List.map (fun (key, l) -> (key - min_key, l)) int_lambda_list in
  let offsetarg = make_offset (-min_key) arg in
  Lswitch(offsetarg,
          {sw_numconsts = numcases; sw_consts = cases;
            sw_numblocks = 0; sw_blocks = []; sw_checked = check ;
            sw_nofail = nofail})


let make_switch_or_test_sequence
    nofail check arg const_lambda_list int_lambda_list =
  if const_lambda_list = [] then
    if check then Lstaticfail else lambda_unit
  else
    let min_key =
      List.fold_right (fun (k, l) m -> min k m) int_lambda_list max_int in
    let max_key =
      List.fold_right (fun (k, l) m -> max k m) int_lambda_list min_int in
  (* min_key and max_key can be arbitrarily large, so watch out for
     overflow in the following comparison *)
    if List.length int_lambda_list <= 1 + max_key / 4 - min_key / 4 then
    (* Sparse matching -- use a sequence of tests *)
      make_test_sequence nofail check (Pintcomp Cneq) (Pintcomp Clt)
        arg const_lambda_list
    else begin
    (* Dense matching -- use a jump table
       (2 bytecode instructions + 1 word per entry in the table) *)
      make_switch_offset nofail check arg min_key max_key int_lambda_list
    end

let make_test_sequence_variant_constant check arg int_lambda_list =
  make_test_sequence false check (Pintcomp Cneq) (Pintcomp Clt) arg
                (List.map (fun (n, l) -> (Const_int n, l)) int_lambda_list)

let make_test_sequence_variant_constr check arg int_lambda_list =
  let v = Ident.create "variant" in
  Llet(Alias, v, Lprim(Pfield 0, [arg]),
       make_test_sequence false check (Pintcomp Cneq) (Pintcomp Clt) (Lvar v)
                (List.map (fun (n, l) -> (Const_int n, l)) int_lambda_list))

let make_bitvect_check arg int_lambda_list lambda =
  let bv = String.make 32 '\000' in
  List.iter
    (fun (n, _) ->
      bv.[n lsr 3] <- Char.chr(Char.code bv.[n lsr 3] lor (1 lsl (n land 7))))
    int_lambda_list;
  Lifthenelse(Lprim(Pbittest, [Lconst(Const_base(Const_string bv)); arg]),
              lambda, Lstaticfail)

let prim_string_notequal =
  Pccall{prim_name = "string_notequal";
         prim_arity = 2; prim_alloc = false;
         prim_native_name = ""; prim_native_float = false}

let is_default act = match raw_action act with
| Lstaticfail -> true
| _ -> false

let rec explode_inter offset i j act k =
  if i <= j then
    explode_inter offset i (j-1) act ((j-offset,act)::k)
  else
    k

let as_int_list cases acts =
  let min_key,_,_ = cases.(0)
  and _,max_key,_ = cases.(Array.length cases-1) in
  let offset = max_key-min_key in
  let rec do_rec i k =
    if i >= 0 then
      let low, high, act =  cases.(i) in
      if is_default acts.(act) then
        do_rec (i-1) k
      else
        do_rec (i-1) (explode_inter min_key low high acts.(act) k)
    else
      k in
  min_key, max_key,do_rec (Array.length cases-1) []


let make_switch_switcher arg cases acts =
  let min_key, max_key, clauses = as_int_list cases acts in
  make_switch_offset false false arg 0 (max_key-min_key) clauses

module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq
  let neint = Pintcomp Cneq
  let leint = Pintcomp Cle
  let ltint = Pintcomp Clt
  let geint = Pintcomp Cge
  let gtint = Pintcomp Cgt

  type act = Lambda.lambda

  let default = Lstaticfail
(*  let equal_action = equal_action *)
  let make_prim p args = Lprim (p,args)
  let make_offset arg n = match n with
  | 0 -> arg
  | _ -> Lprim (Poffsetint n,[arg])
  let bind arg body =
    let newvar,newarg = match arg with
    | Lvar v -> v,arg
    | _      ->
        let newvar = Ident.create "switcher" in
        newvar,Lvar newvar in
    bind Alias newvar arg (body newarg)

  let make_isout h arg = Lprim (Pisout, [h ; arg])
  let make_if cond ifso ifnot = Lifthenelse (cond, ifso, ifnot)
  let make_switch = make_switch_switcher
end

module Switcher = Switch.Make(SArg)
open Switch

let lambda_of_int i =  Lconst (Const_base (Const_int i))

(* Store for actions in object style *)
exception Found of int
type t_store =
    {get : unit -> lambda array ; store : lambda -> int}

let mk_store () =
  let r_acts = ref [] in
  let store act =
    let rec store_rec i = function
      | [] -> i,[act] 
      | act0::rem ->
          if equal_action act act0 then raise (Found i)
          else
            let i,rem = store_rec (i+1) rem in
            i,act0::rem in
    try
      let i,acts = store_rec 0 !r_acts in
      r_acts := acts ;
      i
    with
    | Found i -> i

  and get () = Array.of_list !r_acts in
  {store=store ; get=get}

  
let as_interval_canfail low high l =
  let store = mk_store () in
  let rec nofail_rec cur_low cur_high cur_act = function
    | [] -> begin match high with
      | TooMuch -> [cur_low,cur_high,cur_act]
      | Int h ->
        if cur_high = h then
          [cur_low,cur_high,cur_act]
        else
          [(cur_low,cur_high,cur_act) ; (cur_high+1,h, 0)]
    end
    | ((i,act_i)::rem) as all ->
        let act_index = store.store act_i in
        if cur_high+1= i then
          if act_index=cur_act then
            nofail_rec cur_low i cur_act rem
          else if is_default act_i then
            (cur_low,i-1, cur_act)::fail_rec i i rem
          else
            (cur_low, i-1, cur_act)::nofail_rec i i act_index rem
        else
          (cur_low, cur_high, cur_act)::
          fail_rec ((cur_high+1)) (cur_high+1) all

  and fail_rec cur_low cur_high = function
    | [] -> [(cur_low, cur_high, 0)]
    | (i,act_i)::rem ->
        if is_default act_i then fail_rec cur_low i rem
        else
          (cur_low,i-1,0)::
          nofail_rec i i (store.store act_i) rem in

  let rec init_rec = function
    | [] -> []
    | (i,act_i)::rem as all ->
      if is_default act_i then
        match low with
        | TooMuch -> init_rec rem
        | Int low -> fail_rec low i rem
      else begin match low with
      | TooMuch -> nofail_rec i i (store.store act_i) rem
      | Int low ->
          if low < i then
            (low,i-1,0)::nofail_rec i i (store.store act_i) rem
          else
            nofail_rec i i (store.store act_i) rem
      end in

  ignore (store.store Lstaticfail) ; (* Lstaticfail has action index 0 *)
  let r = init_rec (sort_lambda_list l) in
  low, high, Array.of_list r,  store.get ()

let as_interval_nofail l =
  let store = mk_store ()
  and high = ref (-1)
  and low = ref (-1) in

  let rec i_rec cur_low cur_high cur_act = function
    | [] ->
        high := cur_high ;
        [cur_low, cur_high, cur_act]
    | (i,act)::rem ->
        let act_index = store.store act in
        if act_index = cur_act then
          i_rec cur_low i cur_act rem
        else
          (cur_low, cur_high, cur_act)::
          i_rec i i act_index rem in
  let inters = match sort_lambda_list l with
  | (i,act)::rem ->
      low := i ;
      let act_index = store.store act in
      i_rec i i act_index rem
  | _ -> assert false in
  Int !low, Int !high, Array.of_list inters, store.get ()

let as_interval nofail low high l =
  if nofail then
    as_interval_nofail l
  else
    as_interval_canfail low high l

let call_switcher konst nofail arg low high int_lambda_list =
  let real_low, real_high, cases, actions =
    as_interval nofail low high int_lambda_list in
  Switcher.zyva
    konst arg real_low real_high cases actions


let combine_constant arg cst partial (const_lambda_list, total1) c_catch =
  let nofail = partial=Total in
  let lambda1 =
    match cst with
    | Const_int _ ->
        let int_lambda_list =
          List.map (function Const_int n, l -> n,l | _ -> assert false)
            const_lambda_list in
        call_switcher
          lambda_of_int nofail arg
          Switch.TooMuch Switch.TooMuch
          int_lambda_list
    | Const_char _ ->
        let int_lambda_list =
          List.map (function Const_char c, l -> (Char.code c, l)
            | _ -> assert false)
            const_lambda_list in
        call_switcher
          (fun i -> Lconst (Const_base (Const_int i)))
          nofail arg
          (Switch.Int 0) (Switch.Int 255)
          int_lambda_list
(*
  begin match one_action with
  | Some lambda when List.length int_lambda_list > 8 ->
  make_bitvect_check arg int_lambda_list lambda
  | _ ->
  make_switch_or_test_sequence nofail true arg
  const_lambda_list int_lambda_list
  end
  *)
    | Const_string _ ->
        make_test_sequence
          nofail true prim_string_notequal Praise arg const_lambda_list
    | Const_float _ ->
        make_test_sequence
          nofail
          true (Pfloatcomp Cneq) (Pfloatcomp Clt)
          arg const_lambda_list in
  add_catch (lambda1, nofail) c_catch


let split_cases tag_lambda_list =
  let rec split_rec = function
      [] -> ([], [])
    | (cstr, act) :: rem ->
        let (consts, nonconsts) = split_rec rem in
        match cstr with
          Cstr_constant n -> ((n, act) :: consts, nonconsts)
        | Cstr_block n    -> (consts, (n, act) :: nonconsts)
        | _ -> assert false in
  let const, nonconst = split_rec tag_lambda_list in
  sort_lambda_list const,
  sort_lambda_list nonconst
  

let prerr_c l =
  List.iter (fun (i,_) -> Printf.fprintf stderr "%d " i) l
let prerr_i l =
  List.iter (fun i -> Printf.fprintf stderr "%d " i) l

let rec interval min max k =
  if min >= max then k
  else min::interval (min+1) max k

let find_missing  n l =
  let rec find_rec = function
    | [] -> []
    | [n1,_] -> interval (n1+1) n []
    | (n1,_)::((n2,_)::_ as rem) ->
        interval (n1+1) n2 (find_rec rem) in

  let r = match l with
  | [] -> interval 0 n []
  | (n1,_)::_  ->
      interval 0 n1 (find_rec l) in
(*
  Printf.fprintf stderr "Find missing %d " n;
  prerr_c l ;
  prerr_string " -> " ;
  prerr_i r ;
  prerr_endline "" ;
*)
  r
   

let test_fail arg cstr const nonconst =
  let miss_const =
    find_missing cstr.cstr_consts const
  and miss_nonconst =
    find_missing cstr.cstr_nonconsts nonconst
  in
  match const, miss_const, nonconst, miss_nonconst with
  | _,[n],_,[]   ->
      Some (Lprim (Pintcomp Ceq, [arg ; Lconst (Const_base (Const_int n))]))
  | [n,_],_,[],_   -> 
      Some (Lprim (Pintcomp Cneq, [arg ; Lconst (Const_base (Const_int n))]))
  | _,[],[],_::_    -> Some  (Lprim (Pnot, [Lprim (Pisint, [arg])]))
  | [],_::_,_,[]    -> Some (Lprim (Pisint, [arg]))
  | _, _, _, _      -> None

let combine_constructor arg cstr partial (tag_lambda_list, total1) c_catch =
  let nofail = partial=Total in
  if cstr.cstr_consts < 0 then begin
    (* Special cases for exceptions *)    
    let lambda1 =
      let default, tests =
        if nofail then
          match tag_lambda_list with
          | (_, act)::rem -> act,rem
          | _ -> assert false
        else
          Lstaticfail, tag_lambda_list in
      List.fold_right
        (fun (ex, act) rem ->
           match ex with
           | Cstr_exception path ->
               Lifthenelse(Lprim(Pintcomp Ceq,
                                 [Lprim(Pfield 0, [arg]); transl_path path]),
                           act, rem)
           | _ -> assert false)
       tests default
    in add_catch (lambda1, nofail) c_catch
  end else begin
    (* Regular concrete type *)
    let ncases = List.length tag_lambda_list
    and nconstrs =  cstr.cstr_consts + cstr.cstr_nonconsts in
    let sig_complete = ncases = nconstrs
    and one_action = same_actions tag_lambda_list in
    let total_loc = sig_complete || nofail in
    let (consts, nonconsts) = split_cases tag_lambda_list in
    let lambda1 =
      match total_loc, one_action, test_fail arg cstr consts nonconsts with
      | true, Some act, _ -> act
      | false, Some act, Some (Lprim (Pnot,[test])) ->
          Lifthenelse (test, act, Lstaticfail)
      | false, Some act, Some test ->
          Lifthenelse (test, Lstaticfail, act)
      | _,_, _ ->
        match (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts) with
        | (1, 1, [0, act1], [0, act2]) ->
            Lifthenelse(arg, act2, act1)
        | (1, 1, [0, act1], []) ->
            if total_loc then
              act1
            else
              Lifthenelse(arg, Lstaticfail, act1)
        | (1, 1, [], [0, act2]) ->
            if total_loc then
              act2
            else
              Lifthenelse(arg, act2, Lstaticfail)
        | n,m,l,[] ->
            if total_loc || m=0 then
              call_switcher
                (fun i -> Lconst (Const_base (Const_int i)))
                nofail arg
                (Switch.Int 0) (Switch.Int (n-1))
                l
            else
              Lifthenelse
                (Lprim (Pisint,[arg]),
                 call_switcher                   
                 (fun i -> Lconst (Const_base (Const_int i)))
                   nofail arg
                   (Switch.Int 0) (Switch.Int (n-1))
                   l,
                 Lstaticfail)
              
        | (_, _, _, _) ->
            Lswitch(arg, {sw_numconsts = cstr.cstr_consts;
                         sw_consts = consts;
                         sw_numblocks = cstr.cstr_nonconsts;
                         sw_blocks = nonconsts;
                         sw_checked = false ;
                         sw_nofail = nofail}) in
      add_catch (lambda1,total1 && total_loc) c_catch
  end

let combine_variant row arg partial (tag_lambda_list, total1)
                                    c_catch =
  let row = Btype.row_repr row in
  let num_constr = ref 0 in
  if row.row_closed then
    List.iter
      (fun (_, f) ->
        match Btype.row_field_repr f with
          Rabsent | Reither(true, _::_, _) -> ()
        | _ -> incr num_constr)
      row.row_fields
  else
    num_constr := max_int;
  let (consts, nonconsts) = split_cases tag_lambda_list in
  let test_int_or_block arg if_int if_block =
    Lifthenelse(Lprim (Pisint, [arg]), if_int, if_block) in
  let sig_complete =  List.length tag_lambda_list = !num_constr
  and nofail = partial=Total
  and one_action = same_actions tag_lambda_list in
  let total_loc = nofail || sig_complete in
  let lambda1 = match sig_complete || nofail, one_action with
  | true, Some act -> act
  | _,_ ->
      match (consts, nonconsts) with
      | ([n, act1], [m, act2]) when total_loc ->
          test_int_or_block arg act1 act2
      | ([n, act], []) ->
          make_test_sequence_variant_constant (not total_loc) arg consts
      | (_, []) ->
          let lam = make_test_sequence_variant_constant
                       (not total_loc) arg consts in
          if total_loc then lam else test_int_or_block arg lam Lstaticfail
      | ([], _) ->
          let lam = make_test_sequence_variant_constr
                       (not total_loc) arg nonconsts in
          if total_loc then lam else test_int_or_block arg Lstaticfail lam
      | (_, _) ->
          let lam_const = make_test_sequence_variant_constant
                               (not total_loc) arg consts in
          let lam_nonconst = make_test_sequence_variant_constr
                               (not total_loc) arg nonconsts in
          test_int_or_block arg lam_const lam_nonconst
  in
  add_catch (lambda1, total1 && total_loc) c_catch

let combine_array arg kind _ (len_lambda_list, total1) c_catch =
  let lambda1 =
    let newvar = Ident.create "len" in
    let switch =
      call_switcher
        lambda_of_int
        false (Lvar newvar)
        (Switch.Int 0) Switch.TooMuch
        len_lambda_list in
    bind
      Alias newvar (Lprim(Parraylength kind, [arg])) switch in
  add_catch (lambda1,false)  c_catch

(* Insertion of debugging events *)

let rec event_branch repr lam =
  begin match lam, repr with
    (_, None) ->
      lam
  | (Levent(lam', ev), Some r) ->
      incr r;
      Levent(lam', {lev_loc = ev.lev_loc;
                    lev_kind = ev.lev_kind;
                    lev_repr = repr;
                    lev_env = ev.lev_env})
  | (Llet(str, id, lam, body), _) ->
      Llet(str, id, lam, event_branch repr body)
  | Lstaticraise _,_ -> lam
  | (_, Some r) ->
      Printlambda.lambda Format.str_formatter lam ;
      fatal_error
        ("Matching.event_branch: "^Format.flush_str_formatter ())
  end

(*
  The main compilation function.
   Input:
      partial=exhaustiveness information from Parmatch
      pm=a pattern matching

   Output: a lambda term, a "total" flag
     (true if the lambda term does not raise ``exit'')
*)

let rec compile_list compile_fun = function
    [] -> ([], true)
  | (key, pm) :: rem ->
      let (lambda1, total1) = compile_fun pm in
      let (list2, total2) = compile_list compile_fun rem in
      ((key, lambda1) :: list2, total1 && total2)

let compile_catch compile_fun repr partial to_catch others =
  let partial_catch =
   if others.cases = [] then partial else Partial in
  let rec c_rec = function
  | [] -> [],compile_fun repr partial others
  | (i,vars,m)::rem ->
      let c_catch, c_others = c_rec rem in
      (i, vars, compile_fun repr partial_catch m)::c_catch,
      c_others in
  c_rec to_catch

let compile_test compile_match repr partial divide combine pm =
  let (this_match, to_catch, others) = divide pm in
  let partial' =
    if others.cases=[] then partial else Partial in
  combine partial'
    (compile_list (compile_match repr partial') this_match)
    (compile_catch compile_match repr partial to_catch others)

(* Attempt to avoid some useless bindinds by lowering them *)

(* Approximation of v present in lam *)
let rec approx_present v = function
  | Lconst _ -> false
  | Lstaticfail -> false
  | Lstaticraise (_,args) ->
      List.exists (fun lam -> approx_present v lam) args
  | Lprim (_,args) ->
      List.exists (fun lam -> approx_present v lam) args
  | Llet (Alias, _, l1, l2) ->
      approx_present v l1 || approx_present v l2
  | Lvar vv -> Ident.same v vv
  | _ -> true

let string_of_lam lam =
  Printlambda.lambda Format.str_formatter lam ;
  Format.flush_str_formatter ()

let rec lower_bind v arg lam = match lam with
| Lifthenelse (cond, ifso, ifnot) ->
    let pcond = approx_present v cond
    and pso = approx_present v ifso
    and pnot = approx_present v ifnot in
    begin match pcond, pso, pnot with
    | false, false, false -> lam
    | false, true, false ->        
        Lifthenelse (cond, lower_bind v arg ifso, ifnot)
    | false, false, true ->
        Lifthenelse (cond, ifso, lower_bind v arg ifnot)
    | _,_,_ -> bind Alias v arg lam
    end
| Lswitch (ls,({sw_consts=[i,act] ; sw_blocks = []} as sw))
    when not (approx_present v ls) ->
      Lswitch (ls, {sw with sw_consts = [i,lower_bind v arg act]})
| Lswitch (ls,({sw_consts=[] ; sw_blocks = [i,act]} as sw))
    when not (approx_present v ls) ->
      Lswitch (ls, {sw with sw_blocks = [i,lower_bind v arg act]})
| Llet (Alias, vv, lv, l) ->
    if approx_present v lv then
      bind Alias v arg lam
    else
      Llet (Alias, vv, lv, lower_bind v arg l)
| _ -> 
    bind Alias v arg lam

let bind_check str v arg lam = match str,arg with
| _, Lvar _ ->bind str v arg lam
| Alias,_ -> lower_bind v arg lam 
| _,_     -> bind str v arg lam

let rec compile_match repr partial m =  match m with
    { cases = [] } ->
      (Lstaticfail, false)
  | { cases = ([], action) :: rem; args = argl } ->
      if is_guarded action then begin
        let (lambda, total) =
          compile_match None partial { cases = rem; args = argl }
        and lambda_in = event_branch repr action in
        match lambda with
        | Lstaticfail -> lambda_in, false
        | _           -> Lcatch(lambda_in , lambda), total
      end else
        (event_branch repr action, true)
  | { args = (arg, str)::argl ; cases = (pat::_, _)::_ } ->
      let v,newarg =
        match arg with
        | Lvar v -> v,arg
        | _ ->
            let v = name_pattern "match" m.cases in
            v,Lvar v in
      let pm =
        simplify_matching
          { cases = m.cases; args = (newarg, Alias) :: argl } in
      let (lam, total) =
        do_compile_matching
          repr partial newarg
          (upper_left_pattern pm)
          pm in
      bind_check str v arg lam, total
  | _ -> assert false

and do_compile_matching repr partial newarg pat pm = match pat.pat_desc with
| Tpat_any ->
    compile_no_test divide_var repr partial pm
| Tpat_tuple patl ->
    compile_no_test
      (divide_tuple (List.length patl)) repr partial pm
| Tpat_record((lbl, _) :: _) ->
    compile_no_test
      (divide_record lbl.lbl_all) repr partial pm
| Tpat_constant cst ->
    compile_test
      compile_match repr partial
      divide_constant (combine_constant newarg cst)
      pm
| Tpat_construct (cstr, _) ->
    compile_test compile_match repr partial
      divide_constructor (combine_constructor newarg cstr)
      pm
| Tpat_array _ ->
    let kind = Typeopt.array_pattern_kind pat in
    compile_test compile_match repr partial
      (divide_array kind) (combine_array newarg kind)
      pm
| Tpat_variant(lab, _, row) ->
    compile_test compile_match repr partial
      (divide_variant row)
      (combine_variant row newarg)
      pm
| _ ->
    fatal_error "Matching.do_compile_matching"

and compile_no_test divide repr partial pm =
  let (this_match, to_catch, others) = divide pm in
  let partial' =
    if others.cases=[] then partial else Partial in
  combine_line
    (compile_match repr partial' this_match)
    (compile_catch compile_match repr partial to_catch others)

(* The entry points *)


(* had toplevel handler when appropriate *)

let check_total loc total lambda  handler_fun =
  if total then
    lambda
  else
    Lcatch(lambda, handler_fun())

let compile_matching loc repr handler_fun arg pat_act_list partial =
  let pm =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [arg, Strict] } in
  let (lambda, total) = compile_match repr partial pm in
  check_total loc total lambda handler_fun

let partial_function loc () =
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_match_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string !Location.input_name);
               Const_base(Const_int loc.Location.loc_start);
               Const_base(Const_int loc.Location.loc_end)]))])])

let for_function loc repr param pat_act_list partial =
  compile_matching loc repr (partial_function loc) param pat_act_list partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith param pat_act_list =
  compile_matching Location.none None (fun () -> Lprim(Praise, [param]))
    param pat_act_list Partial

let for_let loc param pat body =
  compile_matching loc None (partial_function loc) param [pat, body] Partial

(* Handling of tupled functions and matches *)


let flatten_pattern size p =
  match p.pat_desc with
    Tpat_tuple args -> args
  | Tpat_any -> replicate_list any_pat size  
  | _ -> raise Cannot_flatten

let flatten_cases size cases =
  let rec flat_rec = function
    | [] -> [],[]
    | ({pat_desc=Tpat_or (_,_)} as pat :: _, act) :: rem ->
        let vars =
          IdentSet.elements
            (IdentSet.inter
               (extract_vars IdentSet.empty pat)
               (free_variables act)) in
        let raise_num = next_raise_count () in
        let mk_new_action vs =
          Lstaticraise
            (raise_num, List.map (fun v -> Lvar v) vs) in
        let new_cases,to_catch =
          flat_rec
            (explode_or_pat None [] mk_new_action rem vars [] pat) in
        new_cases,
        (raise_num,vars,(act,true))::to_catch
    | (pat :: _, act)::rem ->
        let new_cases, to_catch = flat_rec rem in
        (flatten_pattern size pat, act)::new_cases,
        to_catch
    | _ -> assert false in
  flat_rec cases

let for_tupled_function loc paraml pats_act_list partial =
  let pm =
    { cases = pats_act_list;
      args = List.map (fun id -> (Lvar id, Strict)) paraml } in
  let (lambda, total) = compile_match None partial pm in
  check_total loc total lambda (partial_function loc)

let for_multiple_match loc paraml pat_act_list partial =
  let pm1 =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [Lprim(Pmakeblock(0, Immutable), paraml), Strict] } in
  let pm2 =
    simplify_matching pm1 in
  try
    let idl = List.map (fun _ -> Ident.create "match") paraml in
    let new_cases, to_catch = flatten_cases (List.length paraml) pm2.cases in
    let pm3 =
      { cases = new_cases ;
        args = List.map (fun id -> (Lvar id, Alias)) idl } in
    let (lambda, total) =
      add_catch
        (compile_match None partial pm3)
        (to_catch,(Lstaticfail,true)) in
    let lambda2 =
      check_total loc total lambda (partial_function loc) in
    List.fold_right2 (bind Strict) idl paraml lambda2
  with Cannot_flatten ->
    let (lambda, total) = compile_match None partial pm2 in
    check_total loc total lambda (partial_function loc)

