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

(* Detection of partial matches and unused match cases. *)

open Misc
open Asttypes
open Types
open Typedtree

(****************************************)
(* Utilities for retrieving constructor *)
(* and record label names               *)
(****************************************)

exception Empty (* Empty pattern *)

let get_type_descr ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv ty) in
  match ty.desc with
  | Tconstr (path,_,_,_) -> Env.find_type path tenv
  | _ -> fatal_error "Parmatch.get_type_descr"

let get_constr tag ty tenv =
  match get_type_descr ty tenv with
  | {type_kind=Type_variant constr_list} ->
      Datarepr.find_constr_by_tag tag constr_list
  | _ -> fatal_error "Parmatch.get_constr"

let find_label lbl lbls =
  try
    let name,_,_ = List.nth lbls lbl.lbl_pos in
    name
  with Failure "nth" -> "*Unkown label*"

let get_record_labels ty tenv =
  match get_type_descr ty tenv with
  | {type_kind = Type_record(lbls, rep)} -> lbls
  | _ -> fatal_error "Parmatch.get_record_labels"

(*************************************)
(* Utilities for building patterns   *)
(*************************************)

let make_pat desc ty tenv =
  {pat_desc = desc; pat_loc = Location.none;
   pat_type = ty ; pat_env = tenv }

let omega = make_pat Tpat_any Ctype.none Env.empty

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

(****************************)
(* Utilities for matching   *)
(****************************)

(* Check top matching *)
let simple_match p1 p2 = 
  match p1.pat_desc, p2.pat_desc with
  | Tpat_construct(c1, _), Tpat_construct(c2, _) ->
      c1.cstr_tag = c2.cstr_tag
  | Tpat_variant(l1, _, _), Tpat_variant(l2, _, _) ->
      l1 = l2
  | Tpat_constant(Const_float s1), Tpat_constant(Const_float s2) ->
      float_of_string s1 = float_of_string s2
  | Tpat_constant(c1), Tpat_constant(c2) -> c1 = c2
  | Tpat_tuple _, Tpat_tuple _ -> true
  | Tpat_record _ , Tpat_record _ -> true
  | Tpat_array p1s, Tpat_array p2s -> List.length p1s = List.length p2s
  | _, (Tpat_any | Tpat_var(_)) -> true
  | _, _ -> false




(* extract record fields as a whole *)
let record_arg p = match p.pat_desc with
| Tpat_any -> []
| Tpat_record args -> args
| _ -> fatal_error "Parmatch.as_record"


(* Raise Not_found when pos is not present in arg *)
let sort_fields args =
  Sort.list
    (fun (lbl1,_) (lbl2,_) -> lbl1.lbl_pos <= lbl2.lbl_pos)
    args

let get_field pos arg =
  let _,p = List.find (fun (lbl,_) -> pos = lbl.lbl_pos) arg in
  p


let extract_fields omegas arg =
  List.map
    (fun (lbl,_) ->
      try
        get_field lbl.lbl_pos arg
      with Not_found -> omega)
    omegas

let records_args l1 l2 =
  let l1 = sort_fields l1
  and l2 = sort_fields l2 in
  let rec combine r1 r2 l1 l2 = match l1,l2 with
  | [],[] -> r1,r2
  | [],(_,p2)::rem2 -> combine (omega::r1) (p2::r2) [] rem2
  | (_,p1)::rem1,[] -> combine (p1::r1) (omega::r2) rem1 []
  | (lbl1,p1)::rem1, (lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        combine (p1::r1) (omega::r2) rem1 l2
      else if lbl1.lbl_pos > lbl2.lbl_pos then
        combine (omega::r1) (p2::r2) l1 rem2
      else (* same label on both sides *)
        combine (p1::r1) (p2::r2) rem1 rem2 in
  combine [] [] l1 l2
;;

let sort_record p = match p.pat_desc with
| Tpat_record args ->
    make_pat
      (Tpat_record (sort_fields args))
      p.pat_type p.pat_env
| _ -> p

let all_record_args lbls = match lbls with
| ({lbl_all=lbl_all},_)::_ ->
    let t =
      Array.map
        (fun lbl -> lbl,omega) lbl_all in
    List.iter
      (fun ((lbl,_) as x) ->  t.(lbl.lbl_pos) <- x)
      lbls ;
    Array.to_list t
|  _ -> fatal_error "Parmatch.all_record_args"


(* Build argument list when p2 >= p1, where p1 is a simple pattern *)
let simple_match_args p1 p2 =
  match p2.pat_desc with
    Tpat_construct(cstr, args) -> args
  | Tpat_variant(lab, Some arg, _) -> [arg]
  | Tpat_tuple(args)  -> args
  | Tpat_record(args) ->  extract_fields (record_arg p1) args
  | Tpat_array(args) -> args
  | (Tpat_any | Tpat_var(_)) ->
      begin match p1.pat_desc with
        Tpat_construct(_, args) -> omega_list args
      | Tpat_variant(_, Some _, _) -> [omega]
      | Tpat_tuple(args) -> omega_list args
      | Tpat_record(args) ->  omega_list args
      | Tpat_array(args) ->  omega_list args
      | _ -> []
      end
  | _ -> []

(*
  Normalize a pattern ->
   all arguments are omega (simple pattern) and no more variables
*)

let rec normalize_pat q = match q.pat_desc with
  | Tpat_any | Tpat_constant _ -> q
  | Tpat_var _ -> make_pat Tpat_any q.pat_type q.pat_env
  | Tpat_alias (p,_) -> normalize_pat q
  | Tpat_tuple (args) ->
      make_pat (Tpat_tuple (omega_list args)) q.pat_type q.pat_env
  | Tpat_construct  (c,args) ->
      make_pat (Tpat_construct (c,omega_list args)) q.pat_type q.pat_env
  | Tpat_variant (l, arg, row) ->
      make_pat (Tpat_variant (l, may_map (fun _ -> omega) arg, row))
        q.pat_type q.pat_env
  | Tpat_array (args) ->
      make_pat (Tpat_array (omega_list args))  q.pat_type q.pat_env
  | Tpat_record (largs) ->
      make_pat (Tpat_record (List.map (fun (lbl,_) -> lbl,omega) largs))
        q.pat_type q.pat_env
  | Tpat_or (_,_) -> fatal_error "Parmatch.normalize_pat"


(*
  Build normalized (cf. supra) discriminating pattern,
  in the non-data type case
*)

let discr_pat q pss =

  let rec acc_pat acc pss = match pss with
    ({pat_desc = Tpat_alias (p,_)}::ps)::pss -> 
        acc_pat acc ((p::ps)::pss)
  | ({pat_desc = Tpat_or (p1,p2)}::ps)::pss ->
        acc_pat acc ((p1::ps)::(p2::ps)::pss)
  | ({pat_desc = (Tpat_any | Tpat_var _)}::_)::pss ->
        acc_pat acc pss
  | (({pat_desc = Tpat_tuple _} as p)::_)::_ -> normalize_pat p
  | (({pat_desc = Tpat_record largs} as p)::_)::pss ->
      let new_omegas =
        List.fold_left
          (fun r (lbl,_) ->
            try
              let _ = get_field lbl.lbl_pos r in
              r
            with Not_found ->
              (lbl,omega)::r)
          (record_arg acc)
          largs in
      acc_pat
        (make_pat (Tpat_record new_omegas) p.pat_type p.pat_env)
        pss
  | _ -> acc in

  match normalize_pat q with
  | {pat_desc= (Tpat_any | Tpat_record _)} as q ->
      sort_record (acc_pat q pss)
  | q -> q

(*
   In case a matching value is found, set actual arguments
   of the matching pattern.
*)

let rec read_args xs r = match xs,r with
| [],_ -> [],r
| _::xs, arg::rest ->
   let args,rest = read_args xs rest in
   arg::args,rest
| _,_ -> fatal_error "Parmatch.read_args"


let set_args q r = match q with
| {pat_desc = Tpat_tuple omegas} ->
    let args,rest = read_args omegas r in
    make_pat (Tpat_tuple args) q.pat_type q.pat_env::rest
| {pat_desc = Tpat_record omegas} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_record
         (List.map2 (fun (lbl,_) arg -> lbl,arg) omegas args))
      q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_construct (c,omegas)} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_construct (c,args)) q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_variant (l, omega, row)} ->
    let arg, rest =
      match omega, r with
        Some _, a::r -> Some a, r
      | None, r -> None, r
      | _ -> assert false
    in
    make_pat
      (Tpat_variant (l, arg, row)) q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_array omegas} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_array args) q.pat_type q.pat_env::
    rest
| {pat_desc=Tpat_constant _|Tpat_any} ->
    q::r (* case any is used in matching.ml *)
| _ -> fatal_error "Parmatch.set_args"


(* filter pss acording to pattern q *)
let filter_one q pss =
  let rec filter_rec = function
      ({pat_desc = Tpat_alias(p,_)}::ps)::pss -> 
        filter_rec ((p::ps)::pss)
    | ({pat_desc = Tpat_or(p1,p2)}::ps)::pss ->
        filter_rec ((p1::ps)::(p2::ps)::pss)
    | (p::ps)::pss ->
        if simple_match q p
        then (simple_match_args q p @ ps) :: filter_rec pss
        else filter_rec pss
    | _ -> [] in
  filter_rec pss

(*
  Filter pss in the ``extra case''. This applies :
  - According to an extra constructor (datatype case, non-complete signature).
  - Acordinng to anything (all-variables case).
*)
let filter_extra pss =
  let rec filter_rec = function
      ({pat_desc = Tpat_alias(p,_)}::ps)::pss -> 
        filter_rec ((p::ps)::pss)
    | ({pat_desc = Tpat_or(p1,p2)}::ps)::pss ->
        filter_rec ((p1::ps)::(p2::ps)::pss)
    | ({pat_desc = (Tpat_any | Tpat_var(_))} :: qs) :: pss ->
        qs :: filter_rec pss
    | _::pss  -> filter_rec pss
    | [] -> [] in
  filter_rec pss

(* 
  Pattern p0 is the discriminating pattern,
  returns [(q0,[pss0]) ; ... ; (qn,[pssn])]
  where the qi's are simple patterns and the pssi's are
  matched matrices.

  In the case when matching is useless (all-variable case),
  returns []
  
*)
let filter_all pat0 pss =

  let rec insert q qs env =
    match env with
      [] ->
        let q0 = normalize_pat q in
        [q0, [simple_match_args q0 q @ qs]]
    | ((q0,pss) as c)::env ->
        if simple_match q0 q
        then (q0, ((simple_match_args q0 q @ qs) :: pss)) :: env
        else c :: insert q qs env in

  let rec filter_rec env = function
    ({pat_desc = Tpat_alias(p,_)}::ps)::pss ->
      filter_rec env ((p::ps)::pss)
  | ({pat_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_rec env ((p1::ps)::(p2::ps)::pss)
  | ({pat_desc = (Tpat_any | Tpat_var(_))}::_)::pss ->
      filter_rec env pss
  | (p::ps)::pss ->
      filter_rec (insert p ps env) pss
  | _ -> env

  and filter_omega env = function
    ({pat_desc = Tpat_alias(p,_)}::ps)::pss ->
      filter_omega env ((p::ps)::pss)
  | ({pat_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_omega env ((p1::ps)::(p2::ps)::pss)
  | ({pat_desc = (Tpat_any | Tpat_var(_))}::ps)::pss ->
      filter_omega
        (List.map (fun (q,qss) -> (q,(simple_match_args q omega @ ps) :: qss)) env)
        pss
  | _::pss -> filter_omega env pss
  | [] -> env in
        
  filter_omega
    (filter_rec
      (match pat0.pat_desc with
        (Tpat_record(_) | Tpat_tuple(_)) -> [pat0,[]]
      | _ -> [])
      pss)
    pss


(*
  Check whether the first column of env makes up a complete signature or
  not.
*)      

let full_match tdefs force env =  match env with
| ({pat_desc = Tpat_construct ({cstr_tag=Cstr_exception _},_)},_)::_ ->
    false
| ({pat_desc = Tpat_construct(c,_)},_) :: _ ->
    List.length env = c.cstr_consts + c.cstr_nonconsts
| ({pat_desc = Tpat_variant(_,_,row)},_) :: _ ->
    let fields =
      List.map
        (function ({pat_desc = Tpat_variant (tag, _, _)}, _) -> tag
          | _ -> assert false)
        env
    in
    let row = Btype.row_repr row in
    if force then begin
      (* force=true, we are called from check_partial, and must close *)
      let (ok, nm) =
        List.fold_left
          (fun (ok,nm) (tag,f) ->
            match Btype.row_field_repr f with
              Rabsent -> (ok, nm)
            | Reither(_, _, false, e) ->
                (* m=false means that this tag is not explicitly matched *)
                e := Some Rabsent;
                (ok, None)
            | Reither (_, _, true, _)
                (* m=true, do not discard matched tags, rather warn *)
            | Rpresent _ ->
                (ok && List.mem tag fields, nm))
          (true, row.row_name) row.row_fields in
      if not row.row_closed || nm != row.row_name then
        (* this unification cannot fail *)
        Ctype.unify tdefs row.row_more
          (Btype.newgenty
             (Tvariant {row with row_fields = []; row_more = Btype.newgenvar();
                        row_closed = true; row_name = nm}));
      ok
    end else
      row.row_closed &&
      List.for_all
        (fun (tag,f) ->
          Btype.row_field_repr f = Rabsent || List.mem tag fields)
        row.row_fields
| ({pat_desc = Tpat_constant(Const_char _)},_) :: _ ->
    List.length env = 256
| ({pat_desc = Tpat_constant(_)},_) :: _ -> false
| ({pat_desc = Tpat_tuple(_)},_) :: _ -> true
| ({pat_desc = Tpat_record(_)},_) :: _ -> true
| ({pat_desc = Tpat_array(_)},_) :: _ -> false
| _ -> fatal_error "Parmatch.full_match"

(* complement constructor tags *)
let complete_tags nconsts nconstrs tags =
  let seen_const = Array.create nconsts false
  and seen_constr = Array.create nconstrs false in
  List.iter
    (function
      | Cstr_constant i -> seen_const.(i) <- true
      | Cstr_block i -> seen_constr.(i) <- true
      | _  -> assert false)
    tags ;
  let r = ref [] in
  for i = 0 to nconsts-1 do
    if not seen_const.(i) then
      r := Cstr_constant i :: !r
  done ;
  for i = 0 to nconstrs-1 do
    if not seen_constr.(i) then
      r := Cstr_block i :: !r
  done ;  
  !r

(* build a pattern from a constructor list *)
let pat_of_constr ex_pat cstr =
 {ex_pat with pat_desc = Tpat_construct (cstr,omegas cstr.cstr_arity)}
    
let rec pat_of_constrs ex_pat = function
| [] -> raise Empty
| [cstr] -> pat_of_constr ex_pat cstr
| cstr::rem ->    
    {ex_pat with
    pat_desc=
      Tpat_or
        (pat_of_constr ex_pat cstr,
         pat_of_constrs ex_pat rem)}

(* Sends back a pattern that complements constructor tags all_tag *)
let complete_constrs p all_tags = match p.pat_desc with
| Tpat_construct (c,_) ->
    begin try
      let not_tags = complete_tags  c.cstr_consts c.cstr_nonconsts all_tags in
      List.map
        (fun tag ->
          let _,targs = get_constr tag p.pat_type p.pat_env in
          {c with
      cstr_tag = tag ;
      cstr_args = targs ;
      cstr_arity = List.length targs})
        not_tags
with
| Datarepr.Constr_not_found ->
    fatal_error "Parmatch.complete_constr: constr_not_found"
    end
| _ -> fatal_error "Parmatch.complete_constr"


(*
  Builds a pattern that is incompatible with all patterns in
  in the first column of env
*)

let build_other env =  match env with
| ({pat_desc = Tpat_construct ({cstr_tag=Cstr_exception _} as c,_)},_) as p
  ::_ ->
    make_pat
      (Tpat_construct
         ({c with
           cstr_tag=(Cstr_exception
            (Path.Pident (Ident.create "*exception*")))},
          []))
      Ctype.none Env.empty
| ({pat_desc = Tpat_construct (_,_)} as p,_) :: _ ->
      let get_tag = function
        | {pat_desc = Tpat_construct (c,_)} -> c.cstr_tag
        | _ -> fatal_error "Parmatch.get_tag" in
      let all_tags =  List.map (fun (p,_) -> get_tag p) env in
      pat_of_constrs p (complete_constrs p all_tags)
| ({pat_desc = Tpat_variant(_,_,row)} as p,_) :: _ ->
    let tags =
      List.map
        (function ({pat_desc = Tpat_variant (tag, _, _)}, _) -> tag
                | _ -> assert false)
        env
    in
    let row = Btype.row_repr row in
    let make_other_pat tag const =
      let arg = if const then None else Some omega in
      make_pat (Tpat_variant(tag, arg, row)) p.pat_type p.pat_env in
    begin match
      List.fold_left
        (fun others (tag,f) ->
          if List.mem tag tags then others else
          match Btype.row_field_repr f with
            Rabsent (* | Reither _ *) -> others
          (* This one is called after erasing pattern info *)
          | Reither (c, _, _, _) -> make_other_pat tag c :: others
          | Rpresent arg -> make_other_pat tag (arg = None) :: others)
        [] row.row_fields
    with [] -> assert false
    | pat::other_pats ->
        List.fold_left
          (fun p_res pat ->
            make_pat (Tpat_or (pat, p_res)) p.pat_type p.pat_env)
          pat other_pats
    end
| ({pat_desc = Tpat_constant(Const_char _)} as p,_) :: _ ->
    let all_chars =
      List.map
        (fun (p,_) -> match p.pat_desc with
        | Tpat_constant (Const_char c) -> c
        | _ -> assert false)
        env in
    
    let rec find_other i imax =
      if i > imax then raise Not_found
      else
        let ci = Char.chr i in
        if List.mem ci all_chars then
          find_other (i+1) imax
        else
          make_pat (Tpat_constant (Const_char ci)) p.pat_type p.pat_env in
    let rec try_chars = function
      | [] -> omega
      | (c1,c2) :: rest ->
          try
            find_other (Char.code c1) (Char.code c2)
          with
          | Not_found -> try_chars rest in

    try_chars
      [ 'a', 'z' ; 'A', 'Z' ; '0', '9' ;
        ' ', '~' ; Char.chr 0 , Char.chr 255]
| ({pat_desc=(Tpat_constant (Const_int _))} as p,_) :: _ ->
    let all_ints =
      List.map
        (fun (p,_) -> match p.pat_desc with
        | Tpat_constant (Const_int i) -> i
        | _ -> assert false)
        env in
    let rec try_ints i =
      if List.mem i all_ints then try_ints (i+1)
      else
        make_pat
          (Tpat_constant (Const_int i)) p.pat_type p.pat_env in
    try_ints 0
| ({pat_desc=(Tpat_constant (Const_string _))} as p,_) :: _ ->
    let all_lengths =
      List.map
        (fun (p,_) -> match p.pat_desc with
        | Tpat_constant (Const_string s) -> String.length s
        | _ -> assert false)
        env in
    let rec try_strings i =
      if List.mem i all_lengths then try_strings (i+1)
      else
        make_pat
          (Tpat_constant (Const_string (String.make i '*')))
          p.pat_type p.pat_env in
    try_strings 0
| ({pat_desc=(Tpat_constant (Const_float _))} as p,_) :: _ ->
    let all_floats =
      List.map
        (fun (p,_) -> match p.pat_desc with
        | Tpat_constant (Const_float s) -> float_of_string s
        | _ -> assert false)
        env in
    let rec try_floats f =
      if List.mem f all_floats then try_floats (f +. 1.0)
      else
        make_pat
          (Tpat_constant (Const_float (string_of_float f^".0")))
          p.pat_type p.pat_env in
    try_floats 0.0
| ({pat_desc = Tpat_array args} as p,_)::_ ->
    let all_lengths =
      List.map
        (fun (p,_) -> match p.pat_desc with
        | Tpat_array args -> List.length args
        | _ -> assert false)
        env in
    let rec try_arrays l =
      if List.mem l all_lengths then try_arrays (l+1)
      else
        make_pat
          (Tpat_array (omegas l))
          p.pat_type p.pat_env in
    try_arrays 0
| [] -> omega
| _ -> omega  

(*
  Core function :
  Is the last row of pattern matrix pss + qs satisfiable ?
  That is :
    Does there exists at least one value vector, es such that :
     1- for all ps in pss ps # es (ps and es are not compatible)
     2- qs <= es                  (es matches qs)

  Moreover, when argument buid is true, a matching value is returned.
*)

type 'a result = Rnone | Rsome of 'a | Rok

let rec try_many f = function
  | [] -> Rnone
  | x::rest ->
      begin match f x with
      | Rnone -> try_many f rest
      | r -> r
      end
  
let rec satisfiable tdefs build pss qs =
  match pss with
    [] -> if build then Rsome qs else Rok (* qs is a matching vector *)
  | _ ->
    match qs with
      [] -> Rnone
    | {pat_desc = Tpat_or(q1,q2)}::qs ->
        begin match satisfiable tdefs build pss (q1::qs) with
        | Rnone -> satisfiable tdefs build pss (q2::qs)
        | r -> r
        end
    | {pat_desc = Tpat_alias(q,_)}::qs ->
        satisfiable tdefs build pss (q::qs)
    | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
        let q0 = discr_pat omega pss in     
        begin match filter_all q0 pss with
          (* first column of pss is made of variables only *)
          [] -> begin match satisfiable tdefs build (filter_extra pss) qs with
          | Rsome r -> Rsome (q0::r)
          | r -> r
          end
        | constrs ->          
            let try_non_omega (p,pss) =
              match
                satisfiable tdefs build pss (simple_match_args p omega @ qs)
              with
              | Rsome r -> Rsome (set_args p r)
              | r -> r in
            if full_match tdefs build constrs
            then try_many try_non_omega constrs
            else
              match satisfiable tdefs build (filter_extra pss) qs with
              | Rnone -> try_many try_non_omega constrs
              | Rok -> Rok
              | Rsome r ->
                  ignore (try_many try_non_omega constrs);
                  try
                    Rsome (build_other constrs::r)
                  with
      (* cannot occur, since constructors don't make a full signature *)
                  | Empty -> fatal_error "Parmatch.satisfiable"

        end
    | q::qs ->
        let q0 = discr_pat q pss in
        match
          satisfiable tdefs build (filter_one q0 pss)
            (simple_match_args q0 q @ qs)
        with
        | Rsome r -> Rsome (set_args q0 r)
        | r -> r


let has_guard act =
  match act.exp_desc with
    Texp_when(_, _) -> true
  | _ -> false

(*
  Build up a working pattern matrix.
    1- Retain minimal patterns (for optimizing when catch all's  are here)
    2- Forget about guarded patterns
*)

let rec initial_matrix = function
    [] -> []
  | (pat, act) :: rem ->
       if has_guard act
       then initial_matrix rem
       else [pat] :: initial_matrix rem

(* p less_equal means, forall B,  V matches q implies V mactches p *)
let rec le_pat p q =
  match (p.pat_desc, q.pat_desc) with
  | Tpat_var _,_ -> true | Tpat_any, _ -> true
  | Tpat_alias(p,_), _ -> le_pat p q
  | _, Tpat_alias(q,_) -> le_pat p q
  | _, Tpat_or(q1,q2) -> le_pat p q1 && le_pat p q2
  | Tpat_constant(c1), Tpat_constant(c2) -> c1 = c2
  | Tpat_construct(c1,ps), Tpat_construct(c2,qs) ->
      c1.cstr_tag = c2.cstr_tag && le_pats ps qs
  | Tpat_variant(l1,Some p1,_), Tpat_variant(l2,Some p2,_) ->
      l1 = l2 && le_pat p1 p2
  | Tpat_variant(l1,None,_), Tpat_variant(l2,None,_) -> l1 = l2
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_record l1, Tpat_record l2 ->
      let ps,qs = records_args l1 l2 in
      le_pats ps qs
  | Tpat_array(ps), Tpat_array(qs) ->
     List.length ps = List.length qs && le_pats ps qs
(* In all other cases, enumeration is performed *)
  | _,_  ->
      begin match satisfiable Env.empty false [[p]] [q] with
      | Rnone -> true
      | _ -> false
      end

      
and le_pats ps qs =
  match ps,qs with
    p::ps, q::qs -> le_pat p q && le_pats ps qs
  | _, _         -> true

let get_mins le ps =
  let rec select_rec r = function
    [] -> r
  | p::ps ->
      if List.exists (fun p0 -> le p0 p) ps
      then select_rec r ps
      else select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)

(*************************************)
(* Values as patterns pretty printer *)
(*************************************)

open Format
;;

let get_constr_name tag ty tenv  = match tag with
| Cstr_exception path -> Path.name path
| _ ->
  try
    let name,_ = get_constr tag ty tenv in name
  with
  | Datarepr.Constr_not_found -> "*Unknown constructor*"

let is_cons tag v  = match get_constr_name tag v.pat_type v.pat_env with
| "::" -> true
| _ -> false

  
let rec pretty_val ppf v = match v.pat_desc with
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var x -> Ident.print ppf x
  | Tpat_constant (Const_int i) -> fprintf ppf "%d" i
  | Tpat_constant (Const_char c) ->
      fprintf ppf "'%s'"  (Char.escaped c)
  | Tpat_constant (Const_string s) ->
      fprintf ppf "\"%s\"" (String.escaped s)
  | Tpat_constant (Const_float s) ->
      fprintf ppf "%s" s
  | Tpat_tuple vs ->
      fprintf ppf "@[(%a)@]" (pretty_vals ",") vs
  | Tpat_construct ({cstr_tag=tag},[]) ->
      let name = get_constr_name tag v.pat_type v.pat_env in
      fprintf ppf "%s" name
  | Tpat_construct ({cstr_tag=tag},[w]) ->
      let name = get_constr_name tag v.pat_type v.pat_env in
      fprintf ppf "@[<2>%s@ %a@]" name pretty_arg w
  | Tpat_construct ({cstr_tag=tag},vs) ->
      let name = get_constr_name tag v.pat_type v.pat_env in
      begin match (name, vs) with
        ("::", [v1;v2]) ->
          fprintf ppf "@[%a::@,%a@]" pretty_car v1 pretty_cdr v2
      |  _ ->
          fprintf ppf "@[<2>%s@ @[(%a)@]@]" name (pretty_vals ",") vs
      end
  | Tpat_variant (l, None, _) ->
      fprintf ppf "`%s" l
  | Tpat_variant (l, Some w, _) ->
      fprintf ppf "@[<2>`%s@ %a@]" l pretty_arg w
  | Tpat_record lvs ->
      fprintf ppf "@[{%a}@]"
        (pretty_lvals (get_record_labels v.pat_type v.pat_env))
        (List.filter
           (function
             | (_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
             | _ -> true) lvs)
  | Tpat_array vs ->
      fprintf ppf "@[[| %a |]@]" (pretty_vals " ;") vs
  | Tpat_alias (v,x) ->
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.print x
  | Tpat_or (v,w)    ->
      fprintf ppf "@[(%a|@,%a)@]" pretty_or v pretty_or w

and pretty_car ppf v = match v.pat_desc with
| Tpat_construct ({cstr_tag=tag}, [_ ; _])
    when is_cons tag v ->
      fprintf ppf "(%a)" pretty_val v
| _ -> pretty_val ppf v

and pretty_cdr ppf v = match v.pat_desc with
| Tpat_construct ({cstr_tag=tag}, [v1 ; v2])
    when is_cons tag v ->
      fprintf ppf "%a::@,%a" pretty_car v1 pretty_cdr v2
| _ -> pretty_val ppf v

and pretty_arg ppf v = match v.pat_desc with
| Tpat_construct (_,_::_) -> fprintf ppf "(%a)" pretty_val v
|  _ -> pretty_val ppf v

and pretty_or ppf v = match v.pat_desc with
| Tpat_or (v,w) ->
    fprintf ppf "%a|@,%a" pretty_or v pretty_or w
| _ -> pretty_val ppf v

and pretty_vals sep ppf = function
  | [] -> ()
  | [v] -> pretty_val ppf v
  | v::vs ->
      fprintf ppf "%a%s@ %a" pretty_val v sep (pretty_vals sep) vs

and pretty_lvals lbls ppf = function
  | [] -> ()
  | [lbl,v] ->
      let name = find_label lbl lbls in
      fprintf ppf "%s=%a" name pretty_val v
  | (lbl,v)::rest ->
      let name = find_label lbl lbls in
      fprintf ppf "%s=%a;@ %a" name pretty_val v (pretty_lvals lbls) rest

let top_pretty ppf v =
  fprintf ppf "@[%a@]@?" pretty_val v


(* p and q compatible means, there exists V that matches both *)

let rec compat p q =
  match p.pat_desc,q.pat_desc with
  | Tpat_alias (p,_),_     -> compat p q
  | _,Tpat_alias (q,_)     -> compat p q
  | (Tpat_any|Tpat_var _),_ -> true
  | _,(Tpat_any|Tpat_var _) -> true
  | Tpat_or (p1,p2),_       -> compat p1 q || compat p2 q
  | _,Tpat_or (q1,q2)       -> compat p q1 || compat p q2    
  | Tpat_constant c1, Tpat_constant c2 -> c1=c2
  | Tpat_tuple ps, Tpat_tuple qs -> compats ps qs
  | Tpat_construct (c1,ps1), Tpat_construct (c2,ps2) ->
      c1.cstr_tag = c2.cstr_tag && compats ps1 ps2
  | Tpat_variant(l1,Some p1,_), Tpat_variant(l2,Some p2,_) ->
      l1=l2 && compat p1 p2
  | Tpat_variant (l1,None,_), Tpat_variant(l2,None,_) -> l1 = l2
  | Tpat_variant (_, None, _), Tpat_variant (_,Some _, _) -> false
  | Tpat_variant (_, Some _, _), Tpat_variant (_, None, _) -> false
  | Tpat_record l1,Tpat_record l2 ->
      let ps,qs = records_args l1 l2 in
      compats ps qs
  | Tpat_array ps, Tpat_array qs ->
      List.length ps = List.length qs &&
      compats ps qs
  | _,_  ->
      assert false
        
and compats ps qs = match ps,qs with
| [], [] -> true
| p::ps, q::qs -> compat p q && compats ps qs
| _,_    -> assert false

(*
    lub p q is a pattern that matches all values matched by p and q
    may raise Empty, when p and q and not compatible
    Exact
*)

let rec lub p q = match p.pat_desc,q.pat_desc with
| Tpat_alias (p,_),_     -> lub p q
| _,Tpat_alias (q,_)     -> lub p q
| (Tpat_any|Tpat_var _),_ -> q
| _,(Tpat_any|Tpat_var _) -> p
| Tpat_or (p1,p2),_       -> orlub p1 p2 q
| _,Tpat_or (q1,q2)       -> orlub q1 q2 p (* Thanks god, lub is commutative *)
| Tpat_constant c1, Tpat_constant c2 when c1=c2 -> p
| Tpat_tuple ps, Tpat_tuple qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_tuple rs) p.pat_type p.pat_env
| Tpat_construct (c1,ps1), Tpat_construct (c2,ps2)
      when  c1.cstr_tag = c2.cstr_tag  ->
        let rs = lubs ps1 ps2 in
        make_pat (Tpat_construct (c1,rs)) p.pat_type p.pat_env
| Tpat_variant(l1,Some p1,row), Tpat_variant(l2,Some p2,_)
     when  l1=l2 ->
       let r=lub p1 p2 in
       make_pat (Tpat_variant (l1,Some r,row)) p.pat_type p.pat_env
| Tpat_variant (l1,None,_), Tpat_variant(l2,None,_)
     when l1 = l2 -> p
| Tpat_record l1,Tpat_record l2 ->
    let rs = record_lubs l1 l2 in
    make_pat (Tpat_record rs) p.pat_type p.pat_env
| Tpat_array ps, Tpat_array qs
    when List.length ps = List.length qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_array rs) p.pat_type p.pat_env
| _,_  ->
    raise Empty

and orlub p1 p2 q =
  try    
    let r1 = lub p1 q in
    try
      {q with pat_desc=(Tpat_or (r1,lub p2 q))}
    with
    | Empty -> r1
  with
  | Empty -> lub p2 q

and record_lubs l1 l2 =
  let l1 = sort_fields l1 and l2 = sort_fields l2 in
  let rec lub_rec l1 l2 = match l1,l2 with
  | [],_ -> l2
  | _,[] -> l1
  | (lbl1,p1)::rem1, (lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        (lbl1,p1)::lub_rec rem1 l2
      else if lbl2.lbl_pos < lbl1.lbl_pos  then
        (lbl2,p2)::lub_rec l1 rem2
      else
        (lbl1,lub p1 p2)::lub_rec rem1 rem2 in
  lub_rec l1 l2
         
and lubs ps qs = match ps,qs with
| p::ps, q::qs -> lub p q :: lubs ps qs
| _,_ -> []
    
    
(******************************)
(* Entry points               *)
(*    - Partial match         *)
(*    - Unused match case     *)
(******************************)


(*
  A small cvs commit/commit discussion....
  JG: 
    Exhaustiveness of matching MUST be checked, even
    when the warning is excluded explicitely by user.
  LM: 
    Why such a strange thing ? 
  JG:
    Because the typing of variants depends on it.
  LM:    
    Ok, note that by contrast, unused clause check still can be avoided at
        user request.
*)

let check_partial tdefs loc casel =
    let pss = get_mins le_pats (initial_matrix casel) in    
    match pss with
    | [] ->
        (*
          This can occur
            - For empty matches generated by ocamlp4
            - when all patterns have guards
           Then match should be considered non-exhaustive
           (cf. matching.ml) no warning is issued,
           users should know what they do 
         *)
        Partial
    | ps::_  ->
        match satisfiable tdefs true pss (omega_list ps) with
        | Rnone -> Total
        | Rok ->
            Location.prerr_warning loc (Warnings.Partial_match "");
            Partial
        | Rsome [v] ->
            let errmsg =
              try
                let buf = Buffer.create 16 in
                let fmt = formatter_of_buffer buf in
                top_pretty fmt v;
                Buffer.contents buf
              with _ ->
                "" in
            Location.prerr_warning loc (Warnings.Partial_match errmsg);
            Partial
        | _ ->
            fatal_error "Parmatch.check_partial"

let location_of_clause = function
    pat :: _ -> pat.pat_loc
  | _ -> fatal_error "Parmatch.location_of_clause"


let check_unused tdefs casel =
  if Warnings.is_active Warnings.Unused_match then
    let prefs =   
      List.fold_right
        (fun (pat,act as clause) r ->
          if has_guard act
          then ([], ([pat], act)) :: r
          else ([], ([pat], act)) :: 
            List.map (fun (pss,clause) -> [pat]::pss,clause) r)
        casel [] in
    List.iter
      (fun (pss, ((qs, _) as clause)) ->
        try
          if
            (match satisfiable tdefs false pss qs with
            | Rnone -> true
            | Rok -> false
            | _ -> assert false)
          then
            Location.prerr_warning
              (location_of_clause qs) Warnings.Unused_match
        with e ->
          Location.prerr_warning (location_of_clause qs)
            (Warnings.Other "Fatal Error") ;
          raise e)
      prefs




