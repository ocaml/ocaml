(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
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

let get_type_descr ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv ty) in
  match ty.desc with
  | Tconstr (path,_,_) -> Env.find_type path tenv
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
  | {type_kind = Type_record lbls} -> lbls
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

let sort_record p = match p.pat_desc with
| Tpat_record args ->
    make_pat
      (Tpat_record
         (Sort.list
            (fun (lbl1,_) (lbl2,_) ->
              lbl1.lbl_pos <= lbl2.lbl_pos)
            args))
      p.pat_type p.pat_env
| _ -> p

(* Build argument list when p2 >= p1, where p1 is a simple pattern *)
let simple_match_args p1 p2 =
  match p2.pat_desc with
    Tpat_construct(cstr, args) -> args
  | Tpat_tuple(args)  -> args
  | Tpat_record(args) ->  extract_fields (record_arg p1) args
  | Tpat_array(args) -> args
  | (Tpat_any | Tpat_var(_)) ->
      begin match p1.pat_desc with
        Tpat_construct(_, args) -> omega_list args
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
  | Tpat_any | Tpat_constant _ | Tpat_construct (_,[]) -> q
  | Tpat_var _ -> make_pat Tpat_any q.pat_type q.pat_env
  | Tpat_alias (p,_) -> normalize_pat q
  | Tpat_tuple (args) ->
      make_pat (Tpat_tuple (omega_list args)) q.pat_type q.pat_env
  | Tpat_construct  (c,args) ->
      make_pat (Tpat_construct (c,omega_list args)) q.pat_type q.pat_env
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
| {pat_desc = Tpat_array omegas} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_array args) q.pat_type q.pat_env::
    rest
| {pat_desc=Tpat_constant c} ->
    q::r
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

let full_match env =  match env with
| ({pat_desc = Tpat_construct ({cstr_tag=Cstr_exception _},_)},_)::_ ->
    false
| ({pat_desc = Tpat_construct(c,_)},_) :: _ ->
    List.length env = c.cstr_consts + c.cstr_nonconsts
| ({pat_desc = Tpat_constant(Const_char _)},_) :: _ ->
    List.length env = 256
| ({pat_desc = Tpat_constant(_)},_) :: _ -> false
| ({pat_desc = Tpat_tuple(_)},_) :: _ -> true
| ({pat_desc = Tpat_record(_)},_) :: _ -> true
| ({pat_desc = Tpat_array(_)},_) :: _ -> false
| _ -> fatal_error "Parmatch.full_match"

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
| ({pat_desc = Tpat_construct (c,_)} as p,_) :: _ ->
    begin try

      let get_tag = function
        | {pat_desc = Tpat_construct (c,_)} -> c.cstr_tag
        | _ -> fatal_error "Parmatch.get_tag" in
      let all_tags =
        List.map (fun (p,_) -> get_tag p) env in

      let not_tags = complete_tags  c.cstr_consts c.cstr_nonconsts all_tags in
      let make_other_pat tag =
        let _,targs = get_constr tag p.pat_type p.pat_env in
        make_pat
          (Tpat_construct
             ({c with
      cstr_tag = tag ;
      cstr_args = targs ;
      cstr_arity = List.length targs},
              omega_list targs))
          p.pat_type p.pat_env in
      begin match not_tags with
      | [] -> omega (* should not occur, because full_match env is true *)
      | t::rest ->
          List.fold_left
            (fun p_res tag ->
              make_pat
                (Tpat_or (make_other_pat tag, p_res))
                p.pat_type p.pat_env)
            (make_other_pat t)
            rest
      end
    with
    | Datarepr.Constr_not_found -> omega
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
  
let rec satisfiable build pss qs =
  match pss with
    [] -> if build then Rsome qs else Rok (* qs is a matching vector *)
  | _ ->
    match qs with
      [] -> Rnone
    | {pat_desc = Tpat_or(q1,q2)}::qs ->
        begin match satisfiable build pss (q1::qs) with
        | Rnone -> satisfiable build pss (q2::qs)
        | r -> r
        end
    | {pat_desc = Tpat_alias(q,_)}::qs ->
        satisfiable build pss (q::qs)
    | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
        let q0 = discr_pat omega pss in     
        begin match filter_all q0 pss with
          (* first column of pss is made of variables only *)
          [] -> begin match satisfiable build (filter_extra pss) qs with
          | Rsome r -> Rsome (q0::r)
          | r -> r
          end
        | constrs ->          
            let try_non_omega (p,pss) =
              match satisfiable build pss (simple_match_args p omega @ qs) with
              | Rsome r -> Rsome (set_args p r)
              | r -> r in
            if full_match constrs
            then try_many try_non_omega constrs
            else
              match satisfiable build (filter_extra pss) qs with
              | Rnone -> try_many try_non_omega constrs
              | Rok -> Rok
              | Rsome r ->  Rsome (build_other constrs::r)
        end
    | q::qs ->
        let q0 = discr_pat q pss in
        match
          satisfiable build (filter_one q0 pss) (simple_match_args q0 q @ qs)
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

let rec le_pat p q =
  match (p.pat_desc, q.pat_desc) with
    (Tpat_var _ | Tpat_any), _ -> true
  | Tpat_alias(p,_), _ -> le_pat p q
  | _, Tpat_alias(q,_) -> le_pat p q
  | Tpat_or(p1,p2), _ -> le_pat p1 q or le_pat p2 q
  | _, Tpat_or(q1,q2) -> le_pat p q1 & le_pat p q2
  | Tpat_constant(c1), Tpat_constant(c2) -> c1 = c2
  | Tpat_construct(c1,ps), Tpat_construct(c2,qs) ->
      c1.cstr_tag = c2.cstr_tag && le_pats ps qs
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_record l1, Tpat_record l2 ->
      let ps = List.map (fun (_,p) -> p) l1
      and qs = extract_fields l1 l2 in
      le_pats ps qs
  | Tpat_array(ps), Tpat_array(qs) ->
     List.length ps = List.length qs && le_pats ps qs
  | _, _ -> false  

and le_pats ps qs =
  match ps,qs with
    p::ps, q::qs -> le_pat p q && le_pats ps qs
  | _, _         -> true

let get_mins ps =
  let rec select_rec r = function
    [] -> r
  | p::ps ->
      if List.exists (fun p0 -> le_pats p0 p) ps
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
  | Tpat_any | Tpat_var _ -> fprintf ppf "_"
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
  | Tpat_record lvs ->
      fprintf ppf "@[{%a}@]"
        (pretty_lvals (get_record_labels v.pat_type v.pat_env))
        (List.filter
           (function
             | (_,{pat_desc=Tpat_any}) -> false
             | _ -> true) lvs)
  | Tpat_array vs ->
      fprintf ppf "@[[| %a |]@]" (pretty_vals " ;") vs
  | Tpat_alias (v,_) -> pretty_val ppf v
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


(******************************)
(* Exported functions         *)
(*    - Partial match         *)
(*    - Unused match case     *)
(******************************)

let check_partial loc casel =
  let pss = get_mins (initial_matrix casel) in
  let r = match pss with
  | []     -> begin match casel with
    | [] -> Rnone
    | (p,_) :: _ -> Rsome [p]
  end
  | ps::_  -> satisfiable true pss (omega_list ps) in
  match r with
  | Rnone -> ()
  | Rok ->
      Location.print_warning loc (Warnings.Partial_match "")
  | Rsome [v] ->
      let errmsg =
        try
          let buf = Buffer.create 16 in
          let fmt = formatter_of_buffer buf in
          top_pretty fmt v;
          Buffer.contents buf
        with _ ->
          "" in
      Location.print_warning loc (Warnings.Partial_match errmsg)
  | _ ->
      fatal_error "Parmatch.check_partial"

let location_of_clause = function
    pat :: _ -> pat.pat_loc
  | _ -> fatal_error "Parmatch.location_of_clause"

let check_unused casel =
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
        (match satisfiable false pss qs with
        | Rnone -> true
        | Rok -> false
        | _ -> assert false)
      then
        Location.print_warning (location_of_clause qs) Warnings.Unused_match
      with e ->
        Location.print_warning (location_of_clause qs)
          (Warnings.Other "Fatal Error") ;
        raise e)
    prefs
