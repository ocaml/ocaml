(* Detection of partial matches and unused match cases. *)

open Misc
open Asttypes
open Typedtree


let make_pat desc ty =
  {pat_desc = desc; pat_loc = Location.none; pat_type = ty}

let omega = make_pat Tpat_any Ctype.none

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = omegas(List.length l)

let has_guard act =
  match act.exp_desc with
    Texp_when(_, _) -> true
  | _ -> false

let simple_match p1 p2 = 
  match p1.pat_desc, p2.pat_desc with
    Tpat_construct(c1, _), Tpat_construct(c2, _) ->
      c1.cstr_tag = c2.cstr_tag
  | Tpat_constant(c1), Tpat_constant(c2) ->
      c1 = c2
  | Tpat_tuple(_), Tpat_tuple(_) -> true
  | Tpat_record(_), Tpat_record(_) -> true
  | _, (Tpat_any | Tpat_var(_)) -> true
  | _, _ -> false

(* Return the set of labels and number of fields for a record pattern. *)

let record_labels p =
  match p.pat_desc with
    Tpat_record((lbl1, pat1) :: rem) -> Array.to_list lbl1.lbl_all
  | _ -> fatal_error "Parmatch.record_labels"

let record_num_fields p =
  match p.pat_desc with
    Tpat_record((lbl1, pat1) :: rem) -> Array.length lbl1.lbl_all
  | _ -> fatal_error "Parmatch.record_num_fields"

let set_fields size l =
  let v = Array.new size omega in
  let rec change_rec l = match l with
    (lbl,p)::l ->  v.(lbl.lbl_pos) <- p ;  change_rec l 
  | [] -> () in
  change_rec l;
  Array.to_list v

let simple_match_args p1 p2 =
  match p2.pat_desc with
    Tpat_construct(cstr, args) -> args
  | Tpat_tuple(args)  -> args
  | Tpat_record(args) ->  set_fields (record_num_fields p1) args
  | (Tpat_any | Tpat_var(_)) ->
      begin match p1.pat_desc with
        Tpat_construct(_, args) -> omega_list args
      | Tpat_tuple(args) -> omega_list args
      | Tpat_record(args) ->  omega_list args
      | _ -> []
      end
  | _ -> []

(*
  Computes the discriminating pattern for matching by the first
  column of pss, that is:
     checks for a tuple or a record when q is a variable.
*)

let rec simple_pat q pss = match pss with
    ({pat_desc = Tpat_alias(p,_)}::ps)::pss -> 
        simple_pat q ((p::ps)::pss)
  | ({pat_desc = Tpat_or(p1,p2)}::ps)::pss ->
        simple_pat q ((p1::ps)::(p2::ps)::pss)
  | ({pat_desc = (Tpat_any | Tpat_var(_))}::_)::pss ->
        simple_pat q pss
  | (({pat_desc = Tpat_tuple(args)} as p)::_)::_ ->
        make_pat (Tpat_tuple(omega_list args)) p.pat_type
  | (({pat_desc = Tpat_record(args)} as p)::_)::pss ->
        make_pat (Tpat_record (List.map (fun lbl -> (lbl,omega)) (record_labels p)))
                 p.pat_type
  | _ -> q

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

let filter_all pat0 pss =

  let rec insert q qs env =
    match env with
      [] -> [q, [simple_match_args q q @ qs]]
    | ((p,pss) as c)::env ->
        if simple_match q p
        then (p, ((simple_match_args p q @ qs) :: pss)) :: env
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

      
let full_match env =
  match env with
    ({pat_desc = Tpat_construct(c,_)},_) :: _ ->
      List.length env = c.cstr_span
  | ({pat_desc = Tpat_constant(Const_char _)},_) :: _ ->
      List.length env = 256
  | ({pat_desc = Tpat_constant(_)},_) :: _ -> false
  | ({pat_desc = Tpat_tuple(_)},_) :: _ -> true
  | ({pat_desc = Tpat_record(_)},_) :: _ -> true
  | _ -> fatal_error "Parmatch.full_match"

(*
  Is the last row of pattern matrix pss + qs satisfiable ?
        That is :
  Does there List.exists at least one value vector, es such that :
   1/ for all ps in pss ps # es (ps and es are not compatible)
   2/ qs <= es                  (es matches qs)
*)

let rec satisfiable pss qs =
  match pss with
    [] -> true
  | _ ->
    match qs with
      [] -> false
    | {pat_desc = Tpat_or(q1,q2)}::qs ->
        satisfiable pss (q1::qs) or satisfiable pss (q2::qs)
    | {pat_desc = Tpat_alias(q,_)}::qs ->
        satisfiable pss (q::qs)
    | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
        let q0 = simple_pat omega pss in     
        begin match filter_all q0 pss with
          (* first column of pss is made of variables only *)
          [] -> satisfiable (filter_extra pss) qs 
        | constrs ->          
            let try_non_omega (p,pss) =
              satisfiable pss (simple_match_args p omega @ qs)  in
            if full_match constrs
            then List.exists try_non_omega constrs
            else satisfiable (filter_extra pss) qs or
                 List.exists try_non_omega constrs
        end
    | q::qs ->
        let q0 = simple_pat q pss in
        satisfiable (filter_one q0 pss) (simple_match_args q0 q @ qs)

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
      c1.cstr_tag = c2.cstr_tag & le_pats ps qs
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_record(l1), Tpat_record(l2) ->
     let size = record_num_fields p in
     le_pats (set_fields size l1) (set_fields size l2)
  | _, _ -> false  

and le_pats ps qs =
  match ps,qs with
    p::ps, q::qs -> le_pat p q & le_pats ps qs
  | _, _         -> true

let get_mins ps =
  let rec select_rec r = function
    [] -> r
  | p::ps ->
      if List.exists (fun p0 -> le_pats p0 p) ps
      then select_rec r ps
      else select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)

let check_partial loc casel =
  let pss = get_mins (initial_matrix casel) in
  if match pss with
      []     -> true
    | ps::_  -> satisfiable pss (List.map (fun _ -> omega) ps)
  then Location.print_warning loc "this pattern-matching is not exhaustive"

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
      if not (satisfiable pss qs) then
        Location.print_warning (location_of_clause qs)
                                "this match case is unused.")
    prefs
