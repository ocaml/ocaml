open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch
open Printf
open Matchcommon

(************************************************)
(* Get rid of variables, aliases and or-pattern *)
(************************************************)

(* Alpha-conversion to fresh variables of variables
   bound by patterp in action lam *)
let alpha_ids alpha_pat ids p lam =
  let env =
    List.map
      (fun id ->
	let nid = Ident.create (Ident.name id) in
	id,nid)
      ids in
  let p = alpha_pat env p in
  let tbl =
    List.fold_right
      (fun (id,nid) tbl -> Ident.add id (Lvar nid) tbl)
      env Ident.empty in
  let lam = subst_lambda tbl lam in
  p,lam

let alpha p lam =
  let ids = IdentSet.elements (extract_vars IdentSet.empty p) in
  alpha_ids alpha_pat ids p lam

(* Out module, can produce lambda code or a dag *)
module type Out = sig
  type out
  (* forth and back *)
  val final : lambda -> out
  val to_lambda : out -> lambda

  (* need that for guards *)
  val is_guarded : out -> bool
  val patch_guarded : out -> out -> out
  val event_branch : int ref option -> out -> out

  (* constructors *)
  val alias : Ident.t -> Ident.t -> out -> out
  val field_ids : Discr.discr -> Ident.t ->
    Ident.t list * (let_kind * lambda) list
  val get_fields : Ident.t list -> (let_kind * lambda) list -> out -> out
  val switch : Ident.t -> (Discr.discr * out) list -> out option -> out
end

module OutLambda = struct
  type out = lambda
  let final lam = lam
  let to_lambda lam = lam

  let is_guarded = is_guarded
  let patch_guarded = patch_guarded
  let event_branch = event_branch

  let alias = Discr.alias
  let field_ids = Discr.field_ids
  let get_fields = Discr.get_fields
  let switch = Discr.switch
end

module Out : Out = OutLambda

(* Variables, aliases and or-pattern
   are handled by preprocessing,
   Records are also normalized *)

let precompile x cls =
  
  let rec simplify = function
    | [] -> []
    | ((pat :: patl, action) as cl) :: rem ->
        begin match pat.pat_desc with
        | Tpat_var id ->
            (omega :: patl, Out.alias id x action) ::
            simplify rem
        | Tpat_any ->
            cl :: simplify rem
        | Tpat_alias(p, id) ->
            simplify ((p :: patl, Out.alias id x action) :: rem)
        | Tpat_record [] ->
            (omega :: patl, action)::
            simplify rem
        | Tpat_record lbls ->
            let all_lbls = all_record_args lbls in
            let full_pat = {pat with pat_desc=Tpat_record all_lbls} in
            (full_pat::patl,action)::
            simplify rem
        | Tpat_or (p1,p2,_) -> (* or expansion *)
	    let p2,act2 = (* alpha p2 action *) p2,action in
	    (* alpha-conversion not needed, all variables in pat
	       will be bound to another variable *)
            simplify ((p1::patl,action)::(p2::patl,act2) :: rem)
        | _ -> cl :: simplify rem
        end
    | _ -> assert false in

  simplify cls

(* Spot variable pattern *)
let rec is_var_pat p = match p.pat_desc with
| Tpat_any|Tpat_var _ -> true
| Tpat_alias (p,_) -> is_var_pat p
| Tpat_or (p1,p2,_) -> is_var_pat p1 && is_var_pat p2
| _ -> false

let all_vars ps = List.for_all is_var_pat ps

let rec compile_match repr xs pss lam_fail = match pss with
| [] -> lam_fail
| (ps,act)::pss when all_vars ps ->
    compile_row repr xs ps act xs pss lam_fail
| _ -> match xs with
  | x::xs ->
      do_compile_matching repr x xs pss lam_fail
  | [] -> assert false

and compile_row repr xs ps act ys pss lam_fail = match xs,ps with
| [],[] ->
    if Out.is_guarded act then
      let lam = compile_match None ys pss lam_fail in
      Out.event_branch repr (Out.patch_guarded lam act)
    else
      Out.event_branch repr act
| x::xs,p::ps ->
    begin match p.pat_desc with
    | Tpat_any -> compile_row repr xs ps act ys pss lam_fail
    | Tpat_var id ->
	compile_row repr xs ps (Out.alias id x act) ys pss lam_fail
    | Tpat_alias (p,id) ->
	compile_row repr (x::xs) (p::ps) (Out.alias id x act) ys pss lam_fail
    | Tpat_or (p,_,_) ->
	compile_row repr (x::xs) (p::ps) act ys pss lam_fail
    | _ -> assert false
    end
| _,_ -> assert false

and do_compile_matching repr x xs pss lam_fail =
  let pss = precompile x pss in
  let ds = Discr.collect pss in
(*
  prerr_endline "MATCH" ;
  pretty_match pss ;
  prerr_endline "" ;
*)
  let cls =
    Discr.DSet.fold
      (fun d k ->
	let pss = Discr.specialize d pss in
	let ys,es = Out.field_ids d x in
	let lam = compile_match repr (ys@xs) pss lam_fail in
	(d,Out.get_fields ys es lam)::k)
      ds [] in
  Out.switch x cls
    (if Discr.has_default ds then
      Some (compile_match repr xs (Discr.default pss) lam_fail)
    else
      None)
;;


(* Free variables in a pattern *)
let vars_pat p =
  IdentSet.elements (extract_vars IdentSet.empty p)

and vars_pats ps =
  IdentSet.elements
    (List.fold_left extract_vars IdentSet.empty ps)


(* Split actions into exit/trap pair *)
let rec split_guarded lam_exit = function
  | Lifthenelse (cond, body, Lstaticraise (0,[])) ->
      Lifthenelse (cond, lam_exit, Lstaticraise (0,[])),body
  | Llet(str, id, lam, body) ->
      let lam1,lam2 = split_guarded lam_exit body in
      Llet (str, id, lam, lam1),lam2
  | Levent(lam, ev) ->
      let lam1,lam2 = split_guarded lam_exit lam in
      Levent (lam1, ev),lam2
  | _ -> assert false
;;

let split alpha_ids vars_pat pat_act_list =
  List.fold_right
    (fun (p,act) cls ->
      let num_exit = next_raise_count ()
      and ids = vars_pat p in
      let lam_exit =
	Lstaticraise (num_exit, List.map (fun id -> Lvar id) ids) in
      let match_act, def_act =
	if is_guarded act then
	  split_guarded lam_exit act
	else
	  lam_exit, act in
      let p,match_act = alpha_ids ids p match_act in
      ((p,match_act),(num_exit, ids, def_act))::cls)
    pat_act_list []
;;

(* Translate actions to out.out and back *)

let compile_match_out repr xs pss fail =
  let pss = List.map (fun (ps,lam) -> ps,Out.final lam) pss
  and fail = Out.final fail in
  let out = compile_match repr xs pss fail in
  Out.to_lambda out

(******************)

let add_defs lam defs =
  List.fold_left
    (fun lam (num,ids,body) -> Lstaticcatch (lam,(num,ids),body))
    lam defs

let compile_matching loc repr handler_fun arg pat_act_list _partial =
  let v =
    match arg with Lvar v -> v | _ -> Ident.create "m" in
  let pat_exits = split (alpha_ids alpha_pat) vars_pat pat_act_list in
  let pat_act_list,defs = List.split pat_exits in
  let pss = List.map (fun (p,act)  -> [p],act) pat_act_list
  and num_fail = next_raise_count () in
  let lam =
    compile_match_out repr [v] pss (Lstaticraise (num_fail,[])) in
  let lam = bind Strict v arg lam in
  let lam = add_defs lam defs in
  Lstaticcatch (lam,(num_fail,[]),handler_fun ())



(************************)

let rec super_flatten size (p,act) k = match p.pat_desc with
| Tpat_any -> (omegas size,act)::k
| Tpat_tuple ps -> (ps,act)::k
| Tpat_or (p1,p2,_) ->
    super_flatten size (p1,act) (super_flatten size (alpha p2 act) k)
| _ -> raise Cannot_flatten
      

let for_multiple_match loc args pat_act_list _partial =  
  let repr = None in
  try
    let pat_exits = split (alpha_ids alpha_pat) vars_pat pat_act_list in
    let pss,defs =  List.split pat_exits in
    let pss = List.fold_right (super_flatten (List.length args)) pss [] in
    let xs =
      List.map
	(function | Lvar v -> v | lam -> Ident.create "m")
	args in
    let num_fail = next_raise_count () in
    let lam = compile_match_out repr xs pss (Lstaticraise (num_fail,[])) in
    let lam =
      List.fold_right2
	(fun x arg lam -> bind Strict x arg lam)
	xs args lam in
    let lam = add_defs lam defs in
    Lstaticcatch (lam,(num_fail,[]), partial_function loc ())
  with
  | Cannot_flatten ->
      let arg =  (Lprim(Pmakeblock(0, Immutable), args)) in
      compile_matching loc repr (partial_function loc) arg pat_act_list _partial

(************************)

let for_tupled_function loc xs pats_act_list  _partial =
  let pats_exits =
    split
      (alpha_ids (fun env ps -> List.map (alpha_pat env) ps))
      vars_pats pats_act_list in
  let pss,defs =  List.split pats_exits in 
  let num_fail = next_raise_count () in
  let lam = compile_match_out None xs pss (Lstaticraise (num_fail,[])) in
  let lam =  add_defs lam defs in
  Lstaticcatch (lam,(num_fail,[]),partial_function loc ())
	  
