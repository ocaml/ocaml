(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compilation of pattern matching *)

open Misc
open Location
open Asttypes
open Primitive
open Typedtree
open Lambda


(*  See Peyton-Jones, "The Implementation of functional programming
    languages", chapter 5. *)

type pattern_matching =
  { mutable cases : (pattern list * lambda) list;
    args  : lambda list }

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

(* To expand "or" patterns and remove aliases *)

let rec simplify = function
    ({pat_desc = Tpat_alias(p, id)} :: patl, action) :: rem ->
      simplify((p :: patl, action) :: rem)
  | ({pat_desc = Tpat_or(p1, p2)} :: patl, action) :: rem ->
      let shared_action = share_lambda action in
      simplify((p1 :: patl, shared_action) ::
               (p2 :: patl, shared_action) :: rem)
  | cases ->
      cases

(* Matching against a constant *)

let make_constant_matching (arg :: argl) =
  {cases = []; args = argl}

let divide_constant {cases = cl; args = al} =
  let rec divide cl =
    match simplify cl with
      ({pat_desc = Tpat_constant cst} :: patl, action) :: rem ->
        let (constants, others) = divide rem in
        (add make_constant_matching constants cst (patl, action) al, others)
    | cl ->
      ([], {cases = cl; args = al})
  in divide cl

(* Matching against a constructor *)

let make_constr_matching cstr (arg :: argl) =
  let (first_pos, last_pos) =
    match cstr.cstr_tag with
      Cstr_constant _ | Cstr_block _ -> (0, cstr.cstr_arity - 1)
    | Cstr_exception _ -> (1, cstr.cstr_arity) in
  let rec make_args pos =
    if pos > last_pos
    then argl
    else Lprim(Pfield pos, [arg]) :: make_args (pos + 1) in
  {cases = []; args = make_args first_pos}

let divide_constructor {cases = cl; args = al} =
  let rec divide cl =
    match simplify cl with
      ({pat_desc = Tpat_construct(cstr, args)} :: patl, action) :: rem ->
        let (constructs, others) = divide rem in
        (add (make_constr_matching cstr) constructs
             cstr.cstr_tag (args @ patl, action) al,
         others)
    | cl ->
      ([], {cases = cl; args = al})
  in divide cl

(* Matching against a variable *)

let divide_var {cases = cl; args = al} =
  let rec divide cl =
    match simplify cl with
      ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rem ->
        let (vars, others) = divide rem in
        (add_line (patl, action) vars, others)
    | cl ->
        (make_constant_matching al, {cases = cl; args = al})
  in divide cl

(* Matching against a tuple pattern *)

let make_tuple_matching num_comps = function
    [] -> fatal_error "Matching.make_tuple_matching"
  | Lprim(Pmakeblock _, components) :: argl ->
      {cases = []; args = components @ argl}
  | arg :: argl ->
      let rec make_args pos =
        if pos >= num_comps
        then argl
        else Lprim(Pfield pos, [arg]) :: make_args (pos + 1) in
      {cases = []; args = make_args 0}

let any_pat =
  {pat_desc = Tpat_any; pat_loc = Location.none; pat_type = Ctype.none}

let divide_tuple arity {cases = cl; args = al} =
  let rec divide cl =
    match simplify cl with
      ({pat_desc = Tpat_tuple args} :: patl, action) :: rem ->
        add_line (args @ patl, action) (divide rem)
    | ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rem ->
        let rec make_args n =
          if n >= arity then patl else any_pat :: make_args (n+1) in
        add_line (make_args 0, action) (divide rem)
    | [] ->
        make_tuple_matching arity al
  in divide cl

(* Matching against a record pattern *)

let make_record_matching all_labels (arg :: argl) =
  let rec make_args pos =
    if pos >= Array.length all_labels then argl else begin
      let lbl = all_labels.(pos) in
      match lbl.lbl_repres with
        Record_regular ->
          Lprim(Pfield lbl.lbl_pos, [arg]) :: make_args(pos + 1)
      | Record_float ->
          Lprim(Pfloatfield lbl.lbl_pos, [arg]) :: make_args(pos + 1)
    end in
  {cases = []; args = make_args 0}

let divide_record all_labels {cases = cl; args = al} =
  let num_fields = Array.length all_labels in
  let record_matching_line lbl_pat_list =
    let patv = Array.new num_fields any_pat in
    List.iter (fun (lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
    Array.to_list patv in
  let rec divide cl =
    match simplify cl with
      ({pat_desc = Tpat_record lbl_pat_list} :: patl, action) :: rem ->
        add_line (record_matching_line lbl_pat_list @ patl, action)
                 (divide rem)
    | ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rem ->
        add_line (record_matching_line [] @ patl, action) (divide rem)
    | [] ->
        make_record_matching all_labels al
  in divide cl

(* To List.combine sub-matchings together *)

let combine_var (lambda1, total1) (lambda2, total2) =
  if total1 then (lambda1, true) else (Lcatch(lambda1, lambda2), total2)

let make_test_sequence tst arg const_lambda_list =
  List.fold_right
    (fun (c, act) rem ->
      Lifthenelse(Lprim(tst, [arg; Lconst(Const_base c)]), act, rem))
    const_lambda_list Lstaticfail

let combine_constant arg cst (const_lambda_list, total1) (lambda2, total2) =
  let lambda1 =
    match cst with
      Const_int _ ->
        make_test_sequence (Pintcomp Ceq) arg const_lambda_list
    | Const_char _ ->
        let casel =
          List.map (fun (Const_char c, l) -> (Char.code c, l))
                   const_lambda_list in
        let (transl_table, actions, num_actions) =
          Dectree.make_decision_tree casel in
        Lswitch(Lprim(Ptranslate transl_table, [arg]),
                num_actions, actions, 0, [])
    | Const_string _ ->
        make_test_sequence
          (Pccall{prim_name = "string_equal";
                  prim_arity = 2; prim_alloc = false;
                  prim_native_name = ""; prim_native_float = false})
          arg const_lambda_list
    | Const_float _ ->
        make_test_sequence (Pfloatcomp Ceq) arg const_lambda_list
  in (Lcatch(lambda1, lambda2), total2)

let combine_constructor arg cstr (tag_lambda_list, total1) (lambda2, total2) =
  if cstr.cstr_consts < 0 then begin
    (* Special cases for exceptions *)
    let lambda1 =
      List.fold_right
        (fun (Cstr_exception path, act) rem ->
          Lifthenelse(Lprim(Pintcomp Ceq, 
                            [Lprim(Pfield 0, [arg]); transl_path path]),
                      act, rem))
        tag_lambda_list Lstaticfail
    in (Lcatch(lambda1, lambda2), total2)
  end else begin
    (* Regular concrete type *)
    let rec split_cases = function
      [] -> ([], [])
    | (cstr, act) :: rem ->
        let (consts, nonconsts) = split_cases rem in
        match cstr with
          Cstr_constant n -> ((n, act) :: consts, nonconsts)
        | Cstr_block n    -> (consts, (n, act) :: nonconsts) in
    let (consts, nonconsts) = split_cases tag_lambda_list in
    let lambda1 =
      match (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts) with
        (1, 0, [0, act], []) -> act
      | (0, 1, [], [0, act]) -> act
      | (1, 1, [0, act1], [0, act2]) ->
          Lifthenelse(arg, act2, act1)
      | (1, 1, [0, act1], []) ->
          Lifthenelse(arg, Lstaticfail, act1)
      | (1, 1, [], [0, act2]) ->
          Lifthenelse(arg, act2, Lstaticfail)
      | (_, _, _, _) ->
          Lswitch(arg, cstr.cstr_consts, consts,
                       cstr.cstr_nonconsts, nonconsts) in
    if total1
     & List.length tag_lambda_list = cstr.cstr_consts + cstr.cstr_nonconsts
    then (lambda1, true)
    else (Lcatch(lambda1, lambda2), total2)
  end

(* The main compilation function.
   Input: a pattern matching.
   Output: a lambda term, a "total" flag (true if we're sure that the
     matching covers all cases; this is an approximation). *)

let rec compile_match m =

  let rec compile_list = function
    [] -> ([], true)
  | (key, pm) :: rem ->
      let (lambda1, total1) = compile_match pm in
      let (list2, total2) = compile_list rem in
      ((key, lambda1) :: list2, total1 & total2) in

  match { cases = simplify m.cases; args = m.args } with
    { cases = [] } ->
      (Lstaticfail, false)
  | { cases = ([], action) :: rem; args = argl } ->
      if is_guarded action then begin
        let (lambda, total) = compile_match { cases = rem; args = argl } in
        (Lcatch(action, lambda), total)
      end else
        (action, true)
  | { cases = (pat :: patl, action) :: _; args = arg :: _ } as pm ->
      match pat.pat_desc with
        Tpat_any | Tpat_var _ ->
          let (vars, others) = divide_var pm in
          combine_var (compile_match vars) (compile_match others)
      | Tpat_constant cst ->
          let (constants, others) = divide_constant pm in
          combine_constant arg cst
            (compile_list constants) (compile_match others)
      | Tpat_tuple patl ->
          compile_match (divide_tuple (List.length patl) pm)
      | Tpat_construct(cstr, patl) ->
          let (constrs, others) = divide_constructor pm in
          combine_constructor arg cstr
            (compile_list constrs) (compile_match others)
      | Tpat_record((lbl, _) :: _) ->
          compile_match (divide_record lbl.lbl_all pm)

(* The entry points *)

let compile_matching handler_fun arg pat_act_list =
  let pm =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [arg] } in
  let (lambda, total) = compile_match pm in
  if total then lambda else Lcatch(lambda, handler_fun())

let partial_function loc () =
  Lprim(Praise, [Lprim(Pmakeblock 0,
          [transl_path Predef.path_match_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string !Location.input_name);
               Const_base(Const_int loc.loc_start);
               Const_base(Const_int loc.loc_end)]))])])

let for_function loc param pat_act_list =
  compile_matching (partial_function loc) param pat_act_list

let for_trywith param pat_act_list =
  compile_matching (fun () -> Lprim(Praise, [Lvar param]))
                   (Lvar param) pat_act_list

let for_let loc param pat body =
  compile_matching (partial_function loc) (Lvar param) [pat, body]

