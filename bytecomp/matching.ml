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
  | _ -> Ident.new default

(* To expand "or" patterns, remove aliases, and bind named components *)

let any_pat =
  {pat_desc = Tpat_any; pat_loc = Location.none; pat_type = Ctype.none}

let simplify_matching m =
  match m.args with
    [] -> m
  | (arg, mut) :: argl ->
      let rec simplify = function
        (pat :: patl, action as patl_action) :: rem ->
          begin match pat.pat_desc with
            Tpat_var id ->
              (any_pat :: patl, Llet(Alias, id, arg, action)) ::
              simplify rem
          | Tpat_alias(p, id) ->
              simplify ((p :: patl, Llet(Alias, id, arg, action)) :: rem)
          | Tpat_or(p1, p2) ->
              let shared_action = share_lambda action in
              simplify ((p1 :: patl, shared_action) ::
                        (p2 :: patl, shared_action) :: rem)
          | _ ->
              patl_action :: simplify rem
          end
      | cases -> cases in
    { args = m.args; cases = simplify m.cases }

(* Matching against a constant *)

let make_constant_matching = function
    [] -> fatal_error "Matching.make_constant_matching"
  | (arg :: argl) -> {cases = []; args = argl}

let divide_constant {cases = cl; args = al} =
  let rec divide = function
      ({pat_desc = Tpat_constant cst} :: patl, action) :: rem ->
        let (constants, others) = divide rem in
        (add make_constant_matching constants cst (patl, action) al, others)
    | cl ->
      ([], {cases = cl; args = al})
  in divide cl

(* Matching against a constructor *)

let make_constr_matching cstr = function
    [] -> fatal_error "Matching.make_constr_matching"
  | ((arg, mut) :: argl) ->
      let (first_pos, last_pos) =
        match cstr.cstr_tag with
          Cstr_constant _ | Cstr_block _ -> (0, cstr.cstr_arity - 1)
        | Cstr_exception _ -> (1, cstr.cstr_arity) in
      let rec make_args pos =
        if pos > last_pos
        then argl
        else (Lprim(Pfield pos, [arg]), Alias) :: make_args (pos + 1) in
      {cases = []; args = make_args first_pos}

let divide_constructor {cases = cl; args = al} =
  let rec divide = function
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
  let rec divide = function
      ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        let (vars, others) = divide rem in
        (add_line (patl, action) vars, others)
    | cl ->
        (make_constant_matching al, {cases = cl; args = al})
  in divide cl

(* Matching against a tuple pattern *)

let make_tuple_matching num_comps = function
    [] -> fatal_error "Matching.make_tuple_matching"
  | (arg, mut) :: argl ->
      let rec make_args pos =
        if pos >= num_comps
        then argl
        else (Lprim(Pfield pos, [arg]), Alias) :: make_args (pos + 1) in
      {cases = []; args = make_args 0}

let divide_tuple arity {cases = cl; args = al} =
  let rec divide = function
      ({pat_desc = Tpat_tuple args} :: patl, action) :: rem ->
        add_line (args @ patl, action) (divide rem)
    | ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        add_line (replicate_list any_pat arity @ patl, action) (divide rem)
    | _ ->
        make_tuple_matching arity al
  in divide cl

(* Matching against a record pattern *)

let make_record_matching all_labels = function
    [] -> fatal_error "Matching.make_tuple_matching"
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
            | Mutable -> Strict in
          (Lprim(access, [arg]), str) :: make_args(pos + 1)
        end in
      {cases = []; args = make_args 0}

let divide_record all_labels {cases = cl; args = al} =
  let num_fields = Array.length all_labels in
  let record_matching_line lbl_pat_list =
    let patv = Array.new num_fields any_pat in
    List.iter (fun (lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
    Array.to_list patv in
  let rec divide = function
      ({pat_desc = Tpat_record lbl_pat_list} :: patl, action) :: rem ->
        add_line (record_matching_line lbl_pat_list @ patl, action)
                 (divide rem)
    | ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        add_line (record_matching_line [] @ patl, action) (divide rem)
    | _ ->
        make_record_matching all_labels al
  in divide cl

(* To combine sub-matchings together *)

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

  match m with
    { cases = [] } ->
      (Lstaticfail, false)
  | { cases = ([], action) :: rem; args = argl } ->
      if is_guarded action then begin
        let (lambda, total) = compile_match { cases = rem; args = argl } in
        (Lcatch(action, lambda), total)
      end else
        (action, true)
  | { args = (arg, str) :: argl } ->
      let v = name_pattern "match" m.cases in
      let newarg = Lvar v in
      let pm =
        simplify_matching
          { cases = m.cases; args = (newarg, Alias) :: argl } in
      let (lam, total) =
        match pm.cases with
          (pat :: patl, action) :: _ ->
            begin match pat.pat_desc with
              Tpat_any ->
                let (vars, others) = divide_var pm in
                combine_var (compile_match vars) (compile_match others)
            | Tpat_constant cst ->
                let (constants, others) = divide_constant pm in
                combine_constant newarg cst
                  (compile_list constants) (compile_match others)
            | Tpat_tuple patl ->
                compile_match (divide_tuple (List.length patl) pm)
            | Tpat_construct(cstr, patl) ->
                let (constrs, others) = divide_constructor pm in
                combine_constructor newarg cstr
                  (compile_list constrs) (compile_match others)
            | Tpat_record((lbl, _) :: _) ->
                compile_match (divide_record lbl.lbl_all pm)
            | _ ->
                fatal_error "Matching.compile_match1"
            end
        | _ -> fatal_error "Matching.compile_match2" in
      (Llet(str, v, arg, lam), total)

(* The entry points *)

let compile_matching handler_fun arg pat_act_list =
  let pm =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [arg, Strict] } in
  let (lambda, total) = compile_match pm in
  if total then lambda else Lcatch(lambda, handler_fun())

let partial_function loc () =
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_match_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string !Location.input_name);
               Const_base(Const_int loc.loc_start);
               Const_base(Const_int loc.loc_end)]))])])

let for_function loc param pat_act_list =
  compile_matching (partial_function loc) param pat_act_list

let for_trywith param pat_act_list =
  compile_matching (fun () -> Lprim(Praise, [param])) param pat_act_list

let for_let loc param pat body =
  compile_matching (partial_function loc) param [pat, body]

let for_multiple_match loc paraml pat_act_list =
  let pm1 =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [Lprim(Pmakeblock(0, Immutable), paraml), Strict] } in
  let pm2 =
    simplify_matching pm1 in
  let rec flatten_patterns = function
      ({pat_desc = Tpat_tuple args} :: _, action) :: rem ->
        (args, action) :: flatten_patterns rem
    | ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        (replicate_list any_pat (List.length paraml), action) ::
        flatten_patterns rem
    | _ ->
        [] in
  let pm3 =
    { cases = flatten_patterns pm2.cases;
      args = List.map (fun lam -> (lam, Strict)) paraml } in
  let (lambda, total) = compile_match pm3 in
  if total then lambda else Lcatch(lambda, partial_function loc ())
