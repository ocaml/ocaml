(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compilation of pattern matching *)

open Misc
open Location
open Asttypes
open Primitive
open Types
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
  | _ -> Ident.create default

(* To remove aliases and bind named components *)

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
        let (tuples, others) = divide rem in
        (add_line (args @ patl, action) tuples, others)
    | ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        let (tuples, others) = divide rem in
        (add_line (replicate_list any_pat arity @ patl, action) tuples, others)
    | cl ->
        (make_tuple_matching arity al, {cases = cl; args = al})
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
            | Mutable -> StrictOpt in
          (Lprim(access, [arg]), str) :: make_args(pos + 1)
        end in
      {cases = []; args = make_args 0}

let divide_record all_labels {cases = cl; args = al} =
  let num_fields = Array.length all_labels in
  let record_matching_line lbl_pat_list =
    let patv = Array.create num_fields any_pat in
    List.iter (fun (lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
    Array.to_list patv in
  let rec divide = function
      ({pat_desc = Tpat_record lbl_pat_list} :: patl, action) :: rem ->
        let (records, others) = divide rem in
        (add_line (record_matching_line lbl_pat_list @ patl, action) records,
         others)
    | ({pat_desc = Tpat_any} :: patl, action) :: rem ->
        let (records, others) = divide rem in
        (add_line (record_matching_line [] @ patl, action) records, others)
    | cl ->
        (make_record_matching all_labels al, {cases = cl; args = al})
  in divide cl

(* Matching against an or pattern. *)

let rec flatten_orpat_match pat =
  match pat.pat_desc with
    Tpat_or(p1, p2) -> flatten_orpat_match p1 @ flatten_orpat_match p2
  | _ -> [[pat], lambda_unit]

let divide_orpat = function
    {cases = (orpat :: patl, act) :: casel; args = arg1 :: argl as args} ->
      ({cases = flatten_orpat_match orpat; args = [arg1]},
       {cases = [patl, act]; args = argl},
       {cases = casel; args = args})
  | _ ->
    fatal_error "Matching.divide_orpat"

(* To combine sub-matchings together *)

let combine_var (lambda1, total1) (lambda2, total2) =
  if total1 then (lambda1, true)
  else if lambda2 = Lstaticfail then (lambda1, total1)
  else (Lcatch(lambda1, lambda2), total2)

let make_test_sequence tst arg const_lambda_list =
  List.fold_right
    (fun (c, act) rem ->
      Lifthenelse(Lprim(tst, [arg; Lconst(Const_base c)]), act, rem))
    const_lambda_list Lstaticfail

let make_switch_or_test_sequence arg const_lambda_list int_lambda_list =
  let min_key =
    List.fold_right (fun (k, l) m -> min k m) int_lambda_list max_int in
  let max_key =
    List.fold_right (fun (k, l) m -> max k m) int_lambda_list min_int in
  (* min_key and max_key can be arbitrarily large, so watch out for
     overflow in the following comparison *)
  if List.length int_lambda_list <= 1 + max_key / 4 - min_key / 4 then
    (* Sparse matching -- use a sequence of tests
       (4 bytecode instructions per test)  *)
    make_test_sequence (Pintcomp Ceq) arg const_lambda_list
  else begin
    (* Dense matching -- use a jump table
       (2 bytecode instructions + 1 word per entry in the table) *)
    let numcases = max_key - min_key + 1 in
    let cases =
      List.map (fun (key, l) -> (key - min_key, l)) int_lambda_list in
    let offsetarg =
      if min_key = 0 then arg else Lprim(Poffsetint(-min_key), [arg]) in
    Lswitch(offsetarg,
            {sw_numconsts = numcases; sw_consts = cases;
             sw_numblocks = 0; sw_blocks = []; sw_checked = true})
  end

let make_bitvect_check arg int_lambda_list =
  let bv = String.make 32 '\000' in
  List.iter
    (fun (n, l) ->
      bv.[n lsr 3] <- Char.chr(Char.code bv.[n lsr 3] lor (1 lsl (n land 7))))
    int_lambda_list;
  Lifthenelse(Lprim(Pbittest, [Lconst(Const_base(Const_string bv)); arg]),
              lambda_unit, Lstaticfail)

let prim_string_equal =
  Pccall{prim_name = "string_equal";
         prim_arity = 2; prim_alloc = false;
         prim_native_name = ""; prim_native_float = false}

let combine_constant arg cst (const_lambda_list, total1) (lambda2, total2) =
  let lambda1 =
    match cst with
      Const_int _ ->
        let int_lambda_list =
          List.map (function Const_int n, l -> n,l | _ -> assert false)
                   const_lambda_list in
        make_switch_or_test_sequence arg const_lambda_list int_lambda_list
    | Const_char _ ->
        let int_lambda_list =
          List.map (function Const_char c, l -> (Char.code c, l)
                           | _ -> assert false)
                   const_lambda_list in
        if List.for_all (fun (c, l) -> l = lambda_unit) const_lambda_list then
          make_bitvect_check arg int_lambda_list 
        else
          make_switch_or_test_sequence arg const_lambda_list int_lambda_list
    | Const_string _ ->
        make_test_sequence prim_string_equal arg const_lambda_list
    | Const_float _ ->
        make_test_sequence (Pfloatcomp Ceq) arg const_lambda_list
  in (Lcatch(lambda1, lambda2), total2)

let combine_constructor arg cstr (tag_lambda_list, total1) (lambda2, total2) =
  if cstr.cstr_consts < 0 then begin
    (* Special cases for exceptions *)
    let lambda1 =
      List.fold_right
        (fun (ex, act) rem ->
           match ex with
           | Cstr_exception path ->
               Lifthenelse(Lprim(Pintcomp Ceq, 
                                 [Lprim(Pfield 0, [arg]); transl_path path]),
                           act, rem)
           | _ -> assert false)
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
        | Cstr_block n    -> (consts, (n, act) :: nonconsts)
        | _ -> assert false in
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
          Lswitch(arg, {sw_numconsts = cstr.cstr_consts;
                        sw_consts = consts;
                        sw_numblocks = cstr.cstr_nonconsts;
                        sw_blocks = nonconsts;
                        sw_checked = false}) in
    if total1
     & List.length tag_lambda_list = cstr.cstr_consts + cstr.cstr_nonconsts
    then (lambda1, true)
    else (Lcatch(lambda1, lambda2), total2)
  end

let combine_orpat (lambda1, total1) (lambda2, total2) (lambda3, total3) =
  (Lcatch(Lsequence(lambda1, lambda2), lambda3), total3)

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
  | (_, Some r) ->
(*      incr r;
      Levent(lam, {lev_loc = -1;
                   lev_kind = Lev_before;
                   lev_repr = repr;
                   lev_env = Env.Env_empty})
*)      fatal_error "Matching.event_branch"
  end

(* The main compilation function.
   Input: a pattern matching.
   Output: a lambda term, a "total" flag (true if we're sure that the
     matching covers all cases; this is an approximation). *)

let rec compile_match repr m =

  let rec compile_list = function
    [] -> ([], true)
  | (key, pm) :: rem ->
      let (lambda1, total1) = compile_match repr pm in
      let (list2, total2) = compile_list rem in
      ((key, lambda1) :: list2, total1 & total2) in

  match m with
    { cases = [] } ->
      (Lstaticfail, false)
  | { cases = ([], action) :: rem; args = argl } ->
      if is_guarded action then begin
        let (lambda, total) =
          compile_match None { cases = rem; args = argl }
        in
        (Lcatch(event_branch repr action, lambda), total)
      end else
        (event_branch repr action, true)
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
                combine_var (compile_match repr vars)
                            (compile_match repr others)
            | Tpat_constant cst ->
                let (constants, others) = divide_constant pm in
                combine_constant newarg cst
                  (compile_list constants) (compile_match repr others)
            | Tpat_tuple patl ->
                let (tuples, others) = divide_tuple (List.length patl) pm in
                combine_var (compile_match repr tuples)
                            (compile_match repr others)
            | Tpat_construct(cstr, patl) ->
                let (constrs, others) = divide_constructor pm in
                combine_constructor newarg cstr
                  (compile_list constrs) (compile_match repr others)
            | Tpat_record((lbl, _) :: _) ->
                let (records, others) = divide_record lbl.lbl_all pm in
                combine_var (compile_match repr records)
                            (compile_match repr others)
            | Tpat_or(pat1, pat2) ->
                (* Avoid duplicating the code of the action *)
                let (or_match, remainder_line, others) = divide_orpat pm in
                combine_orpat (compile_match None or_match)
                              (compile_match repr remainder_line)
                              (compile_match repr others)
            | _ ->
                fatal_error "Matching.compile_match1"
            end
        | _ -> fatal_error "Matching.compile_match2" in
      (Llet(str, v, arg, lam), total)
  | _ -> assert false

(* The entry points *)

let compile_matching repr handler_fun arg pat_act_list =
  let pm =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [arg, Strict] } in
  let (lambda, total) = compile_match repr pm in
  if total then lambda else Lcatch(lambda, handler_fun())

let partial_function loc () =
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_match_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string !Location.input_name);
               Const_base(Const_int loc.loc_start);
               Const_base(Const_int loc.loc_end)]))])])

let for_function loc repr param pat_act_list =
  compile_matching repr (partial_function loc) param pat_act_list

let for_trywith param pat_act_list =
  compile_matching None (fun () -> Lprim(Praise, [param])) param pat_act_list

let for_let loc param pat body =
  compile_matching None (partial_function loc) param [pat, body]

(* Handling of tupled functions and matches *)

exception Cannot_flatten

let flatten_pattern size p =
  match p.pat_desc with
    Tpat_tuple args -> args
  | Tpat_any -> replicate_list any_pat size
  | _ -> raise Cannot_flatten

let flatten_cases size cases =
  List.map (function (pat :: _, act) -> (flatten_pattern size pat, act)
                   | _ -> assert false)
           cases

let for_tupled_function loc paraml pats_act_list =
  let pm =
    { cases = pats_act_list;
      args = List.map (fun id -> (Lvar id, Strict)) paraml } in
  let (lambda, total) = compile_match None pm in
  if total then lambda else Lcatch(lambda, partial_function loc ())

let for_multiple_match loc paraml pat_act_list =
  let pm1 =
    { cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
      args = [Lprim(Pmakeblock(0, Immutable), paraml), Strict] } in
  let pm2 =
    simplify_matching pm1 in
  let pm3 =
    try
      { cases = flatten_cases (List.length paraml) pm2.cases;
        args = List.map (fun lam -> (lam, Strict)) paraml }
    with Cannot_flatten ->
      pm2 in
  let (lambda, total) = compile_match None pm3 in
  if total then lambda else Lcatch(lambda, partial_function loc ())
