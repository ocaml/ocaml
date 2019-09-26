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

(* Compilation of pattern matching

   Based upon Lefessant-Maranget ``Optimizing Pattern-Matching'' ICFP'2001.

   A previous version was based on Peyton-Jones, ``The Implementation of
   functional programming languages'', chapter 5.


   Overview of the implementation
   ==============================

       1. Precompilation
       -----------------

     (split_and_precompile)
   We first split the initial pattern matching (or "pm") along its first column
   -- simplifying pattern heads in the process --, so that we obtain an ordered
   list of pms.
   For every pm in this list, and any two patterns in its first column, either
   the patterns have the same head, or their heads match disjoint sets of
   values. (In particular, two extension constructors that may or may not be
   equal due to hidden rebinding cannot occur in the same simple pm.)

       2. Compilation
       --------------

   The compilation of one of these pms obtained after precompiling is done as
   follows:

     (divide)
   We split the match along the first column again, this time grouping rows
   which start with the same head, and removing the first column.
   As a result we get a "division", which is a list a "cells" of the form:
         discriminating pattern head * specialized pm

     (compile_list + compile_match)
   We then map over the division to compile each cell: we simply restart the
   whole process on the second element of each cell.
   Each cell is now of the form:
         discriminating pattern head * lambda

     (combine_constant, combine_construct, combine_array, ...)
   We recombine the cells using a switch or some ifs, and if the matching can
   fail, introduce a jump to the next pm that could potentially match the
   scrutiny.

       3. Chaining of pms
       ------------------

     (comp_match_handlers)
   Once the pms have been compiled, we stitch them back together in the order
   produced by precompilation, resulting in the following structure:
   {v
       catch
         catch
           <first body>
         with <exit i> ->
           <second body>
       with <exit j> ->
         <third body>
   v}

   Additionally, bodies whose corresponding exit-number is never used are
   discarded. So for instance, if in the pseudo-example above we know that exit
   [i] is never taken, we would actually generate:
   {v
       catch
         <first body>
       with <exit j> ->
         <third body>
   v}

*)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Parmatch
open Printf
open Printpat

let dbg = false

(*
   Compatibility predicate that considers potential rebindings of constructors
   of an extension type.

   "may_compat p q" returns false when p and q never admit a common instance;
   returns true when they may have a common instance.
*)

module MayCompat = Parmatch.Compat (struct
  let equal = Types.may_equal_constr
end)

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
  Printlambda.lambda Format.str_formatter lam;
  Format.flush_str_formatter ()

let all_record_args lbls =
  match lbls with
  | (_, { lbl_all }, _) :: _ ->
      let t =
        Array.map
          (fun lbl -> (mknoloc (Longident.Lident "?temp?"), lbl, omega))
          lbl_all
      in
      List.iter (fun ((_, lbl, _) as x) -> t.(lbl.lbl_pos) <- x) lbls;
      Array.to_list t
  | _ -> fatal_error "Matching.all_record_args"

type matrix = pattern list list

let add_omega_column pss = List.map (fun ps -> omega :: ps) pss

let rec rev_split_at n ps =
  if n <= 0 then
    ([], ps)
  else
    match ps with
    | p :: rem ->
        let left, right = rev_split_at (n - 1) rem in
        (p :: left, right)
    | _ -> assert false

exception NoMatch

let ncols = function
  | [] -> 0
  | ps :: _ -> List.length ps

module Context : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val start : int -> t

  val eprintf : t -> unit

  val specialize : pattern -> t -> t

  val lshift : t -> t

  val rshift : t -> t

  val rshift_num : int -> t -> t

  val lub : pattern -> t -> t

  val matches : t -> matrix -> bool

  val combine : t -> t

  val select_columns : matrix -> t -> t

  val union : t -> t -> t
end = struct
  module Row = struct
    type t = { left : pattern list; right : pattern list }

    let eprintf { left; right } =
      Format.eprintf "LEFT:%a RIGHT:%a\n" pretty_line left pretty_line right

    let le c1 c2 = le_pats c1.left c2.left && le_pats c1.right c2.right

    let lshift { left; right } =
      match right with
      | x :: xs -> { left = x :: left; right = xs }
      | _ -> assert false

    let lforget { left; right } =
      match right with
      | _ :: xs -> { left = omega :: left; right = xs }
      | _ -> assert false

    let rshift { left; right } =
      match left with
      | p :: ps -> { left = ps; right = p :: right }
      | _ -> assert false

    let rshift_num n { left; right } =
      let shifted, left = rev_split_at n left in
      { left; right = shifted @ right }

    (** Recombination of contexts (eg: (_,_)::p1::p2::rem ->  (p1,p2)::rem)
  All mutable fields are replaced by '_', since side-effects in
  guards can alter these fields *)
    let combine { left; right } =
      match left with
      | p :: ps -> { left = ps; right = set_args_erase_mutable p right }
      | _ -> assert false
  end

  type t = Row.t list

  let empty = []

  let start n : t = [ { left = []; right = omegas n } ]

  let is_empty = function
    | [] -> true
    | _ -> false

  let eprintf ctx = List.iter Row.eprintf ctx

  let lshift ctx =
    if List.length ctx < !Clflags.match_context_rows then
      List.map Row.lshift ctx
    else
      (* Context pruning *)
      get_mins Row.le (List.map Row.lforget ctx)

  let rshift ctx = List.map Row.rshift ctx

  let rshift_num n ctx = List.map (Row.rshift_num n) ctx

  let combine ctx = List.map Row.combine ctx

  let ctx_matcher p =
    let p = normalize_pat p in
    match p.pat_desc with
    | Tpat_construct (_, cstr, omegas) -> (
        fun q rem ->
          match q.pat_desc with
          | Tpat_construct (_, cstr', args)
          (* NB: may_constr_equal considers (potential) constructor rebinding *)
            when Types.may_equal_constr cstr cstr' ->
              (p, args @ rem)
          | Tpat_any -> (p, omegas @ rem)
          | _ -> raise NoMatch
      )
    | Tpat_constant cst -> (
        fun q rem ->
          match q.pat_desc with
          | Tpat_constant cst' when const_compare cst cst' = 0 -> (p, rem)
          | Tpat_any -> (p, rem)
          | _ -> raise NoMatch
      )
    | Tpat_variant (lab, Some omega, _) -> (
        fun q rem ->
          match q.pat_desc with
          | Tpat_variant (lab', Some arg, _) when lab = lab' -> (p, arg :: rem)
          | Tpat_any -> (p, omega :: rem)
          | _ -> raise NoMatch
      )
    | Tpat_variant (lab, None, _) -> (
        fun q rem ->
          match q.pat_desc with
          | Tpat_variant (lab', None, _) when lab = lab' -> (p, rem)
          | Tpat_any -> (p, rem)
          | _ -> raise NoMatch
      )
    | Tpat_array omegas -> (
        let len = List.length omegas in
        fun q rem ->
          match q.pat_desc with
          | Tpat_array args when List.length args = len -> (p, args @ rem)
          | Tpat_any -> (p, omegas @ rem)
          | _ -> raise NoMatch
      )
    | Tpat_tuple omegas -> (
        let len = List.length omegas in
        fun q rem ->
          match q.pat_desc with
          | Tpat_tuple args when List.length args = len -> (p, args @ rem)
          | Tpat_any -> (p, omegas @ rem)
          | _ -> raise NoMatch
      )
    | Tpat_record (((_, lbl, _) :: _ as l), _) -> (
        (* Records are normalized *)
        let len = Array.length lbl.lbl_all in
        fun q rem ->
          match q.pat_desc with
          | Tpat_record (((_, lbl', _) :: _ as l'), _)
            when Array.length lbl'.lbl_all = len ->
              let l' = all_record_args l' in
              (p, List.fold_right (fun (_, _, p) r -> p :: r) l' rem)
          | Tpat_any -> (p, List.fold_right (fun (_, _, p) r -> p :: r) l rem)
          | _ -> raise NoMatch
      )
    | Tpat_lazy omega -> (
        fun q rem ->
          match q.pat_desc with
          | Tpat_lazy arg -> (p, arg :: rem)
          | Tpat_any -> (p, omega :: rem)
          | _ -> raise NoMatch
      )
    | _ -> fatal_error "Matching.Context.matcher"

  let specialize q ctx =
    let matcher = ctx_matcher q in
    let rec filter_rec : t -> t = function
      | ({ right = p :: ps } as l) :: rem -> (
          match p.pat_desc with
          | Tpat_or (p1, p2, _) ->
              filter_rec
                ({ l with right = p1 :: ps }
                :: { l with
                     Row.right (* disam not principal, OK *) = p2 :: ps
                   }
                :: rem
                )
          | Tpat_alias (p, _, _) ->
              filter_rec ({ l with right = p :: ps } :: rem)
          | Tpat_var _ -> filter_rec ({ l with right = omega :: ps } :: rem)
          | _ -> (
              let rem = filter_rec rem in
              try
                let to_left, right = matcher p ps in
                { left = to_left :: l.left; right } :: rem
              with NoMatch -> rem
            )
        )
      | [] -> []
      | _ -> fatal_error "Matching.Context.specialize"
    in
    filter_rec ctx

  let select_columns pss ctx =
    let n = ncols pss in
    let lub_row ps { Row.left; right } =
      let transfer, right = rev_split_at n right in
      match lubs transfer ps with
      | exception Empty -> None
      | inter -> Some { Row.left = inter @ left; right }
    in
    let lub_with_ctx ps = List.filter_map (lub_row ps) ctx in
    List.flatten (List.map lub_with_ctx pss)

  let lub p ctx =
    List.filter_map
      (fun { Row.left; right } ->
        match right with
        | q :: rem -> (
            try Some { Row.left; right = lub p q :: rem } with Empty -> None
          )
        | _ -> fatal_error "Matching.Context.lub")
      ctx

  let matches ctx pss =
    List.exists
      (fun { Row.right = qs } -> List.exists (fun ps -> may_compats qs ps) pss)
      ctx

  let union pss qss = get_mins Row.le (pss @ qss)
end

exception OrPat

let rec flatten_pat_line size p k =
  match p.pat_desc with
  | Tpat_any -> omegas size :: k
  | Tpat_tuple args -> args :: k
  | Tpat_or (p1, p2, _) ->
      flatten_pat_line size p1 (flatten_pat_line size p2 k)
  | Tpat_alias (p, _, _) ->
      (* Note: if this 'as' pat is here, then this is a
                           useless binding, solves PR#3780 *)
      flatten_pat_line size p k
  | _ -> fatal_error "Matching.flatten_pat_line"

let flatten_matrix size pss =
  List.fold_right
    (fun ps r ->
      match ps with
      | [ p ] -> flatten_pat_line size p r
      | _ -> fatal_error "Matching.flatten_matrix")
    pss []

(** A default environment (referred to as "reachable trap handlers" in the
    paper), is an ordered list of [matrix * raise_num] pairs, and is used to
    decide where to jump next if none of the rows in a given matrix match the
    input.

    In such situations, one thing you can do is to jump to the first (leftmost)
    [raise_num] in that list (by doing a raise to the static-cach handler number
    [raise_num]); and you can assume that if the associated pm doesn't match
    either, it will do the same thing, etc.
    This is what [mk_failaction_neg] (and its callers) does.

    A more sophisticated alternative is to use what you know about the input
    (what you might already have matched) and the current pm (what you know you
    can't match) to directly jump to a pm that might match it instead of the
    next one; that is why we don't just keep [raise_num]s but also the
    associated matrices.
    [mk_failaction_pos] does (a slightly more sophisticated version of) this.
*)
module Default_environment : sig
  type t

  val is_empty : t -> bool

  val pop : t -> ((matrix * int) * t) option

  val empty : t

  val cons : matrix -> int -> t -> t

  val specialize : (pattern -> pattern list -> pattern list) -> t -> t

  val pop_column : t -> t

  val pop_compat : pattern -> t -> t

  val flatten : int -> t -> t

  val pp : t -> unit
end = struct
  type t = (matrix * int) list
  (** All matrices in the list should have the same arity -- their rows should
      have the same number of columns -- as it should match the arity of the
      current scrutiny vector. *)

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let cons matrix raise_num default =
    match matrix with
    | [] -> default
    | _ -> (matrix, raise_num) :: default

  let specialize_matrix matcher pss =
    let rec filter_rec = function
      | (p :: ps) :: rem -> (
          match p.pat_desc with
          | Tpat_alias (p, _, _) -> filter_rec ((p :: ps) :: rem)
          | Tpat_var _ -> filter_rec ((omega :: ps) :: rem)
          | _ -> (
              let rem = filter_rec rem in
              try matcher p ps :: rem with
              | NoMatch -> rem
              | OrPat -> (
                  match p.pat_desc with
                  | Tpat_or (p1, p2, _) ->
                      filter_rec [ p1 :: ps; p2 :: ps ] @ rem
                  | _ -> assert false
                )
            )
        )
      | [] -> []
      | _ ->
          pretty_matrix Format.err_formatter pss;
          fatal_error "Matching.Default_environment.specialize_matrix"
    in
    filter_rec pss

  let specialize matcher env =
    let rec make_rec = function
      | [] -> []
      | ([ [] ], i) :: _ -> [ ([ [] ], i) ]
      | (pss, i) :: rem -> (
          let rem = make_rec rem in
          match specialize_matrix matcher pss with
          | [] -> rem
          | [] :: _ -> [ ([ [] ], i) ]
          | pss -> (pss, i) :: rem
        )
    in
    make_rec env

  let pop_column def = specialize (fun _p rem -> rem) def

  let pop_compat p def =
    let compat_matcher q rem =
      if may_compat p q then
        rem
      else
        raise NoMatch
    in
    specialize compat_matcher def

  let pop = function
    | [] -> None
    | def :: defs -> Some (def, defs)

  let pp def =
    Format.eprintf "+++++ Defaults +++++\n";
    List.iter
      (fun (pss, i) -> Format.eprintf "Matrix for %d\n%a" i pretty_matrix pss)
      def;
    Format.eprintf "+++++++++++++++++++++\n"

  let flatten size def =
    List.map (fun (pss, i) -> (flatten_matrix size pss, i)) def
end

module Jumps : sig
  type t

  val is_empty : t -> bool

  val empty : t

  val singleton : int -> Context.t -> t

  val add : int -> Context.t -> t -> t

  val union : t -> t -> t

  val unions : t list -> t

  val map : (Context.t -> Context.t) -> t -> t

  val remove : int -> t -> t

  val extract : int -> t -> Context.t * t

  val eprintf : t -> unit
end = struct
  type t = (int * Context.t) list

  let eprintf (env : t) =
    List.iter
      (fun (i, ctx) ->
        Printf.eprintf "jump for %d\n" i;
        Context.eprintf ctx)
      env

  let rec extract i = function
    | [] -> (Context.empty, [])
    | ((j, pss) as x) :: rem as all ->
        if i = j then
          (pss, rem)
        else if j < i then
          (Context.empty, all)
        else
          let r, rem = extract i rem in
          (r, x :: rem)

  let rec remove i = function
    | [] -> []
    | (j, _) :: rem when i = j -> rem
    | x :: rem -> x :: remove i rem

  let empty = []

  and is_empty = function
    | [] -> true
    | _ -> false

  let singleton i ctx =
    if Context.is_empty ctx then
      []
    else
      [ (i, ctx) ]

  let add i ctx jumps =
    let rec add = function
      | [] -> [ (i, ctx) ]
      | ((j, qss) as x) :: rem as all ->
          if j > i then
            x :: add rem
          else if j < i then
            (i, ctx) :: all
          else
            (i, Context.union ctx qss) :: rem
    in
    if Context.is_empty ctx then
      jumps
    else
      add jumps

  let rec union (env1 : t) env2 =
    match (env1, env2) with
    | [], _ -> env2
    | _, [] -> env1
    | ((i1, pss1) as x1) :: rem1, ((i2, pss2) as x2) :: rem2 ->
        if i1 = i2 then
          (i1, Context.union pss1 pss2) :: union rem1 rem2
        else if i1 > i2 then
          x1 :: union rem1 env2
        else
          x2 :: union env1 rem2

  let rec merge = function
    | env1 :: env2 :: rem -> union env1 env2 :: merge rem
    | envs -> envs

  let rec unions envs =
    match envs with
    | [] -> []
    | [ env ] -> env
    | _ -> unions (merge envs)

  let map f env = List.map (fun (i, pss) -> (i, f pss)) env
end

(* Pattern matching before any compilation *)

type pattern_matching = {
  mutable cases : (pattern list * lambda) list;
  args : (lambda * let_kind) list;
      (** args are not just Ident.t in at least the following cases:
        - when matching the arguments of a constructor,
          direct field projections are used (make_field_args)
        - with lazy patterns args can be of the form [Lazy.force ...]
          (inline_lazy_force). *)
  default : Default_environment.t
}

type handler = {
  provenance : matrix;
  exit : int;
  vars : (Ident.t * Lambda.value_kind) list;
  pm : pattern_matching
}

type pm_or_compiled = {
  body : pattern_matching;
  handlers : handler list;
  or_matrix : matrix
}

(* Pattern matching after application of both the or-pat rule and the
   mixture rule *)

type pm_half_compiled =
  | PmOr of pm_or_compiled
  | PmVar of { inside : pm_half_compiled }
  | Pm of pattern_matching

(* Only used inside the various split functions, we only keep [me] when we're
   done splitting / precompiling. *)
type pm_half_compiled_info = {
  me : pm_half_compiled;
  matrix : matrix;
  (* the matrix matched by [me]. Is used to extend the list of reachable trap
        handlers (aka "default environments") when returning from recursive
        calls. *)
  top_default : Default_environment.t
}

let pretty_cases cases =
  List.iter
    (fun (ps, _l) ->
      List.iter (fun p -> Format.eprintf " %a%!" top_pretty p) ps;
      Format.eprintf "\n")
    cases

let pretty_pm pm =
  pretty_cases pm.cases;
  if not (Default_environment.is_empty pm.default) then
    Default_environment.pp pm.default

let rec pretty_precompiled = function
  | Pm pm ->
      Format.eprintf "++++ PM ++++\n";
      pretty_pm pm
  | PmVar x ->
      Format.eprintf "++++ VAR ++++\n";
      pretty_precompiled x.inside
  | PmOr x ->
      Format.eprintf "++++ OR ++++\n";
      pretty_pm x.body;
      pretty_matrix Format.err_formatter x.or_matrix;
      List.iter
        (fun { exit = i; pm; _ } ->
          eprintf "++ Handler %d ++\n" i;
          pretty_pm pm)
        x.handlers

let pretty_precompiled_res first nexts =
  pretty_precompiled first;
  List.iter
    (fun (e, pmh) ->
      eprintf "** DEFAULT %d **\n" e;
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

module StoreExp = Switch.Store (struct
  type t = lambda

  type key = lambda

  let compare_key = Stdlib.compare

  let make_key = Lambda.make_key
end)

let make_exit i = Lstaticraise (i, [])

(* Introduce a catch, if worth it *)
let make_catch d k =
  match d with
  | Lstaticraise (_, []) -> k d
  | _ ->
      let e = next_raise_count () in
      Lstaticcatch (k (make_exit e), (e, []), d)

(* Introduce a catch, if worth it, delayed version *)
let rec as_simple_exit = function
  | Lstaticraise (i, []) -> Some i
  | Llet (Alias, _k, _, _, e) -> as_simple_exit e
  | _ -> None

let make_catch_delayed handler =
  match as_simple_exit handler with
  | Some i -> (i, fun act -> act)
  | None -> (
      let i = next_raise_count () in
      (*
    Printf.eprintf "SHARE LAMBDA: %i\n%s\n" i (string_of_lam handler);
*)
      ( i,
        fun body ->
          match body with
          | Lstaticraise (j, _) ->
              if i = j then
                handler
              else
                body
          | _ -> Lstaticcatch (body, (i, []), handler) )
    )

let raw_action l =
  match make_key l with
  | Some l -> l
  | None -> l

let same_actions = function
  | [] -> None
  | [ (_, act) ] -> Some act
  | (_, act0) :: rem -> (
      match make_key act0 with
      | None -> None
      | key0_opt ->
          let same_act (_, act) = make_key act = key0_opt in
          if List.for_all same_act rem then
            Some act0
          else
            None
    )

let safe_before (ps, act_p) l =
  (* Test for swapping two clauses *)
  let same_actions act1 act2 =
    match (make_key act1, make_key act2) with
    | Some key1, Some key2 -> key1 = key2
    | None, _
    | _, None ->
        false
  in
  List.for_all
    (fun (qs, act_q) -> same_actions act_p act_q || not (may_compats ps qs))
    l

(*
   The half-simplify functions transforms the first column of the match
     - records are expanded so that they possess all fields
     - aliases are removed and replaced by bindings in actions.

   However or-patterns are only half-simplified,
     - aliases under or-patterns are kept
     - or-patterns whose right-hand-side is subsumed by their lhs
       are simplified to their lhs.
       For instance: [(_ :: _ | 1 :: _)] is changed into [_ :: _]
     - or-patterns whose left-hand-side is not simplified
       are preserved: (p|q) is changed into (simpl(p)|simpl(q))
         {v
             # match lazy (print_int 3; 3) with _ | lazy 2 -> ();;
             - : unit = ()
             # match lazy (print_int 3; 3) with lazy 2 | _ -> ();;
             3- : unit = ()
         v}

   In particular, or-patterns may still occur in the head of the output row,
   so this is only a "half-simplification".
*)
let half_simplify_cases args cls =
  let rec simpl_pat p =
    match p.pat_desc with
    | Tpat_any
    | Tpat_var _ ->
        p
    | Tpat_alias (q, id, s) ->
        { p with pat_desc = Tpat_alias (simpl_pat q, id, s) }
    | Tpat_or (p1, p2, o) ->
        let p1, p2 = (simpl_pat p1, simpl_pat p2) in
        if le_pat p1 p2 then
          p1
        else
          { p with pat_desc = Tpat_or (p1, p2, o) }
    | Tpat_record (lbls, closed) ->
        let all_lbls = all_record_args lbls in
        { p with pat_desc = Tpat_record (all_lbls, closed) }
    | _ -> p
  in
  let rec simpl_clause cl =
    match cl with
    | [], _ -> assert false
    | pat :: patl, action -> (
        match pat.pat_desc with
        | Tpat_any -> cl
        | Tpat_var (id, s) ->
            let p = { pat with pat_desc = Tpat_alias (omega, id, s) } in
            simpl_clause (p :: patl, action)
        | Tpat_alias (p, id, _) ->
            let arg =
              match args with
              | [] -> assert false
              | (arg, _) :: _ -> arg
            in
            let k = Typeopt.value_kind pat.pat_env pat.pat_type in
            simpl_clause
              (p :: patl, bind_with_value_kind Alias (id, k) arg action)
        | Tpat_record ([], _) -> (omega :: patl, action)
        | Tpat_record (lbls, closed) ->
            let all_lbls = all_record_args lbls in
            let full_pat =
              { pat with pat_desc = Tpat_record (all_lbls, closed) }
            in
            (full_pat :: patl, action)
        | Tpat_or _ -> (
            let pat_simple = simpl_pat pat in
            match pat_simple.pat_desc with
            | Tpat_or _ -> (pat_simple :: patl, action)
            | _ -> simpl_clause (pat_simple :: patl, action)
          )
        | Tpat_constant _
        | Tpat_tuple _
        | Tpat_construct _
        | Tpat_variant _
        | Tpat_array _
        | Tpat_lazy _
        | Tpat_exception _ ->
            cl
      )
  in
  List.map simpl_clause cls

(* Once matchings are *fully* simplified, one can easily find
   their nature. *)

let rec what_is_cases ~skip_any cases =
  match cases with
  | [] -> omega
  | ([], _) :: _ -> assert false
  | (p :: _, _) :: rem -> (
      match p.pat_desc with
      | Tpat_any when skip_any -> what_is_cases ~skip_any rem
      | Tpat_var _
      | Tpat_or (_, _, _)
      | Tpat_alias (_, _, _) ->
          (* applies to simplified matchings only *)
          assert false
      | _ -> p
    )

let what_is_first_case = what_is_cases ~skip_any:false

let what_is_cases = what_is_cases ~skip_any:true

(* Or-pattern expansion, variables are a complication w.r.t. the article *)

exception Cannot_flatten

let mk_alpha_env arg aliases ids =
  List.map
    (fun id ->
      ( id,
        if List.mem id aliases then
          match arg with
          | Some v -> v
          | _ -> raise Cannot_flatten
        else
          Ident.create_local (Ident.name id) ))
    ids

let rec explode_or_pat p arg patl mk_action vars aliases rem =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) ->
      explode_or_pat p1 arg patl mk_action vars aliases
        (explode_or_pat p2 arg patl mk_action vars aliases rem)
  | Tpat_alias (p, id, _) ->
      explode_or_pat p arg patl mk_action vars (id :: aliases) rem
  | Tpat_var (x, _) ->
      let env = mk_alpha_env arg (x :: aliases) vars in
      (omega :: patl, mk_action (List.map snd env)) :: rem
  | _ ->
      let env = mk_alpha_env arg aliases vars in
      (alpha_pat env p :: patl, mk_action (List.map snd env)) :: rem

let pm_free_variables { cases } =
  List.fold_right
    (fun (_, act) r -> Ident.Set.union (free_variables act) r)
    cases Ident.Set.empty

(* Basic grouping predicates *)
let pat_as_constr = function
  | { pat_desc = Tpat_construct (_, cstr, _) } -> cstr
  | _ -> fatal_error "Matching.pat_as_constr"

let group_const_int = function
  | { pat_desc = Tpat_constant (Const_int _) } -> true
  | _ -> false

let group_const_char = function
  | { pat_desc = Tpat_constant (Const_char _) } -> true
  | _ -> false

let group_const_string = function
  | { pat_desc = Tpat_constant (Const_string _) } -> true
  | _ -> false

let group_const_float = function
  | { pat_desc = Tpat_constant (Const_float _) } -> true
  | _ -> false

let group_const_int32 = function
  | { pat_desc = Tpat_constant (Const_int32 _) } -> true
  | _ -> false

let group_const_int64 = function
  | { pat_desc = Tpat_constant (Const_int64 _) } -> true
  | _ -> false

let group_const_nativeint = function
  | { pat_desc = Tpat_constant (Const_nativeint _) } -> true
  | _ -> false

and group_constructor = function
  | { pat_desc = Tpat_construct (_, _, _) } -> true
  | _ -> false

and group_same_constructor tag = function
  | { pat_desc = Tpat_construct (_, cstr, _) } ->
      Types.equal_tag tag cstr.cstr_tag
  | _ -> false

and group_variant = function
  | { pat_desc = Tpat_variant (_, _, _) } -> true
  | _ -> false

and group_var = function
  | { pat_desc = Tpat_any } -> true
  | _ -> false

and group_tuple = function
  | { pat_desc = Tpat_tuple _ | Tpat_any } -> true
  | _ -> false

and group_record = function
  | { pat_desc = Tpat_record _ | Tpat_any } -> true
  | _ -> false

and group_array = function
  | { pat_desc = Tpat_array _ } -> true
  | _ -> false

and group_lazy = function
  | { pat_desc = Tpat_lazy _ } -> true
  | _ -> false

let can_group p =
  match p.pat_desc with
  | Tpat_any -> group_var
  | Tpat_constant (Const_int _) -> group_const_int
  | Tpat_constant (Const_char _) -> group_const_char
  | Tpat_constant (Const_string _) -> group_const_string
  | Tpat_constant (Const_float _) -> group_const_float
  | Tpat_constant (Const_int32 _) -> group_const_int32
  | Tpat_constant (Const_int64 _) -> group_const_int64
  | Tpat_constant (Const_nativeint _) -> group_const_nativeint
  | Tpat_construct (_, { cstr_tag = Cstr_extension _ as t }, _) ->
      (* Extension constructors with distinct names may be equal thanks to
         constructor rebinding. So we need to produce a specialized
         submatrix for each syntactically-distinct constructor (with a threading
         of exits such that each submatrix falls back to the
         potentially-compatible submatrices below it).  *)
      group_same_constructor t
  | Tpat_construct _ -> group_constructor
  | Tpat_tuple _ -> group_tuple
  | Tpat_record _ -> group_record
  | Tpat_array _ -> group_array
  | Tpat_variant (_, _, _) -> group_variant
  | Tpat_lazy _ -> group_lazy
  | _ -> fatal_error "Matching.can_group"

let is_or p =
  match p.pat_desc with
  | Tpat_or _ -> true
  | _ -> false

let rec omega_like p =
  match p.pat_desc with
  | Tpat_any
  | Tpat_var _ ->
      true
  | Tpat_alias (p, _, _) -> omega_like p
  | Tpat_or (p1, p2, _) -> omega_like p1 || omega_like p2
  | _ -> false

let equiv_pat p q = le_pat p q && le_pat q p

let rec extract_equiv_head p l =
  match l with
  | ((q :: _, _) as cl) :: rem ->
      if equiv_pat p q then
        let others, rem = extract_equiv_head p rem in
        (cl :: others, rem)
      else
        ([], l)
  | _ -> ([], l)

module Or_matrix = struct
  (* Splitting a matrix uses an or-matrix that contains or-patterns (at
     the head of some of its rows).

     The property that we want to maintain for the rows of the
     or-matrix is that if the row p::ps is before q::qs and p is an
     or-pattern, and v::vs matches p but not ps, then we don't need to
     try q::qs. This is necessary because the compilation of the
     or-pattern p will exit to a sub-matrix and never come back.

     For this to hold, (p::ps) and (q::qs) must satisfy one of:
     - disjointness: p and q are not compatible
     - ordering: if p and q are compatible, ps is more general than qs
       (this only works if the row p::ps is not guarded; otherwise the
        guard could fail and q::qs should still be tried)
  *)

  (* Conditions for appending to the Or matrix *)
  let disjoint p q = not (may_compat p q)

  let safe_below (ps, act) qs =
    (not (is_guarded act)) && Parmatch.le_pats ps qs

  let safe_below_or_matrix l (q, qs) =
    List.for_all
      (function
        | ({ pat_desc = Tpat_or _ } as p) :: ps, act_p ->
            disjoint p q || safe_below (ps, act_p) qs
        | _ -> true)
      l

  (* Insert or append a clause in the Or matrix:
     - insert: adding the clause in the middle of the or_matrix
     - append: adding the clause at the bottom of the or_matrix

     If neither are possible we add to the bottom of the No matrix.
   *)
  let insert_or_append (p, ps, act) rev_ors rev_no =
    let safe_to_insert rem (p, ps) seen =
      let _, not_e = extract_equiv_head p rem in
      (* check append condition for head of O *)
      safe_below_or_matrix not_e (p, ps)
      && (* check insert condition for tail of O *)
         List.for_all
           (fun cl ->
             match cl with
             | q :: _, _ -> disjoint p q
             | _ -> assert false)
           seen
    in
    let rec attempt seen = function
      (* invariant: the new clause is safe to append at the end of
         [seen] (but maybe not [rem] yet) *)
      | [] -> ((p :: ps, act) :: rev_ors, rev_no)
      | ([], _act) :: _ -> assert false
      | ((q :: qs, act_q) as cl) :: rem ->
          if (not (is_or q)) || disjoint p q then
            attempt (cl :: seen) rem
          else if
            Typedtree.pat_bound_idents p = []
            && Typedtree.pat_bound_idents q = []
            && equiv_pat p q
          then
            (* attempt insertion, for equivalent orpats with no variables *)
            if safe_to_insert rem (p, ps) seen then
              (List.rev_append seen ((p :: ps, act) :: cl :: rem), rev_no)
            else
              (* fail to insert or append *)
              (rev_ors, (p :: ps, act) :: rev_no)
          else if safe_below (qs, act_q) ps then
            attempt (cl :: seen) rem
          else
            (rev_ors, (p :: ps, act) :: rev_no)
    in
    attempt [] rev_ors
end

(* Reconstruct default information from half_compiled  pm list *)

let as_matrix cases = get_mins le_pats (List.map (fun (ps, _) -> ps) cases)

(*
  Split a matching along the first column.

    Splitting is first directed by or-patterns, then by
    tests (e.g. constructors)/variable transitions.

    The approach is greedy, every split function attempts to
    raise rows as much as possible in the top matrix,
    then splitting applies again to the remaining rows.

    Some precompilation of or-patterns and
    variable pattern occurs. Mostly this means that bindings
    are performed now,  being replaced by let-bindings
    in actions (cf. half_simplify_cases).

    Additionally, if the match argument is a variable, matchings whose
    first column is made of variables only are split further
    (cf. precompile_var).

  ---

  Note: we assume that the first column of each pattern is coherent -- all
  patterns match values of the same type. This comes from the fact that
  we make agressive splitting decisions, splitting pattern heads that
  may be different into different submatrices; in particular, in a given
  submatrix the first column is formed of first arguments to the same
  constructor.

  GADTs are not an issue because we split columns left-to-right, and
  GADT typing also introduces typing equations left-to-right. In
  particular, a leftmost column in matching.ml will be well-typed under
  a set of equations accepted by the type-checker, and those equations
  are forced to remain consistent: they can equate known types to
  abstract types, but they cannot equate two incompatible known types
  together, and in particular incompatible pattern heads do not appear
  in a leftmost column.

  Parmatch has to be more conservative because it splits less
  agressively: submatrices will contain not just the arguments of
  a given pattern head, but also other lines that may be compatible with
  it, in particular those with a leftmost omega and those starting with
  an extension constructor that may be equal to it.

*)

let rec split_or argo cls args def =
  let cls = half_simplify_cases args cls in
  let rec do_split rev_before rev_ors rev_no = function
    | [] ->
        cons_next (List.rev rev_before) (List.rev rev_ors) (List.rev rev_no)
    | ((p :: ps, act) as cl) :: rem ->
        if not (safe_before cl rev_no) then
          do_split rev_before rev_ors (cl :: rev_no) rem
        else if (not (is_or p)) && safe_before cl rev_ors then
          do_split (cl :: rev_before) rev_ors rev_no rem
        else
          let rev_ors, rev_no =
            Or_matrix.insert_or_append (p, ps, act) rev_ors rev_no
          in
          do_split rev_before rev_ors rev_no rem
    | _ -> assert false
  and cons_next yes yesor no =
    let def, nexts =
      match no with
      | [] -> (def, [])
      | _ ->
          let { me = next; matrix; top_default = def }, nexts =
            do_split [] [] [] no
          in
          let idef = next_raise_count () in
          (Default_environment.cons matrix idef def, (idef, next) :: nexts)
    in
    match yesor with
    | [] -> split_no_or yes args def nexts
    | _ -> precompile_or argo yes yesor args def nexts
  in
  do_split [] [] [] cls

and split_no_or cls args def k =
  (* We split the remaining clauses in as few pms as possible while maintaining
     the property stated earlier (cf. {1. Precompilation}), i.e. for
     any pm in the result, it is possible to decide for any two patterns
     on the first column whether their heads are equal or not.

     This generally means that we'll have two kinds of pms: ones where the first
     column is made of variables only, and ones where the head is actually a
     discriminating pattern.

     There is some subtlety regarding the handling of extension constructors
     (where it is not always possible to syntactically decide whether two
     different heads match different values), but this is handled by the
     [can_group] function. *)
  let rec split cls =
    let discr = what_is_first_case cls in
    collect discr [] [] cls
  and collect group_discr rev_yes rev_no = function
    | ([], _) :: _ -> assert false
    | [ ((ps, _) as cl) ] when rev_yes <> [] && List.for_all omega_like ps ->
        (* This enables an extra division in some frequent cases:
               last row is made of variables only

           Splitting a matrix there creates two default environments (instead of
           one for the non-split matrix), the first of which often gets
           specialized away by further refinement, and the second one jumping
           directly to the catch-all case -- this produces better code.

           This optimisation is tested in the first part of
           testsuite/tests/basic/patmatch_split_no_or.ml *)
        collect group_discr rev_yes (cl :: rev_no) []
    | ((p :: _, _) as cl) :: rem ->
        if can_group group_discr p && safe_before cl rev_no then
          collect group_discr (cl :: rev_yes) rev_no rem
        else if should_split group_discr then (
          assert (rev_no = []);
          let yes = List.rev rev_yes in
          insert_split group_discr yes (cl :: rem) def k
        ) else
          collect group_discr rev_yes (cl :: rev_no) rem
    | [] ->
        let yes = List.rev rev_yes and no = List.rev rev_no in
        insert_split group_discr yes no def k
  and insert_split group_discr yes no def k =
    let precompile_group =
      if group_var group_discr then
        precompile_var
      else
        do_not_precompile
    in
    match no with
    | [] -> precompile_group args yes def k
    | _ ->
        let { me = next; matrix; top_default = def }, nexts = split no in
        let idef = next_raise_count () in
        precompile_group args yes
          (Default_environment.cons matrix idef def)
          ((idef, next) :: nexts)
  and should_split group_discr =
    match group_discr.pat_desc with
    | Tpat_construct (_, { cstr_tag = Cstr_extension _ }, _) ->
        (* it is unlikely that we will raise anything, so we split now *)
        true
    | _ -> false
  in
  split cls

and precompile_var args cls def k =
  (* Strategy: pop the first column,
     precompile the rest, add a PmVar to all precompiled submatrices.

     If the rest doesn't generate any split, abort and do_not_precompile. *)
  match args with
  | [] -> assert false
  | _ :: ((Lvar v, _) as arg) :: rargs -> (
      (* We will use the name of the head column of the submatrix
         we compile, and this is the *second* column of our argument. *)
      match cls with
      | [ _ ] ->
          (* as split as it can *)
          do_not_precompile args cls def k
      | _ -> (
          (* Precompile *)
          let var_cls =
            List.map
              (fun (ps, act) ->
                match ps with
                | p :: ps ->
                    assert (group_var p);
                    (ps, act)
                | _ -> assert false)
              cls
          and var_def = Default_environment.pop_column def in
          let { me = first; matrix }, nexts =
            split_or (Some v) var_cls (arg :: rargs) var_def
          in
          (* Compute top information *)
          match nexts with
          | [] ->
              (* If you need *)
              do_not_precompile args cls def k
          | _ ->
              let rec rebuild_matrix pmh =
                match pmh with
                | Pm pm -> as_matrix pm.cases
                | PmOr { or_matrix = m } -> m
                | PmVar x -> add_omega_column (rebuild_matrix x.inside)
              in
              let rebuild_default nexts def =
                (* We can't just do:
                   {[
                     List.map
                       (fun (mat, e) -> add_omega_column mat, e)
                       top_default (* assuming it'd been bound. *)
                   ]}
                   As we would be loosing information: [def] is more precise
                   than [add_omega_column (pop_column def)]. *)
                List.fold_right
                  (fun (e, pmh) ->
                    Default_environment.cons
                      (add_omega_column (rebuild_matrix pmh))
                      e)
                  nexts def
              in
              let rebuild_nexts nexts k =
                map_end (fun (e, pm) -> (e, PmVar { inside = pm })) nexts k
              in
              let rfirst =
                { me = PmVar { inside = first };
                  matrix = add_omega_column matrix;
                  top_default = rebuild_default nexts def
                }
              and rnexts = rebuild_nexts nexts k in
              (rfirst, rnexts)
        )
    )
  | _ -> do_not_precompile args cls def k

and do_not_precompile args cls def k =
  ( { me = Pm { cases = cls; args; default = def };
      matrix = as_matrix cls;
      top_default = def
    },
    k )

and precompile_or argo cls ors args def k =
  let rec do_cases = function
    | (({ pat_desc = Tpat_or _ } as orp) :: patl, action) :: rem ->
        let others, rem = extract_equiv_head orp rem in
        let orpm =
          { cases =
              (patl, action)
              :: List.map
                   (function
                     | _ :: ps, action -> (ps, action)
                     | _ -> assert false)
                   others;
            args =
              ( match args with
              | _ :: r -> r
              | _ -> assert false
              );
            default = Default_environment.pop_compat orp def
          }
        in
        let pm_fv = pm_free_variables orpm in
        let vars =
          (* bound variables of the or-pattern and used in the orpm actions *)
          Typedtree.pat_bound_idents_full orp
          |> List.filter (fun (id, _, _) -> Ident.Set.mem id pm_fv)
          |> List.map (fun (id, _, ty) ->
                 (id, Typeopt.value_kind orp.pat_env ty))
        in
        let or_num = next_raise_count () in
        let new_patl = Parmatch.omega_list patl in
        let mk_new_action vs =
          Lstaticraise (or_num, List.map (fun v -> Lvar v) vs)
        in
        let rem_cases, rem_handlers = do_cases rem in
        let cases =
          explode_or_pat orp argo new_patl mk_new_action (List.map fst vars) []
            rem_cases
        in
        let handler =
          { provenance = [ [ orp ] ]; exit = or_num; vars; pm = orpm }
        in
        (cases, handler :: rem_handlers)
    | cl :: rem ->
        let new_ord, new_to_catch = do_cases rem in
        (cl :: new_ord, new_to_catch)
    | [] -> ([], [])
  in
  let cases, handlers = do_cases ors in
  let matrix = as_matrix (cls @ ors)
  and body = { cases = cls @ cases; args; default = def } in
  ( { me = PmOr { body; handlers; or_matrix = matrix };
      matrix;
      top_default = def
    },
    k )

let split_and_precompile argo pm =
  let { me = next }, nexts = split_or argo pm.cases pm.args pm.default in
  if
    dbg
    && (nexts <> []
       ||
       match next with
       | PmOr _ -> true
       | _ -> false
       )
  then (
    Format.eprintf "** SPLIT **\n";
    pretty_pm pm;
    pretty_precompiled_res next nexts
  );
  (next, nexts)

(* General divide functions *)

type cell = { pm : pattern_matching; ctx : Context.t; discr : pattern }
(** a submatrix after specializing by discriminant pattern;
    [ctx] is the context shared by all rows. *)

type 'a division = {
  args : (lambda * let_kind) list;
  cells : ('a * cell) list
}

let add_in_div make_matching_fun eq_key key patl_action division =
  let cells =
    match List.find_opt (fun (k, _) -> eq_key key k) division.cells with
    | None ->
        let cell = make_matching_fun division.args in
        cell.pm.cases <- [ patl_action ];
        (key, cell) :: division.cells
    | Some (_, cell) ->
        cell.pm.cases <- patl_action :: cell.pm.cases;
        division.cells
  in
  { division with cells }

let divide make eq_key get_key get_args ctx (pm : pattern_matching) =
  let add clause division =
    match clause with
    | [], _ -> assert false
    | p :: patl, action ->
        add_in_div (make p pm.default ctx) eq_key (get_key p)
          (get_args p patl, action)
          division
  in
  List.fold_right add pm.cases { args = pm.args; cells = [] }

let add_line patl_action pm =
  pm.cases <- patl_action :: pm.cases;
  pm

let divide_line make_ctx make get_args discr ctx (pm : pattern_matching) =
  let add clause submatrix =
    match clause with
    | [], _ -> assert false
    | p :: patl, action -> add_line (get_args p patl, action) submatrix
  in
  let pm = List.fold_right add pm.cases (make pm.default pm.args) in
  { pm; ctx = make_ctx ctx; discr }

(* Then come various functions,
   There is one set of functions per matching style
   (constants, constructors etc.)

   - matcher functions are arguments to Default_environment.specialize (for
   default handlers)
   They may raise NoMatch or OrPat and perform the full
   matching (selection + arguments).

   - get_args and get_key are for the compiled matrices, note that
   selection and getting arguments are separated.

   - make_*_matching combines the previous functions for producing
   new  ``pattern_matching'' records.
*)

let rec matcher_const cst p rem =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) -> (
      try matcher_const cst p1 rem with NoMatch -> matcher_const cst p2 rem
    )
  | Tpat_constant c1 when const_compare c1 cst = 0 -> rem
  | Tpat_any -> rem
  | _ -> raise NoMatch

let get_key_constant caller = function
  | { pat_desc = Tpat_constant cst } -> cst
  | p ->
      Format.eprintf "BAD: %s" caller;
      pretty_pat p;
      assert false

let get_args_constant _ rem = rem

let make_constant_matching p def ctx = function
  | [] -> fatal_error "Matching.make_constant_matching"
  | _ :: argl ->
      let def =
        Default_environment.specialize
          (matcher_const (get_key_constant "make" p))
          def
      and ctx = Context.specialize p ctx in
      { pm = { cases = []; args = argl; default = def };
        ctx;
        discr = normalize_pat p
      }

let divide_constant ctx m =
  divide make_constant_matching
    (fun c d -> const_compare c d = 0)
    (get_key_constant "divide")
    get_args_constant ctx m

(* Matching against a constructor *)

let make_field_args loc binding_kind arg first_pos last_pos argl =
  let rec make_args pos =
    if pos > last_pos then
      argl
    else
      (Lprim (Pfield pos, [ arg ], loc), binding_kind) :: make_args (pos + 1)
  in
  make_args first_pos

let get_key_constr = function
  | { pat_desc = Tpat_construct (_, cstr, _) } -> cstr.cstr_tag
  | _ -> assert false

let get_args_constr p rem =
  match p with
  | { pat_desc = Tpat_construct (_, _, args) } -> args @ rem
  | _ -> assert false

(* NB: matcher_constr applies to default matrices.

       In that context, matching by constructors of extensible
       types degrades to arity checking, due to potential rebinding.
       This comparison is performed by Types.may_equal_constr.
*)

let matcher_constr cstr =
  match cstr.cstr_arity with
  | 0 ->
      let rec matcher_rec q rem =
        match q.pat_desc with
        | Tpat_or (p1, p2, _) -> (
            try matcher_rec p1 rem with NoMatch -> matcher_rec p2 rem
          )
        | Tpat_construct (_, cstr', []) when Types.may_equal_constr cstr cstr'
          ->
            rem
        | Tpat_any -> rem
        | _ -> raise NoMatch
      in
      matcher_rec
  | 1 ->
      let rec matcher_rec q rem =
        match q.pat_desc with
        | Tpat_or (p1, p2, _) -> (
            (* if both sides of the or-pattern match the head constructor,
            (K p1 | K p2) :: rem
          return (p1 | p2) :: rem *)
            let r1 = try Some (matcher_rec p1 rem) with NoMatch -> None
            and r2 = try Some (matcher_rec p2 rem) with NoMatch -> None in
            match (r1, r2) with
            | None, None -> raise NoMatch
            | Some r1, None -> r1
            | None, Some r2 -> r2
            | Some (a1 :: _), Some (a2 :: _) ->
                { a1 with
                  pat_loc = Location.none;
                  pat_desc = Tpat_or (a1, a2, None)
                }
                :: rem
            | _, _ -> assert false
          )
        | Tpat_construct (_, cstr', [ arg ])
          when Types.may_equal_constr cstr cstr' ->
            arg :: rem
        | Tpat_any -> omega :: rem
        | _ -> raise NoMatch
      in
      matcher_rec
  | _ -> (
      fun q rem ->
        match q.pat_desc with
        | Tpat_or (_, _, _) ->
            (* we cannot preserve the or-pattern as in the arity-1 case,
          because we cannot express
            (K (p1, .., pn) | K (q1, .. qn))
          as (p1 .. pn | q1 .. qn) *)
            raise OrPat
        | Tpat_construct (_, cstr', args)
          when Types.may_equal_constr cstr cstr' ->
            args @ rem
        | Tpat_any -> Parmatch.omegas cstr.cstr_arity @ rem
        | _ -> raise NoMatch
    )

let make_constr_matching p def ctx = function
  | [] -> fatal_error "Matching.make_constr_matching"
  | (arg, _mut) :: argl ->
      let cstr = pat_as_constr p in
      let newargs =
        if cstr.cstr_inlined <> None then
          (arg, Alias) :: argl
        else
          match cstr.cstr_tag with
          | Cstr_constant _
          | Cstr_block _ ->
              make_field_args p.pat_loc Alias arg 0 (cstr.cstr_arity - 1) argl
          | Cstr_unboxed -> (arg, Alias) :: argl
          | Cstr_extension _ ->
              make_field_args p.pat_loc Alias arg 1 cstr.cstr_arity argl
      in
      { pm =
          { cases = [];
            args = newargs;
            default = Default_environment.specialize (matcher_constr cstr) def
          };
        ctx = Context.specialize p ctx;
        discr = normalize_pat p
      }

let divide_constructor ctx pm =
  divide make_constr_matching ( = ) get_key_constr get_args_constr ctx pm

(* Matching against a variant *)

let rec matcher_variant_const lab p rem =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) -> (
      try matcher_variant_const lab p1 rem
      with NoMatch -> matcher_variant_const lab p2 rem
    )
  | Tpat_variant (lab1, _, _) when lab1 = lab -> rem
  | Tpat_any -> rem
  | _ -> raise NoMatch

let make_variant_matching_constant p lab def ctx = function
  | [] -> fatal_error "Matching.make_variant_matching_constant"
  | _ :: argl ->
      let def = Default_environment.specialize (matcher_variant_const lab) def
      and ctx = Context.specialize p ctx in
      { pm = { cases = []; args = argl; default = def };
        ctx;
        discr = normalize_pat p
      }

let matcher_variant_nonconst lab p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_variant (lab1, Some arg, _) when lab1 = lab -> arg :: rem
  | Tpat_any -> omega :: rem
  | _ -> raise NoMatch

let make_variant_matching_nonconst p lab def ctx = function
  | [] -> fatal_error "Matching.make_variant_matching_nonconst"
  | (arg, _mut) :: argl ->
      let def =
        Default_environment.specialize (matcher_variant_nonconst lab) def
      and ctx = Context.specialize p ctx in
      { pm =
          { cases = [];
            args = (Lprim (Pfield 1, [ arg ], p.pat_loc), Alias) :: argl;
            default = def
          };
        ctx;
        discr = normalize_pat p
      }

let divide_variant row ctx { cases = cl; args; default = def } =
  let row = Btype.row_repr row in
  let rec divide = function
    | (({ pat_desc = Tpat_variant (lab, pato, _) } as p) :: patl, action)
      :: rem -> (
        let variants = divide rem in
        if
          try Btype.row_field_repr (List.assoc lab row.row_fields) = Rabsent
          with Not_found -> true
        then
          variants
        else
          let tag = Btype.hash_variant lab in
          match pato with
          | None ->
              add_in_div
                (make_variant_matching_constant p lab def ctx)
                ( = ) (Cstr_constant tag) (patl, action) variants
          | Some pat ->
              add_in_div
                (make_variant_matching_nonconst p lab def ctx)
                ( = ) (Cstr_block tag)
                (pat :: patl, action)
                variants
      )
    | _ -> { args; cells = [] }
  in
  divide cl

(*
  Three ``no-test'' cases
  *)

(* Matching against a variable *)
let get_args_var _p rem = rem

let make_var_matching def = function
  | [] -> fatal_error "Matching.make_var_matching"
  | _ :: argl ->
      { cases = [];
        args = argl;
        default = Default_environment.specialize get_args_var def
      }

let divide_var ctx pm =
  divide_line Context.lshift make_var_matching get_args_var omega ctx pm

(* Matching and forcing a lazy value *)

let get_arg_lazy p rem =
  match p with
  | { pat_desc = Tpat_any } -> omega :: rem
  | { pat_desc = Tpat_lazy arg } -> arg :: rem
  | _ -> assert false

let matcher_lazy p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any
  | Tpat_var _ ->
      omega :: rem
  | Tpat_lazy arg -> arg :: rem
  | _ -> raise NoMatch

(* Inlining the tag tests before calling the primitive that works on
   lazy blocks. This is also used in translcore.ml.
   No other call than Obj.tag when the value has been forced before.
*)

let prim_obj_tag = Primitive.simple ~name:"caml_obj_tag" ~arity:1 ~alloc:false

let get_mod_field modname field =
  lazy
    (let mod_ident = Ident.create_persistent modname in
     let env =
       Env.add_persistent_structure mod_ident Env.initial_safe_string
     in
     match Env.open_pers_signature modname env with
     | exception Not_found ->
         fatal_error ("Module " ^ modname ^ " unavailable.")
     | env -> (
         match Env.find_value_by_name (Longident.Lident field) env with
         | exception Not_found ->
             fatal_error ("Primitive " ^ modname ^ "." ^ field ^ " not found.")
         | path, _ -> transl_value_path Location.none env path
       ))

let code_force_lazy_block = get_mod_field "CamlinternalLazy" "force_lazy_block"

let code_force_lazy = get_mod_field "CamlinternalLazy" "force"

(* inline_lazy_force inlines the beginning of the code of Lazy.force. When
   the value argument is tagged as:
   - forward, take field 0
   - lazy, call the primitive that forces (without testing again the tag)
   - anything else, return it

   Using Lswitch below relies on the fact that the GC does not shortcut
   Forward(val_out_of_heap).
*)

let inline_lazy_force_cond arg loc =
  let idarg = Ident.create_local "lzarg" in
  let varg = Lvar idarg in
  let tag = Ident.create_local "tag" in
  let tag_var = Lvar tag in
  let force_fun = Lazy.force code_force_lazy_block in
  Llet
    ( Strict,
      Pgenval,
      idarg,
      arg,
      Llet
        ( Alias,
          Pgenval,
          tag,
          Lprim (Pccall prim_obj_tag, [ varg ], loc),
          Lifthenelse
            ( (* if (tag == Obj.forward_tag) then varg.(0) else ... *)
              Lprim
                ( Pintcomp Ceq,
                  [ tag_var; Lconst (Const_base (Const_int Obj.forward_tag)) ],
                  loc ),
              Lprim (Pfield 0, [ varg ], loc),
              Lifthenelse
                ( (* if (tag == Obj.lazy_tag) then Lazy.force varg else ... *)
                  Lprim
                    ( Pintcomp Ceq,
                      [ tag_var; Lconst (Const_base (Const_int Obj.lazy_tag)) ],
                      loc ),
                  Lapply
                    { ap_should_be_tailcall = false;
                      ap_loc = loc;
                      ap_func = force_fun;
                      ap_args = [ varg ];
                      ap_inlined = Default_inline;
                      ap_specialised = Default_specialise
                    },
                  (* ... arg *)
                  varg ) ) ) )

let inline_lazy_force_switch arg loc =
  let idarg = Ident.create_local "lzarg" in
  let varg = Lvar idarg in
  let force_fun = Lazy.force code_force_lazy_block in
  Llet
    ( Strict,
      Pgenval,
      idarg,
      arg,
      Lifthenelse
        ( Lprim (Pisint, [ varg ], loc),
          varg,
          Lswitch
            ( varg,
              { sw_numconsts = 0;
                sw_consts = [];
                sw_numblocks = 256;
                (* PR#6033 - tag ranges from 0 to 255 *)
                sw_blocks =
                  [ (Obj.forward_tag, Lprim (Pfield 0, [ varg ], loc));
                    ( Obj.lazy_tag,
                      Lapply
                        { ap_should_be_tailcall = false;
                          ap_loc = loc;
                          ap_func = force_fun;
                          ap_args = [ varg ];
                          ap_inlined = Default_inline;
                          ap_specialised = Default_specialise
                        } )
                  ];
                sw_failaction = Some varg
              },
              loc ) ) )

let inline_lazy_force arg loc =
  if !Clflags.afl_instrument then
    (* Disable inlining optimisation if AFL instrumentation active,
       so that the GC forwarding optimisation is not visible in the
       instrumentation output.
       (see https://github.com/stedolan/crowbar/issues/14) *)
    Lapply
      { ap_should_be_tailcall = false;
        ap_loc = loc;
        ap_func = Lazy.force code_force_lazy;
        ap_args = [ arg ];
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise
      }
  else if !Clflags.native_code then
    (* Lswitch generates compact and efficient native code *)
    inline_lazy_force_switch arg loc
  else
    (* generating bytecode: Lswitch would generate too many rather big
         tables (~ 250 elts); conditionals are better *)
    inline_lazy_force_cond arg loc

let make_lazy_matching def = function
  | [] -> fatal_error "Matching.make_lazy_matching"
  | (arg, _mut) :: argl ->
      { cases = [];
        args = (inline_lazy_force arg Location.none, Strict) :: argl;
        default = Default_environment.specialize matcher_lazy def
      }

let divide_lazy p ctx pm =
  divide_line (Context.specialize p) make_lazy_matching get_arg_lazy p ctx pm

(* Matching against a tuple pattern *)

let get_args_tuple arity p rem =
  match p with
  | { pat_desc = Tpat_any } -> omegas arity @ rem
  | { pat_desc = Tpat_tuple args } -> args @ rem
  | _ -> assert false

let matcher_tuple arity p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any
  | Tpat_var _ ->
      omegas arity @ rem
  | Tpat_tuple args when List.length args = arity -> args @ rem
  | _ -> raise NoMatch

let make_tuple_matching loc arity def = function
  | [] -> fatal_error "Matching.make_tuple_matching"
  | (arg, _mut) :: argl ->
      let rec make_args pos =
        if pos >= arity then
          argl
        else
          (Lprim (Pfield pos, [ arg ], loc), Alias) :: make_args (pos + 1)
      in
      { cases = [];
        args = make_args 0;
        default = Default_environment.specialize (matcher_tuple arity) def
      }

let divide_tuple arity p ctx pm =
  divide_line (Context.specialize p)
    (make_tuple_matching p.pat_loc arity)
    (get_args_tuple arity) p ctx pm

(* Matching against a record pattern *)

let record_matching_line num_fields lbl_pat_list =
  let patv = Array.make num_fields omega in
  List.iter (fun (_, lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
  Array.to_list patv

let get_args_record num_fields p rem =
  match p with
  | { pat_desc = Tpat_any } -> record_matching_line num_fields [] @ rem
  | { pat_desc = Tpat_record (lbl_pat_list, _) } ->
      record_matching_line num_fields lbl_pat_list @ rem
  | _ -> assert false

let matcher_record num_fields p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any
  | Tpat_var _ ->
      record_matching_line num_fields [] @ rem
  | Tpat_record ([], _) when num_fields = 0 -> rem
  | Tpat_record (((_, lbl, _) :: _ as lbl_pat_list), _)
    when Array.length lbl.lbl_all = num_fields ->
      record_matching_line num_fields lbl_pat_list @ rem
  | _ -> raise NoMatch

let make_record_matching loc all_labels def = function
  | [] -> fatal_error "Matching.make_record_matching"
  | (arg, _mut) :: argl ->
      let rec make_args pos =
        if pos >= Array.length all_labels then
          argl
        else
          let lbl = all_labels.(pos) in
          let access =
            match lbl.lbl_repres with
            | Record_regular
            | Record_inlined _ ->
                Lprim (Pfield lbl.lbl_pos, [ arg ], loc)
            | Record_unboxed _ -> arg
            | Record_float -> Lprim (Pfloatfield lbl.lbl_pos, [ arg ], loc)
            | Record_extension _ ->
                Lprim (Pfield (lbl.lbl_pos + 1), [ arg ], loc)
          in
          let str =
            match lbl.lbl_mut with
            | Immutable -> Alias
            | Mutable -> StrictOpt
          in
          (access, str) :: make_args (pos + 1)
      in
      let nfields = Array.length all_labels in
      let def = Default_environment.specialize (matcher_record nfields) def in
      { cases = []; args = make_args 0; default = def }

let divide_record all_labels p ctx pm =
  let get_args = get_args_record (Array.length all_labels) in
  divide_line (Context.specialize p)
    (make_record_matching p.pat_loc all_labels)
    get_args p ctx pm

(* Matching against an array pattern *)

let get_key_array = function
  | { pat_desc = Tpat_array patl } -> List.length patl
  | _ -> assert false

let get_args_array p rem =
  match p with
  | { pat_desc = Tpat_array patl } -> patl @ rem
  | _ -> assert false

let matcher_array len p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_array args when List.length args = len -> args @ rem
  | Tpat_any -> Parmatch.omegas len @ rem
  | _ -> raise NoMatch

let make_array_matching kind p def ctx = function
  | [] -> fatal_error "Matching.make_array_matching"
  | (arg, _mut) :: argl ->
      let len = get_key_array p in
      let rec make_args pos =
        if pos >= len then
          argl
        else
          ( Lprim
              ( Parrayrefu kind,
                [ arg; Lconst (Const_base (Const_int pos)) ],
                p.pat_loc ),
            StrictOpt )
          :: make_args (pos + 1)
      in
      let def = Default_environment.specialize (matcher_array len) def
      and ctx = Context.specialize p ctx in
      { pm = { cases = []; args = make_args 0; default = def };
        ctx;
        discr = normalize_pat p
      }

let divide_array kind ctx pm =
  divide (make_array_matching kind) ( = ) get_key_array get_args_array ctx pm

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
  Pccall (Primitive.simple ~name:"caml_string_notequal" ~arity:2 ~alloc:false)

let prim_string_compare =
  Pccall (Primitive.simple ~name:"caml_string_compare" ~arity:2 ~alloc:false)

let bind_sw arg k =
  match arg with
  | Lvar _ -> k arg
  | _ ->
      let id = Ident.create_local "switch" in
      Llet (Strict, Pgenval, id, arg, k (Lvar id))

(* Sequential equality tests *)

let make_string_test_sequence loc arg sw d =
  let d, sw =
    match d with
    | None -> (
        match sw with
        | (_, d) :: sw -> (d, sw)
        | [] -> assert false
      )
    | Some d -> (d, sw)
  in
  bind_sw arg (fun arg ->
      List.fold_right
        (fun (str, lam) k ->
          Lifthenelse
            ( Lprim
                ( prim_string_notequal,
                  [ arg; Lconst (Const_immstring str) ],
                  loc ),
              k,
              lam ))
        sw d)

let rec split k xs =
  match xs with
  | [] -> assert false
  | x0 :: xs ->
      if k <= 1 then
        ([], x0, xs)
      else
        let xs, y0, ys = split (k - 2) xs in
        (x0 :: xs, y0, ys)

let zero_lam = Lconst (Const_base (Const_int 0))

let tree_way_test loc arg lt eq gt =
  Lifthenelse
    ( Lprim (Pintcomp Clt, [ arg; zero_lam ], loc),
      lt,
      Lifthenelse (Lprim (Pintcomp Clt, [ zero_lam; arg ], loc), gt, eq) )

(* Dichotomic tree *)

let rec do_make_string_test_tree loc arg sw delta d =
  let len = List.length sw in
  if len <= strings_test_threshold + delta then
    make_string_test_sequence loc arg sw d
  else
    let lt, (s, act), gt = split len sw in
    bind_sw
      (Lprim (prim_string_compare, [ arg; Lconst (Const_immstring s) ], loc))
      (fun r ->
        tree_way_test loc r
          (do_make_string_test_tree loc arg lt delta d)
          act
          (do_make_string_test_tree loc arg gt delta d))

(* Entry point *)
let expand_stringswitch loc arg sw d =
  match d with
  | None -> bind_sw arg (fun arg -> do_make_string_test_tree loc arg sw 0 None)
  | Some e ->
      bind_sw arg (fun arg ->
          make_catch e (fun d ->
              do_make_string_test_tree loc arg sw 1 (Some d)))

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
        let i, h = make_catch_delayed act in
        let ohs = !hs in
        (hs := fun act -> h (ohs act));
        make_exit i
  in
  (hs, handle_shared)

let share_actions_tree sw d =
  let store = StoreExp.mk_store () in
  (* Default action is always shared *)
  let d =
    match d with
    | None -> None
    | Some d -> Some (store.Switch.act_store_shared () d)
  in
  (* Store all other actions *)
  let sw =
    List.map (fun (cst, act) -> (cst, store.Switch.act_store () act)) sw
  in
  (* Retrieve all actions, including potential default *)
  let acts = store.Switch.act_get_shared () in
  (* Array of actual actions *)
  let hs, handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in
  (* Reconstruct default and switch list *)
  let d =
    match d with
    | None -> None
    | Some d -> Some acts.(d)
  in
  let sw = List.map (fun (cst, j) -> (cst, acts.(j))) sw in
  (!hs, sw, d)

(* Note: dichotomic search requires sorted input with no duplicates *)
let rec uniq_lambda_list sw =
  match sw with
  | []
  | [ _ ] ->
      sw
  | ((c1, _) as p1) :: ((c2, _) :: sw2 as sw1) ->
      if const_compare c1 c2 = 0 then
        uniq_lambda_list (p1 :: sw2)
      else
        p1 :: uniq_lambda_list sw1

let sort_lambda_list l =
  let l = List.stable_sort (fun (x, _) (y, _) -> const_compare x y) l in
  uniq_lambda_list l

let rec do_tests_fail loc fail tst arg = function
  | [] -> fail
  | (c, act) :: rem ->
      Lifthenelse
        ( Lprim (tst, [ arg; Lconst (Const_base c) ], loc),
          do_tests_fail loc fail tst arg rem,
          act )

let rec do_tests_nofail loc tst arg = function
  | [] -> fatal_error "Matching.do_tests_nofail"
  | [ (_, act) ] -> act
  | (c, act) :: rem ->
      Lifthenelse
        ( Lprim (tst, [ arg; Lconst (Const_base c) ], loc),
          do_tests_nofail loc tst arg rem,
          act )

let make_test_sequence loc fail tst lt_tst arg const_lambda_list =
  let const_lambda_list = sort_lambda_list const_lambda_list in
  let hs, const_lambda_list, fail =
    share_actions_tree const_lambda_list fail
  in
  let rec make_test_sequence const_lambda_list =
    if List.length const_lambda_list >= 4 && lt_tst <> Pignore then
      split_sequence const_lambda_list
    else
      match fail with
      | None -> do_tests_nofail loc tst arg const_lambda_list
      | Some fail -> do_tests_fail loc fail tst arg const_lambda_list
  and split_sequence const_lambda_list =
    let list1, list2 =
      rev_split_at (List.length const_lambda_list / 2) const_lambda_list
    in
    Lifthenelse
      ( Lprim (lt_tst, [ arg; Lconst (Const_base (fst (List.hd list2))) ], loc),
        make_test_sequence list1,
        make_test_sequence list2 )
  in
  hs (make_test_sequence const_lambda_list)

module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq

  let neint = Pintcomp Cne

  let leint = Pintcomp Cle

  let ltint = Pintcomp Clt

  let geint = Pintcomp Cge

  let gtint = Pintcomp Cgt

  type act = Lambda.lambda

  let make_prim p args = Lprim (p, args, Location.none)

  let make_offset arg n =
    match n with
    | 0 -> arg
    | _ -> Lprim (Poffsetint n, [ arg ], Location.none)

  let bind arg body =
    let newvar, newarg =
      match arg with
      | Lvar v -> (v, arg)
      | _ ->
          let newvar = Ident.create_local "switcher" in
          (newvar, Lvar newvar)
    in
    bind Alias newvar arg (body newarg)

  let make_const i = Lconst (Const_base (Const_int i))

  let make_isout h arg = Lprim (Pisout, [ h; arg ], Location.none)

  let make_isin h arg = Lprim (Pnot, [ make_isout h arg ], Location.none)

  let make_if cond ifso ifnot = Lifthenelse (cond, ifso, ifnot)

  let make_switch loc arg cases acts =
    let l = ref [] in
    for i = Array.length cases - 1 downto 0 do
      l := (i, acts.(cases.(i))) :: !l
    done;
    Lswitch
      ( arg,
        { sw_numconsts = Array.length cases;
          sw_consts = !l;
          sw_numblocks = 0;
          sw_blocks = [];
          sw_failaction = None
        },
        loc )

  let make_catch = make_catch_delayed

  let make_exit = make_exit
end

(* Action sharing for Lswitch argument *)
let share_actions_sw sw =
  (* Attempt sharing on all actions *)
  let store = StoreExp.mk_store () in
  let fail =
    match sw.sw_failaction with
    | None -> None
    | Some fail ->
        (* Fail is translated to exit, whatever happens *)
        Some (store.Switch.act_store_shared () fail)
  in
  let consts =
    List.map (fun (i, e) -> (i, store.Switch.act_store () e)) sw.sw_consts
  and blocks =
    List.map (fun (i, e) -> (i, store.Switch.act_store () e)) sw.sw_blocks
  in
  let acts = store.Switch.act_get_shared () in
  let hs, handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in
  let fail =
    match fail with
    | None -> None
    | Some fail -> Some acts.(fail)
  in
  ( !hs,
    { sw with
      sw_consts = List.map (fun (i, j) -> (i, acts.(j))) consts;
      sw_blocks = List.map (fun (i, j) -> (i, acts.(j))) blocks;
      sw_failaction = fail
    } )

(* Reintroduce fail action in switch argument,
   for the sake of avoiding carrying over huge switches *)

let reintroduce_fail sw =
  match sw.sw_failaction with
  | None ->
      let t = Hashtbl.create 17 in
      let seen (_, l) =
        match as_simple_exit l with
        | Some i ->
            let old = try Hashtbl.find t i with Not_found -> 0 in
            Hashtbl.replace t i (old + 1)
        | None -> ()
      in
      List.iter seen sw.sw_consts;
      List.iter seen sw.sw_blocks;
      let i_max = ref (-1) and max = ref (-1) in
      Hashtbl.iter
        (fun i c ->
          if c > !max then (
            i_max := i;
            max := c
          ))
        t;
      if !max >= 3 then
        let default = !i_max in
        let remove =
          List.filter (fun (_, lam) ->
              match as_simple_exit lam with
              | Some j -> j <> default
              | None -> true)
        in
        { sw with
          sw_consts = remove sw.sw_consts;
          sw_blocks = remove sw.sw_blocks;
          sw_failaction = Some (make_exit default)
        }
      else
        sw
  | Some _ -> sw

module Switcher = Switch.Make (SArg)
open Switch

let rec last def = function
  | [] -> def
  | [ (x, _) ] -> x
  | _ :: rem -> last def rem

let get_edges low high l =
  match l with
  | [] -> (low, high)
  | (x, _) :: _ -> (x, last high l)

let as_interval_canfail fail low high l =
  let store = StoreExp.mk_store () in
  let do_store _tag act =
    let i = store.act_store () act in
    (*
    eprintf "STORE [%s] %i %s\n" tag i (string_of_lam act) ;
*)
    i
  in
  let rec nofail_rec cur_low cur_high cur_act = function
    | [] ->
        if cur_high = high then
          [ (cur_low, cur_high, cur_act) ]
        else
          [ (cur_low, cur_high, cur_act); (cur_high + 1, high, 0) ]
    | (i, act_i) :: rem as all ->
        let act_index = do_store "NO" act_i in
        if cur_high + 1 = i then
          if act_index = cur_act then
            nofail_rec cur_low i cur_act rem
          else if act_index = 0 then
            (cur_low, i - 1, cur_act) :: fail_rec i i rem
          else
            (cur_low, i - 1, cur_act) :: nofail_rec i i act_index rem
        else if act_index = 0 then
          (cur_low, cur_high, cur_act)
          :: fail_rec (cur_high + 1) (cur_high + 1) all
        else
          (cur_low, cur_high, cur_act)
          :: (cur_high + 1, i - 1, 0)
          :: nofail_rec i i act_index rem
  and fail_rec cur_low cur_high = function
    | [] -> [ (cur_low, cur_high, 0) ]
    | (i, act_i) :: rem ->
        let index = do_store "YES" act_i in
        if index = 0 then
          fail_rec cur_low i rem
        else
          (cur_low, i - 1, 0) :: nofail_rec i i index rem
  in
  let init_rec = function
    | [] -> [ (low, high, 0) ]
    | (i, act_i) :: rem ->
        let index = do_store "INIT" act_i in
        if index = 0 then
          fail_rec low i rem
        else if low < i then
          (low, i - 1, 0) :: nofail_rec i i index rem
        else
          nofail_rec i i index rem
  in
  assert (do_store "FAIL" fail = 0);

  (* fail has action index 0 *)
  let r = init_rec l in
  (Array.of_list r, store)

let as_interval_nofail l =
  let store = StoreExp.mk_store () in
  let rec some_hole = function
    | []
    | [ _ ] ->
        false
    | (i, _) :: ((j, _) :: _ as rem) -> j > i + 1 || some_hole rem
  in
  let rec i_rec cur_low cur_high cur_act = function
    | [] -> [ (cur_low, cur_high, cur_act) ]
    | (i, act) :: rem ->
        let act_index = store.act_store () act in
        if act_index = cur_act then
          i_rec cur_low i cur_act rem
        else
          (cur_low, cur_high, cur_act) :: i_rec i i act_index rem
  in
  let inters =
    match l with
    | (i, act) :: rem ->
        let act_index =
          (* In case there is some hole and that a switch is emitted,
           action 0 will be used as the action of unreachable
           cases (cf. switch.ml, make_switch).
           Hence, this action will be shared *)
          if some_hole rem then
            store.act_store_shared () act
          else
            store.act_store () act
        in
        assert (act_index = 0);
        i_rec i i act_index rem
    | _ -> assert false
  in
  (Array.of_list inters, store)

let sort_int_lambda_list l =
  List.sort
    (fun (i1, _) (i2, _) ->
      if i1 < i2 then
        -1
      else if i2 < i1 then
        1
      else
        0)
    l

let as_interval fail low high l =
  let l = sort_int_lambda_list l in
  ( get_edges low high l,
    match fail with
    | None -> as_interval_nofail l
    | Some act -> as_interval_canfail act low high l )

let call_switcher loc fail arg low high int_lambda_list =
  let edges, (cases, actions) = as_interval fail low high int_lambda_list in
  Switcher.zyva loc edges arg cases actions

let rec list_as_pat = function
  | [] -> fatal_error "Matching.list_as_pat"
  | [ pat ] -> pat
  | pat :: rem -> { pat with pat_desc = Tpat_or (pat, list_as_pat rem, None) }

let complete_pats_constrs = function
  | p :: _ as pats ->
      List.map (pat_of_constr p)
        (complete_constrs p (List.map get_key_constr pats))
  | _ -> assert false

(*
     Following two ``failaction'' function compute n, the trap handler
    to jump to in case of failure of elementary tests
*)

let mk_failaction_neg partial ctx def =
  match partial with
  | Partial -> (
      match Default_environment.pop def with
      | Some ((_, idef), _) ->
          (Some (Lstaticraise (idef, [])), Jumps.singleton idef ctx)
      | None ->
          (* Act as Total, this means
          If no appropriate default matrix exists,
          then this switch cannot fail *)
          (None, Jumps.empty)
    )
  | Total -> (None, Jumps.empty)

(* In line with the article and simpler than before *)
let mk_failaction_pos partial seen ctx defs =
  if dbg then (
    Format.eprintf "**POS**\n";
    Default_environment.pp defs;
    ()
  );
  let rec scan_def env to_test defs =
    match (to_test, Default_environment.pop defs) with
    | [], _
    | _, None ->
        List.fold_left
          (fun (klist, jumps) (pats, i) ->
            let action = Lstaticraise (i, []) in
            let klist =
              List.fold_right
                (fun pat r -> (get_key_constr pat, action) :: r)
                pats klist
            and jumps =
              Jumps.add i (Context.lub (list_as_pat pats) ctx) jumps
            in
            (klist, jumps))
          ([], Jumps.empty) env
    | _, Some ((pss, idef), rem) -> (
        let now, later =
          List.partition (fun (_p, p_ctx) -> Context.matches p_ctx pss) to_test
        in
        match now with
        | [] -> scan_def env to_test rem
        | _ -> scan_def ((List.map fst now, idef) :: env) later rem
      )
  in
  let fail_pats = complete_pats_constrs seen in
  if List.length fail_pats < !Clflags.match_context_rows then (
    let fail, jmps =
      scan_def []
        (List.map (fun pat -> (pat, Context.lub pat ctx)) fail_pats)
        defs
    in
    if dbg then (
      eprintf "POSITIVE JUMPS [%i]:\n" (List.length fail_pats);
      Jumps.eprintf jmps
    );
    (None, fail, jmps)
  ) else (
    (* Too many non-matched constructors -> reduced information *)
    if dbg then eprintf "POS->NEG!!!\n%!";
    let fail, jumps = mk_failaction_neg partial ctx defs in
    if dbg then
      eprintf "FAIL: %s\n"
        ( match fail with
        | None -> "<none>"
        | Some lam -> string_of_lam lam
        );
    (fail, [], jumps)
  )

let combine_constant loc arg cst partial ctx def
    (const_lambda_list, total, _pats) =
  let fail, local_jumps = mk_failaction_neg partial ctx def in
  let lambda1 =
    match cst with
    | Const_int _ ->
        let int_lambda_list =
          List.map
            (function
              | Const_int n, l -> (n, l)
              | _ -> assert false)
            const_lambda_list
        in
        call_switcher loc fail arg min_int max_int int_lambda_list
    | Const_char _ ->
        let int_lambda_list =
          List.map
            (function
              | Const_char c, l -> (Char.code c, l)
              | _ -> assert false)
            const_lambda_list
        in
        call_switcher loc fail arg 0 255 int_lambda_list
    | Const_string _ ->
        (* Note as the bytecode compiler may resort to dichotomic search,
   the clauses of stringswitch  are sorted with duplicates removed.
   This partly applies to the native code compiler, which requires
   no duplicates *)
        let const_lambda_list = sort_lambda_list const_lambda_list in
        let sw =
          List.map
            (fun (c, act) ->
              match c with
              | Const_string (s, _) -> (s, act)
              | _ -> assert false)
            const_lambda_list
        in
        let hs, sw, fail = share_actions_tree sw fail in
        hs (Lstringswitch (arg, sw, fail, loc))
    | Const_float _ ->
        make_test_sequence loc fail (Pfloatcomp CFneq) (Pfloatcomp CFlt) arg
          const_lambda_list
    | Const_int32 _ ->
        make_test_sequence loc fail
          (Pbintcomp (Pint32, Cne))
          (Pbintcomp (Pint32, Clt))
          arg const_lambda_list
    | Const_int64 _ ->
        make_test_sequence loc fail
          (Pbintcomp (Pint64, Cne))
          (Pbintcomp (Pint64, Clt))
          arg const_lambda_list
    | Const_nativeint _ ->
        make_test_sequence loc fail
          (Pbintcomp (Pnativeint, Cne))
          (Pbintcomp (Pnativeint, Clt))
          arg const_lambda_list
  in
  (lambda1, Jumps.union local_jumps total)

let split_cases tag_lambda_list =
  let rec split_rec = function
    | [] -> ([], [])
    | (cstr, act) :: rem -> (
        let consts, nonconsts = split_rec rem in
        match cstr with
        | Cstr_constant n -> ((n, act) :: consts, nonconsts)
        | Cstr_block n -> (consts, (n, act) :: nonconsts)
        | Cstr_unboxed -> (consts, (0, act) :: nonconsts)
        | Cstr_extension _ -> assert false
      )
  in
  let const, nonconst = split_rec tag_lambda_list in
  (sort_int_lambda_list const, sort_int_lambda_list nonconst)

let split_extension_cases tag_lambda_list =
  let rec split_rec = function
    | [] -> ([], [])
    | (cstr, act) :: rem -> (
        let consts, nonconsts = split_rec rem in
        match cstr with
        | Cstr_extension (path, true) -> ((path, act) :: consts, nonconsts)
        | Cstr_extension (path, false) -> (consts, (path, act) :: nonconsts)
        | _ -> assert false
      )
  in
  split_rec tag_lambda_list

let combine_constructor loc arg ex_pat cstr partial ctx def
    (tag_lambda_list, total1, pats) =
  match cstr.cstr_tag with
  | Cstr_extension _ ->
      (* Special cases for extensions *)
      let fail, local_jumps = mk_failaction_neg partial ctx def in
      let lambda1 =
        let consts, nonconsts = split_extension_cases tag_lambda_list in
        let default, consts, nonconsts =
          match fail with
          | None -> (
              match (consts, nonconsts) with
              | _, (_, act) :: rem -> (act, consts, rem)
              | (_, act) :: rem, _ -> (act, rem, nonconsts)
              | _ -> assert false
            )
          | Some fail -> (fail, consts, nonconsts)
        in
        let nonconst_lambda =
          match nonconsts with
          | [] -> default
          | _ ->
              let tag = Ident.create_local "tag" in
              let tests =
                List.fold_right
                  (fun (path, act) rem ->
                    let ext = transl_extension_path loc ex_pat.pat_env path in
                    Lifthenelse
                      (Lprim (Pintcomp Ceq, [ Lvar tag; ext ], loc), act, rem))
                  nonconsts default
              in
              Llet (Alias, Pgenval, tag, Lprim (Pfield 0, [ arg ], loc), tests)
        in
        List.fold_right
          (fun (path, act) rem ->
            let ext = transl_extension_path loc ex_pat.pat_env path in
            Lifthenelse (Lprim (Pintcomp Ceq, [ arg; ext ], loc), act, rem))
          consts nonconst_lambda
      in
      (lambda1, Jumps.union local_jumps total1)
  | _ ->
      (* Regular concrete type *)
      let ncases = List.length tag_lambda_list
      and nconstrs = cstr.cstr_consts + cstr.cstr_nonconsts in
      let sig_complete = ncases = nconstrs in
      let fail_opt, fails, local_jumps =
        if sig_complete then
          (None, [], Jumps.empty)
        else
          mk_failaction_pos partial pats ctx def
      in
      let tag_lambda_list = fails @ tag_lambda_list in
      let consts, nonconsts = split_cases tag_lambda_list in
      let lambda1 =
        match (fail_opt, same_actions tag_lambda_list) with
        | None, Some act -> act (* Identical actions, no failure *)
        | _ -> (
            match
              (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts)
            with
            | 1, 1, [ (0, act1) ], [ (0, act2) ] ->
                (* Typically, match on lists, will avoid isint primitive in that
              case *)
                Lifthenelse (arg, act2, act1)
            | n, 0, _, [] ->
                (* The type defines constant constructors only *)
                call_switcher loc fail_opt arg 0 (n - 1) consts
            | n, _, _, _ -> (
                let act0 =
                  (* = Some act when all non-const constructors match to act *)
                  match (fail_opt, nonconsts) with
                  | Some a, [] -> Some a
                  | Some _, _ ->
                      if List.length nonconsts = cstr.cstr_nonconsts then
                        same_actions nonconsts
                      else
                        None
                  | None, _ -> same_actions nonconsts
                in
                match act0 with
                | Some act ->
                    Lifthenelse
                      ( Lprim (Pisint, [ arg ], loc),
                        call_switcher loc fail_opt arg 0 (n - 1) consts,
                        act )
                | None ->
                    (* Emit a switch, as bytecode implements this sophisticated
                      instruction *)
                    let sw =
                      { sw_numconsts = cstr.cstr_consts;
                        sw_consts = consts;
                        sw_numblocks = cstr.cstr_nonconsts;
                        sw_blocks = nonconsts;
                        sw_failaction = fail_opt
                      }
                    in
                    let hs, sw = share_actions_sw sw in
                    let sw = reintroduce_fail sw in
                    hs (Lswitch (arg, sw, loc))
              )
          )
      in
      (lambda1, Jumps.union local_jumps total1)

let make_test_sequence_variant_constant fail arg int_lambda_list =
  let _, (cases, actions) = as_interval fail min_int max_int int_lambda_list in
  Switcher.test_sequence arg cases actions

let call_switcher_variant_constant loc fail arg int_lambda_list =
  call_switcher loc fail arg min_int max_int int_lambda_list

let call_switcher_variant_constr loc fail arg int_lambda_list =
  let v = Ident.create_local "variant" in
  Llet
    ( Alias,
      Pgenval,
      v,
      Lprim (Pfield 0, [ arg ], loc),
      call_switcher loc fail (Lvar v) min_int max_int int_lambda_list )

let combine_variant loc row arg partial ctx def (tag_lambda_list, total1, _pats)
    =
  let row = Btype.row_repr row in
  let num_constr = ref 0 in
  if row.row_closed then
    List.iter
      (fun (_, f) ->
        match Btype.row_field_repr f with
        | Rabsent
        | Reither (true, _ :: _, _, _) ->
            ()
        | _ -> incr num_constr)
      row.row_fields
  else
    num_constr := max_int;
  let test_int_or_block arg if_int if_block =
    Lifthenelse (Lprim (Pisint, [ arg ], loc), if_int, if_block)
  in
  let sig_complete = List.length tag_lambda_list = !num_constr
  and one_action = same_actions tag_lambda_list in
  let fail, local_jumps =
    if
      sig_complete
      ||
      match partial with
      | Total -> true
      | _ -> false
    then
      (None, Jumps.empty)
    else
      mk_failaction_neg partial ctx def
  in
  let consts, nonconsts = split_cases tag_lambda_list in
  let lambda1 =
    match (fail, one_action) with
    | None, Some act -> act
    | _, _ -> (
        match (consts, nonconsts) with
        | [ (_, act1) ], [ (_, act2) ] when fail = None ->
            test_int_or_block arg act1 act2
        | _, [] ->
            (* One can compare integers and pointers *)
            make_test_sequence_variant_constant fail arg consts
        | [], _ -> (
            let lam = call_switcher_variant_constr loc fail arg nonconsts in
            (* One must not dereference integers *)
            match fail with
            | None -> lam
            | Some fail -> test_int_or_block arg fail lam
          )
        | _, _ ->
            let lam_const = call_switcher_variant_constant loc fail arg consts
            and lam_nonconst =
              call_switcher_variant_constr loc fail arg nonconsts
            in
            test_int_or_block arg lam_const lam_nonconst
      )
  in
  (lambda1, Jumps.union local_jumps total1)

let combine_array loc arg kind partial ctx def (len_lambda_list, total1, _pats)
    =
  let fail, local_jumps = mk_failaction_neg partial ctx def in
  let lambda1 =
    let newvar = Ident.create_local "len" in
    let switch =
      call_switcher loc fail (Lvar newvar) 0 max_int len_lambda_list
    in
    bind Alias newvar (Lprim (Parraylength kind, [ arg ], loc)) switch
  in
  (lambda1, Jumps.union local_jumps total1)

(* Insertion of debugging events *)

let rec event_branch repr lam =
  match (lam, repr) with
  | _, None -> lam
  | Levent (lam', ev), Some r ->
      incr r;
      Levent
        ( lam',
          { lev_loc = ev.lev_loc;
            lev_kind = ev.lev_kind;
            lev_repr = repr;
            lev_env = ev.lev_env
          } )
  | Llet (str, k, id, lam, body), _ ->
      Llet (str, k, id, lam, event_branch repr body)
  | Lstaticraise _, _ -> lam
  | _, Some _ ->
      Printlambda.lambda Format.str_formatter lam;
      fatal_error ("Matching.event_branch: " ^ Format.flush_str_formatter ())

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
    | [] -> ([], Jumps.unions totals, [])
    | (key, cell) :: rem -> (
        if Context.is_empty cell.ctx then
          c_rec totals rem
        else
          try
            let lambda1, total1 = compile_fun cell.ctx cell.pm in
            let c_rem, total, new_discrs =
              c_rec (Jumps.map Context.combine total1 :: totals) rem
            in
            ((key, lambda1) :: c_rem, total, cell.discr :: new_discrs)
          with Unused -> c_rec totals rem
      )
  in
  c_rec [] division

let compile_orhandlers compile_fun lambda1 total1 ctx to_catch =
  let rec do_rec r total_r = function
    | [] -> (r, total_r)
    | { provenance = mat; exit = i; vars; pm } :: rem -> (
        try
          let ctx = Context.select_columns mat ctx in
          let handler_i, total_i = compile_fun ctx pm in
          match raw_action r with
          | Lstaticraise (j, args) ->
              if i = j then
                ( List.fold_right2
                    (bind_with_value_kind Alias)
                    vars args handler_i,
                  Jumps.map (Context.rshift_num (ncols mat)) total_i )
              else
                do_rec r total_r rem
          | _ ->
              do_rec
                (Lstaticcatch (r, (i, vars), handler_i))
                (Jumps.union (Jumps.remove i total_r)
                   (Jumps.map (Context.rshift_num (ncols mat)) total_i))
                rem
        with Unused ->
          do_rec (Lstaticcatch (r, (i, vars), lambda_unit)) total_r rem
      )
  in
  do_rec lambda1 total1 to_catch

let compile_test compile_fun partial divide combine ctx to_match =
  let division = divide ctx to_match in
  let c_div = compile_list compile_fun division.cells in
  match c_div with
  | [], _, _ -> (
      match mk_failaction_neg partial ctx to_match.default with
      | None, _ -> raise Unused
      | Some l, total -> (l, total)
    )
  | _ -> combine ctx to_match.default c_div

(* Attempt to avoid some useless bindings by lowering them *)

(* Approximation of v present in lam *)
let rec approx_present v = function
  | Lconst _ -> false
  | Lstaticraise (_, args) ->
      List.exists (fun lam -> approx_present v lam) args
  | Lprim (_, args, _) -> List.exists (fun lam -> approx_present v lam) args
  | Llet (Alias, _k, _, l1, l2) -> approx_present v l1 || approx_present v l2
  | Lvar vv -> Ident.same v vv
  | _ -> true

let rec lower_bind v arg lam =
  match lam with
  | Lifthenelse (cond, ifso, ifnot) -> (
      let pcond = approx_present v cond
      and pso = approx_present v ifso
      and pnot = approx_present v ifnot in
      match (pcond, pso, pnot) with
      | false, false, false -> lam
      | false, true, false -> Lifthenelse (cond, lower_bind v arg ifso, ifnot)
      | false, false, true -> Lifthenelse (cond, ifso, lower_bind v arg ifnot)
      | _, _, _ -> bind Alias v arg lam
    )
  | Lswitch (ls, ({ sw_consts = [ (i, act) ]; sw_blocks = [] } as sw), loc)
    when not (approx_present v ls) ->
      Lswitch (ls, { sw with sw_consts = [ (i, lower_bind v arg act) ] }, loc)
  | Lswitch (ls, ({ sw_consts = []; sw_blocks = [ (i, act) ] } as sw), loc)
    when not (approx_present v ls) ->
      Lswitch (ls, { sw with sw_blocks = [ (i, lower_bind v arg act) ] }, loc)
  | Llet (Alias, k, vv, lv, l) ->
      if approx_present v lv then
        bind Alias v arg lam
      else
        Llet (Alias, k, vv, lv, lower_bind v arg l)
  | _ -> bind Alias v arg lam

let bind_check str v arg lam =
  match (str, arg) with
  | _, Lvar _ -> bind str v arg lam
  | Alias, _ -> lower_bind v arg lam
  | _, _ -> bind str v arg lam

let comp_exit ctx m =
  match Default_environment.pop m.default with
  | Some ((_, i), _) -> (Lstaticraise (i, []), Jumps.singleton i ctx)
  | None -> fatal_error "Matching.comp_exit"

let rec comp_match_handlers comp_fun partial ctx first_match next_matchs =
  match next_matchs with
  | [] -> comp_fun partial ctx first_match
  | rem -> (
      let rec c_rec body total_body = function
        | [] -> (body, total_body)
        (* Hum, -1 means never taken
        | (-1,pm)::rem -> c_rec body total_body rem *)
        | (i, pm) :: rem -> (
            let ctx_i, total_rem = Jumps.extract i total_body in
            if Context.is_empty ctx_i then
              c_rec body total_body rem
            else
              try
                let li, total_i =
                  comp_fun
                    ( match rem with
                    | [] -> partial
                    | _ -> Partial
                    )
                    ctx_i pm
                in
                c_rec
                  (Lstaticcatch (body, (i, []), li))
                  (Jumps.union total_i total_rem)
                  rem
              with Unused ->
                c_rec (Lstaticcatch (body, (i, []), lambda_unit)) total_rem rem
          )
      in
      try
        let first_lam, total = comp_fun Partial ctx first_match in
        c_rec first_lam total rem
      with Unused -> (
        match next_matchs with
        | [] -> raise Unused
        | (_, x) :: xs -> comp_match_handlers comp_fun partial ctx x xs
      )
    )

(* To find reasonable names for variables *)

let rec name_pattern default = function
  | (pat :: _, _) :: rem -> (
      match pat.pat_desc with
      | Tpat_var (id, _) -> id
      | Tpat_alias (_, id, _) -> id
      | _ -> name_pattern default rem
    )
  | _ -> Ident.create_local default

let arg_to_var arg cls =
  match arg with
  | Lvar v -> (v, arg)
  | _ ->
      let v = name_pattern "*match*" cls in
      (v, Lvar v)

(*
  The main compilation function.
   Input:
      repr=used for inserting debug events
      partial=exhaustiveness information from Parmatch
      ctx=a context
      m=a pattern matching

   Output: a lambda term, a jump summary {..., exit number -> context, .. }
*)

let rec compile_match repr partial ctx (m : pattern_matching) =
  match m with
  | { cases = []; args = [] } -> comp_exit ctx m
  | { cases = ([], action) :: rem } ->
      if is_guarded action then
        let lambda, total =
          compile_match None partial ctx { m with cases = rem }
        in
        (event_branch repr (patch_guarded lambda action), total)
      else
        (event_branch repr action, Jumps.empty)
  | { args = (arg, str) :: argl } ->
      let v, newarg = arg_to_var arg m.cases in
      let first_match, rem =
        split_and_precompile (Some v) { m with args = (newarg, Alias) :: argl }
      in
      let lam, total =
        comp_match_handlers
          (( if dbg then
             do_compile_matching_pr
           else
             do_compile_matching
           )
             repr)
          partial ctx first_match rem
      in
      (bind_check str v arg lam, total)
  | _ -> assert false

(* verbose version of do_compile_matching, for debug *)
and do_compile_matching_pr repr partial ctx x =
  Format.eprintf "COMPILE: %s\nMATCH\n"
    ( match partial with
    | Partial -> "Partial"
    | Total -> "Total"
    );
  pretty_precompiled x;
  Format.eprintf "CTX\n";
  Context.eprintf ctx;
  let ((_, jumps) as r) = do_compile_matching repr partial ctx x in
  Format.eprintf "JUMPS\n";
  Jumps.eprintf jumps;
  r

and do_compile_matching repr partial ctx pmh =
  match pmh with
  | Pm pm -> (
      let arg =
        match pm.args with
        | (first_arg, _) :: _ -> first_arg
        | _ ->
            (* We arrive in do_compile_matching from:
               - compile_matching
               - recursive call on PmVars
               The first one explicitly checks that [args] is nonempty, the
               second one is only generated when the inner pm first looks at
               a variable (i.e. there is something to look at).
            *)
            assert false
      in
      let pat = what_is_cases pm.cases in
      match pat.pat_desc with
      | Tpat_any ->
          compile_no_test divide_var Context.rshift repr partial ctx pm
      | Tpat_tuple patl ->
          compile_no_test
            (divide_tuple (List.length patl) (normalize_pat pat))
            Context.combine repr partial ctx pm
      | Tpat_record ((_, lbl, _) :: _, _) ->
          compile_no_test
            (divide_record lbl.lbl_all (normalize_pat pat))
            Context.combine repr partial ctx pm
      | Tpat_constant cst ->
          compile_test
            (compile_match repr partial)
            partial divide_constant
            (combine_constant pat.pat_loc arg cst partial)
            ctx pm
      | Tpat_construct (_, cstr, _) ->
          compile_test
            (compile_match repr partial)
            partial divide_constructor
            (combine_constructor pat.pat_loc arg pat cstr partial)
            ctx pm
      | Tpat_array _ ->
          let kind = Typeopt.array_pattern_kind pat in
          compile_test
            (compile_match repr partial)
            partial (divide_array kind)
            (combine_array pat.pat_loc arg kind partial)
            ctx pm
      | Tpat_lazy _ ->
          compile_no_test
            (divide_lazy (normalize_pat pat))
            Context.combine repr partial ctx pm
      | Tpat_variant (_, _, row) ->
          compile_test
            (compile_match repr partial)
            partial (divide_variant !row)
            (combine_variant pat.pat_loc !row arg partial)
            ctx pm
      | _ -> assert false
    )
  | PmVar { inside = pmh } ->
      let lam, total =
        do_compile_matching repr partial (Context.lshift ctx) pmh
      in
      (lam, Jumps.map Context.rshift total)
  | PmOr { body; handlers } ->
      let lam, total = compile_match repr partial ctx body in
      compile_orhandlers (compile_match repr partial) lam total ctx handlers

and compile_no_test divide up_ctx repr partial ctx to_match =
  let { pm = this_match; ctx = this_ctx } = divide ctx to_match in
  let lambda, total = compile_match repr partial this_ctx this_match in
  (lambda, Jumps.map up_ctx total)

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

let is_lazy_pat p = match p.pat_desc with
  | Tpat_lazy _ -> true
  | Tpat_alias _
  | Tpat_variant _
  | Tpat_record _
  | Tpat_tuple _
  | Tpat_construct _
  | Tpat_array _
  | Tpat_or _
  | Tpat_constant _
  | Tpat_var _
  | Tpat_any ->
      false
  | Tpat_exception _ -> assert false

let has_lazy p =
  Typedtree.exists_pattern is_lazy_pat p

let is_record_with_mutable_field p =
  match p.pat_desc with
  | Tpat_record (lps, _) ->
      List.exists
        (fun (_, lbl, _) ->
          match lbl.Types.lbl_mut with
          | Mutable -> true
          | Immutable -> false)
        lps
  | Tpat_alias _
  | Tpat_variant _
  | Tpat_lazy _
  | Tpat_tuple _
  | Tpat_construct _
  | Tpat_array _
  | Tpat_or _
  | Tpat_constant _
  | Tpat_var _
  | Tpat_any ->
      false
  | Tpat_exception _ -> assert false

let has_mutable p =
  Typedtree.exists_pattern is_record_with_mutable_field p

(* Downgrade Total when
   1. Matching accesses some mutable fields;
   2. And there are  guards or lazy patterns.
*)

let check_partial has_mutable has_lazy pat_act_list = function
  | Partial -> Partial
  | Total ->
      if
        pat_act_list = []
        || (* allow empty case list *)
           List.exists
             (fun (pats, lam) ->
               has_mutable pats && (is_guarded lam || has_lazy pats))
             pat_act_list
      then
        Partial
      else
        Total

let check_partial_list =
  check_partial (List.exists has_mutable) (List.exists has_lazy)

let check_partial = check_partial has_mutable has_lazy

(* have toplevel handler when appropriate *)

let check_total total lambda i handler_fun =
  if Jumps.is_empty total then
    lambda
  else
    Lstaticcatch (lambda, (i, []), handler_fun ())

let compile_matching repr handler_fun arg pat_act_list partial =
  let partial = check_partial pat_act_list partial in
  match partial with
  | Partial -> (
      let raise_num = next_raise_count () in
      let pm =
        { cases = List.map (fun (pat, act) -> ([ pat ], act)) pat_act_list;
          args = [ (arg, Strict) ];
          default = Default_environment.(cons [ [ omega ] ] raise_num empty)
        }
      in
      try
        let lambda, total = compile_match repr partial (Context.start 1) pm in
        check_total total lambda raise_num handler_fun
      with Unused -> assert false
      (* ; handler_fun() *)
    )
  | Total ->
      let pm =
        { cases = List.map (fun (pat, act) -> ([ pat ], act)) pat_act_list;
          args = [ (arg, Strict) ];
          default = Default_environment.empty
        }
      in
      let lambda, total = compile_match repr partial (Context.start 1) pm in
      assert (Jumps.is_empty total);
      lambda

let partial_function loc () =
  let slot =
    transl_extension_path loc Env.initial_safe_string Predef.path_match_failure
  in
  let fname, line, char = Location.get_pos_info loc.Location.loc_start in
  Lprim
    ( Praise Raise_regular,
      [ Lprim
          ( Pmakeblock (0, Immutable, None),
            [ slot;
              Lconst
                (Const_block
                   ( 0,
                     [ Const_base (Const_string (fname, None));
                       Const_base (Const_int line);
                       Const_base (Const_int char)
                     ] ))
            ],
            loc )
      ],
      loc )

let for_function loc repr param pat_act_list partial =
  compile_matching repr (partial_function loc) param pat_act_list partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith param pat_act_list =
  compile_matching None
    (fun () -> Lprim (Praise Raise_reraise, [ param ], Location.none))
    param pat_act_list Partial

let simple_for_let loc param pat body =
  compile_matching None (partial_function loc) param [ (pat, body) ] Partial

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

let rec map_return f = function
  | Llet (str, k, id, l1, l2) -> Llet (str, k, id, l1, map_return f l2)
  | Lletrec (l1, l2) -> Lletrec (l1, map_return f l2)
  | Lifthenelse (lcond, lthen, lelse) ->
      Lifthenelse (lcond, map_return f lthen, map_return f lelse)
  | Lsequence (l1, l2) -> Lsequence (l1, map_return f l2)
  | Levent (l, ev) -> Levent (map_return f l, ev)
  | Ltrywith (l1, id, l2) -> Ltrywith (map_return f l1, id, map_return f l2)
  | Lstaticcatch (l1, b, l2) ->
      Lstaticcatch (map_return f l1, b, map_return f l2)
  | (Lstaticraise _ | Lprim (Praise _, _, _)) as l -> l
  | l -> f l

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

let assign_pat opt nraise catch_ids loc pat lam =
  let rec collect acc pat lam =
    match (pat.pat_desc, lam) with
    | Tpat_tuple patl, Lprim (Pmakeblock _, lams, _) ->
        opt := true;
        List.fold_left2 collect acc patl lams
    | Tpat_tuple patl, Lconst (Const_block (_, scl)) ->
        opt := true;
        let collect_const acc pat sc = collect acc pat (Lconst sc) in
        List.fold_left2 collect_const acc patl scl
    | _ ->
        (* pattern idents will be bound in staticcatch (let body), so we
       refresh them here to guarantee binders  uniqueness *)
        let pat_ids = pat_bound_idents pat in
        let fresh_ids = List.map (fun id -> (id, Ident.rename id)) pat_ids in
        (fresh_ids, alpha_pat fresh_ids pat, lam) :: acc
  in
  (* sublets were accumulated by 'collect' with the leftmost tuple
     pattern at the bottom of the list; to respect right-to-left
     evaluation order for tuples, we must evaluate sublets
     top-to-bottom. To preserve tail-rec, we will fold_left the
     reversed list. *)
  let rev_sublets = List.rev (collect [] pat lam) in
  let exit =
    (* build an Ident.tbl to avoid quadratic refreshing costs *)
    let add t (id, fresh_id) = Ident.add id fresh_id t in
    let add_ids acc (ids, _pat, _lam) = List.fold_left add acc ids in
    let tbl = List.fold_left add_ids Ident.empty rev_sublets in
    let fresh_var id = Lvar (Ident.find_same id tbl) in
    Lstaticraise (nraise, List.map fresh_var catch_ids)
  in
  let push_sublet code (_ids, pat, lam) = simple_for_let loc lam pat code in
  List.fold_left push_sublet exit rev_sublets

let for_let loc param pat body =
  match pat.pat_desc with
  | Tpat_any ->
      (* This eliminates a useless variable (and stack slot in bytecode)
         for "let _ = ...". See #6865. *)
      Lsequence (param, body)
  | Tpat_var (id, _) ->
      (* fast path, and keep track of simple bindings to unboxable numbers *)
      let k = Typeopt.value_kind pat.pat_env pat.pat_type in
      Llet (Strict, k, id, param, body)
  | _ ->
      let opt = ref false in
      let nraise = next_raise_count () in
      let catch_ids = pat_bound_idents_full pat in
      let ids_with_kinds =
        List.map
          (fun (id, _, typ) -> (id, Typeopt.value_kind pat.pat_env typ))
          catch_ids
      in
      let ids = List.map (fun (id, _, _) -> id) catch_ids in
      let bind = map_return (assign_pat opt nraise ids loc pat) param in
      if !opt then
        Lstaticcatch (bind, (nraise, ids_with_kinds), body)
      else
        simple_for_let loc param pat body

(* Handling of tupled functions and matchings *)

(* Easy case since variables are available *)
let for_tupled_function loc paraml pats_act_list partial =
  let partial = check_partial_list pats_act_list partial in
  let raise_num = next_raise_count () in
  let omegas = [ List.map (fun _ -> omega) paraml ] in
  let pm =
    { cases = pats_act_list;
      args = List.map (fun id -> (Lvar id, Strict)) paraml;
      default = Default_environment.(cons omegas raise_num empty)
    }
  in
  try
    let lambda, total =
      compile_match None partial (Context.start (List.length paraml)) pm
    in
    check_total total lambda raise_num (partial_function loc)
  with Unused -> partial_function loc ()

let flatten_pattern size p =
  match p.pat_desc with
  | Tpat_tuple args -> args
  | Tpat_any -> omegas size
  | _ -> raise Cannot_flatten

let flatten_cases size cases =
  List.map
    (fun (ps, action) ->
      match ps with
      | [ p ] -> (flatten_pattern size p, action)
      | _ -> fatal_error "Matching.flatten_case")
    cases

let flatten_pm size args pm =
  { args;
    cases = flatten_cases size pm.cases;
    default = Default_environment.flatten size pm.default
  }

let flatten_handler size handler =
  { handler with provenance = flatten_matrix size handler.provenance }

let flatten_precompiled size args pmh =
  match pmh with
  | Pm pm -> Pm (flatten_pm size args pm)
  | PmOr { body = b; handlers = hs; or_matrix = m } ->
      PmOr
        { body = flatten_pm size args b;
          handlers = List.map (flatten_handler size) hs;
          or_matrix = flatten_matrix size m
        }
  | PmVar _ -> assert false

(*
   compiled_flattened is a ``comp_fun'' argument to comp_match_handlers.
   Hence it needs a fourth argument, which it ignores
*)

let compile_flattened repr partial ctx pmh =
  match pmh with
  | Pm pm -> compile_match repr partial ctx pm
  | PmOr { body = b; handlers = hs } ->
      let lam, total = compile_match repr partial ctx b in
      compile_orhandlers (compile_match repr partial) lam total ctx hs
  | PmVar _ -> assert false

let do_for_multiple_match loc paraml pat_act_list partial =
  let repr = None in
  let partial = check_partial pat_act_list partial in
  let raise_num, pm1 =
    let raise_num, default =
      match partial with
      | Partial ->
          let raise_num = next_raise_count () in
          (raise_num, Default_environment.(cons [ [ omega ] ] raise_num empty))
      | Total -> (-1, Default_environment.empty)
    in
    ( raise_num,
      { cases = List.map (fun (pat, act) -> ([ pat ], act)) pat_act_list;
        args =
          [ (Lprim (Pmakeblock (0, Immutable, None), paraml, loc), Strict) ];
        default
      } )
  in
  try
    try
      (* Once for checking that compilation is possible *)
      let next, nexts = split_and_precompile None pm1 in
      let size = List.length paraml
      and idl = List.map (fun _ -> Ident.create_local "*match*") paraml in
      let args = List.map (fun id -> (Lvar id, Alias)) idl in
      let flat_next = flatten_precompiled size args next
      and flat_nexts =
        List.map (fun (e, pm) -> (e, flatten_precompiled size args pm)) nexts
      in
      let lam, total =
        comp_match_handlers (compile_flattened repr) partial
          (Context.start size) flat_next flat_nexts
      in
      List.fold_right2 (bind Strict) idl paraml
        ( match partial with
        | Partial -> check_total total lam raise_num (partial_function loc)
        | Total ->
            assert (Jumps.is_empty total);
            lam
        )
    with Cannot_flatten -> (
      let lambda, total = compile_match None partial (Context.start 1) pm1 in
      match partial with
      | Partial -> check_total total lambda raise_num (partial_function loc)
      | Total ->
          assert (Jumps.is_empty total);
          lambda
    )
  with Unused -> assert false

(* ; partial_function loc () *)

(* PR#4828: Believe it or not, the 'paraml' argument below
   may not be side effect free. *)

let param_to_var param =
  match param with
  | Lvar v -> (v, None)
  | _ -> (Ident.create_local "*match*", Some param)

let bind_opt (v, eo) k =
  match eo with
  | None -> k
  | Some e -> Lambda.bind Strict v e k

let for_multiple_match loc paraml pat_act_list partial =
  let v_paraml = List.map param_to_var paraml in
  let paraml = List.map (fun (v, _) -> Lvar v) v_paraml in
  List.fold_right bind_opt v_paraml
    (do_for_multiple_match loc paraml pat_act_list partial)
