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

module Scoped_location = Debuginfo.Scoped_location

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
  | [] -> fatal_error "Matching.all_record_args"
  | (_, { lbl_all }, _) :: _ ->
      let t =
        Array.map
          (fun lbl ->
            (mknoloc (Longident.Lident "?temp?"), lbl, Patterns.omega))
          lbl_all
      in
      List.iter (fun ((_, lbl, _) as x) -> t.(lbl.lbl_pos) <- x) lbls;
      Array.to_list t

let expand_record_head h =
  let open Patterns.Head in
  match h.pat_desc with
  | Record [] -> fatal_error "Matching.expand_record_head"
  | Record ({ lbl_all } :: _) ->
      { h with pat_desc = Record (Array.to_list lbl_all) }
  | _ -> h

let bind_alias p id ~arg ~action =
  let k = Typeopt.value_kind p.pat_env p.pat_type in
  bind_with_value_kind Alias (id, k) arg action

let head_loc ~scopes head =
  Scoped_location.of_location ~scopes head.pat_loc

type 'a clause = 'a * lambda

let map_on_row f (row, action) = (f row, action)

let map_on_rows f = List.map (map_on_row f)

module Non_empty_row = Patterns.Non_empty_row

module General = struct
  include Patterns.General

  type nonrec clause = pattern Non_empty_row.t clause
end

module Half_simple : sig
  include module type of Patterns.Half_simple
  (** Half-simplified patterns are patterns where:
        - records are expanded so that they possess all fields
        - aliases are removed and replaced by bindings in actions.

      Or-patterns are not removed, they are only "half-simplified":
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

      In particular, or-patterns may still occur in the leading column,
      so this is only a "half-simplification". *)

  type nonrec clause = pattern Non_empty_row.t clause

  val of_clause : arg:lambda -> General.clause -> clause
end = struct
  include Patterns.Half_simple

  type nonrec clause = pattern Non_empty_row.t clause

  let rec simpl_under_orpat p =
    match p.pat_desc with
    | Tpat_any
    | Tpat_var _ ->
        p
    | Tpat_alias (q, id, s) ->
        { p with pat_desc = Tpat_alias (simpl_under_orpat q, id, s) }
    | Tpat_or (p1, p2, o) ->
        let p1, p2 = (simpl_under_orpat p1, simpl_under_orpat p2) in
        if le_pat p1 p2 then
          p1
        else
          { p with pat_desc = Tpat_or (p1, p2, o) }
    | Tpat_record (lbls, closed) ->
        let all_lbls = all_record_args lbls in
        { p with pat_desc = Tpat_record (all_lbls, closed) }
    | _ -> p

  (* Explode or-patterns and turn aliases into bindings in actions *)
  let of_clause ~arg cl =
    let rec aux (((p, patl), action) : General.clause) : clause =
      let continue p (view : General.view) : clause =
        aux (({ p with pat_desc = view }, patl), action)
      in
      let stop p (view : view) : clause =
        (({ p with pat_desc = view }, patl), action)
      in
      match p.pat_desc with
      | `Any -> stop p `Any
      | `Var (id, s) -> continue p (`Alias (Patterns.omega, id, s))
      | `Alias (p, id, _) ->
          aux
            ( (General.view p, patl),
              bind_alias p id ~arg ~action )
      | `Record ([], _) as view -> stop p view
      | `Record (lbls, closed) ->
          let full_view = `Record (all_record_args lbls, closed) in
          stop p full_view
      | `Or _ -> (
          let orpat = General.view (simpl_under_orpat (General.erase p)) in
          match orpat.pat_desc with
          | `Or _ as or_view -> stop orpat or_view
          | other_view -> continue orpat other_view
        )
      | ( `Constant _ | `Tuple _ | `Construct _ | `Variant _ | `Array _
        | `Lazy _ ) as view ->
          stop p view
    in
    aux cl
end

exception Cannot_flatten

module Simple : sig
  include module type of Patterns.Simple

  type nonrec clause = pattern Non_empty_row.t clause

  val head : pattern -> Patterns.Head.t

  val explode_or_pat :
    arg:lambda ->
    Half_simple.pattern ->
    mk_action:(vars:Ident.t list -> lambda) ->
    patbound_action_vars:Ident.t list ->
    (pattern * lambda) list
end = struct
  include Patterns.Simple

  type nonrec clause = pattern Non_empty_row.t clause

  let head p = fst (Patterns.Head.deconstruct p)

  let alpha env (p : pattern) : pattern =
    let alpha_pat env p = Typedtree.alpha_pat env p in
    let pat_desc =
      match p.pat_desc with
      | `Any -> `Any
      | `Constant cst -> `Constant cst
      | `Tuple ps -> `Tuple (List.map (alpha_pat env) ps)
      | `Construct (cstr, cst_descr, args) ->
          `Construct (cstr, cst_descr, List.map (alpha_pat env) args)
      | `Variant (cstr, argo, row_desc) ->
          `Variant (cstr, Option.map (alpha_pat env) argo, row_desc)
      | `Record (fields, closed) ->
          let alpha_field env (lid, l, p) = (lid, l, alpha_pat env p) in
          `Record (List.map (alpha_field env) fields, closed)
      | `Array ps -> `Array (List.map (alpha_pat env) ps)
      | `Lazy p -> `Lazy (alpha_pat env p)
    in
    { p with pat_desc }

  (* Consider the following matching problem involving a half-simple pattern,
     with an or-pattern and as-patterns below it:

       match arg, other-args with
       | (Foo(y, z) as x | Bar(x, y) as z), other-pats -> action[x,y,z]

     (action[x,y,z] is some right-hand-side expression using x, y and z,
      but we assume that it uses no variables from [other-pats]).

     [explode_or_pat] explodes this into the following:

       match arg, other-args with
       | Foo(y1, z1), other-pats -> let x1 = arg in action[x1,y1,z1]
       | Bar(x2, y2), other-pats -> let z2 = arg in action[x2,y2,z2]

     notice that the binding occurrences of x,y,z are alpha-renamed with
     fresh variables x1,y1,z1 and x2,y2,z2.

     We assume that it is fine to duplicate the argument [arg] in each
     exploded branch; in most cases it is a variable (in which case
     the bindings [let x1 = arg] are inlined on the fly), except when
     compiling in [do_for_multiple_match] where it is a tuple of
     variables.
  *)
  let explode_or_pat ~arg (p : Half_simple.pattern)
        ~mk_action ~patbound_action_vars
    : (pattern * lambda) list =
    let rec explode p aliases rem =
      let split_explode p aliases rem = explode (General.view p) aliases rem in
      match p.pat_desc with
      | `Or (p1, p2, _) ->
          split_explode p1 aliases (split_explode p2 aliases rem)
      | `Alias (p, id, _) -> split_explode p (id :: aliases) rem
      | `Var (id, str) ->
          explode
            { p with pat_desc = `Alias (Patterns.omega, id, str) }
            aliases rem
      | #view as view ->
          (* We are doing two things here:
             - we freshen the variables of the pattern, to
               avoid reusing the same identifier in distinct exploded
               branches
             - we bind the variables in [aliases] to the argument [arg]
               (the other variables are bound by [view]); to avoid
               code duplication if [arg] is itself not a variable, we
               generate a binding for it, but only if the binding is
               needed.

             We are careful to avoid binding [arg] if not needed due
             to the {!do_for_multiple_match} usage, which tries to
             compile a tuple pattern [match e1, .. en with ...]
             without allocating the tuple [(e1, .., en)].
          *)
          let rec fresh_clause arg_id action_vars renaming_env = function
            | [] ->
                let fresh_pat = alpha renaming_env { p with pat_desc = view } in
                let fresh_action = mk_action ~vars:(List.rev action_vars) in
                (fresh_pat, fresh_action)
            | pat_id :: rem_vars ->
              if not (List.mem pat_id aliases) then begin
                let fresh_id = Ident.rename pat_id in
                let action_vars = fresh_id :: action_vars in
                let renaming_env = ((pat_id, fresh_id) :: renaming_env) in
                fresh_clause arg_id action_vars renaming_env rem_vars
              end else begin match arg_id, arg with
                | Some id, _
                | None, Lvar id ->
                  let action_vars = id :: action_vars in
                  fresh_clause arg_id action_vars renaming_env rem_vars
                | None, _ ->
                  (* [pat_id] is a name used locally to refer to the argument,
                     so it makes sense to reuse it (refreshed) *)
                  let id = Ident.rename pat_id in
                  let action_vars = (id :: action_vars) in
                  let pat, action =
                    fresh_clause (Some id) action_vars renaming_env rem_vars
                  in
                  pat, bind_alias pat id ~arg ~action
              end
          in
          fresh_clause None [] [] patbound_action_vars :: rem
    in
    explode (p : Half_simple.pattern :> General.pattern) [] []
end

let expand_record_simple : Simple.pattern -> Simple.pattern =
 fun p ->
  match p.pat_desc with
  | `Record (l, _) -> { p with pat_desc = `Record (all_record_args l, Closed) }
  | _ -> p

type initial_clause = pattern list clause

type matrix = pattern list list

let add_omega_column pss = List.map (fun ps -> Patterns.omega :: ps) pss

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

let matcher discr (p : Simple.pattern) rem =
  let discr = expand_record_head discr in
  let p = expand_record_simple p in
  let omegas = Patterns.(omegas (Head.arity discr)) in
  let ph, args = Patterns.Head.deconstruct p in
  let yes () = args @ rem in
  let no () = raise NoMatch in
  let yesif b =
    if b then
      yes ()
    else
      no ()
  in
  let open Patterns.Head in
  match (discr.pat_desc, ph.pat_desc) with
  | Any, _ -> rem
  | ( ( Constant _ | Construct _ | Variant _ | Lazy | Array _ | Record _
      | Tuple _ ),
      Any ) ->
      omegas @ rem
  | Constant cst, Constant cst' -> yesif (const_compare cst cst' = 0)
  | Constant _, (Construct _ | Variant _ | Lazy | Array _ | Record _ | Tuple _)
    ->
      no ()
  | Construct cstr, Construct cstr' ->
      (* NB: may_equal_constr considers (potential) constructor rebinding;
          Types.may_equal_constr does check that the arities are the same,
          preserving row-size coherence. *)
      yesif (Types.may_equal_constr cstr cstr')
  | Construct _, (Constant _ | Variant _ | Lazy | Array _ | Record _ | Tuple _)
    ->
      no ()
  | Variant { tag; has_arg }, Variant { tag = tag'; has_arg = has_arg' } ->
      yesif (tag = tag' && has_arg = has_arg')
  | Variant _, (Constant _ | Construct _ | Lazy | Array _ | Record _ | Tuple _)
    ->
      no ()
  | Array n1, Array n2 -> yesif (n1 = n2)
  | Array _, (Constant _ | Construct _ | Variant _ | Lazy | Record _ | Tuple _)
    ->
      no ()
  | Tuple n1, Tuple n2 -> yesif (n1 = n2)
  | Tuple _, (Constant _ | Construct _ | Variant _ | Lazy | Array _ | Record _)
    ->
      no ()
  | Record l, Record l' ->
      (* we already expanded the record fully *)
      yesif (List.length l = List.length l')
  | Record _, (Constant _ | Construct _ | Variant _ | Lazy | Array _ | Tuple _)
    ->
      no ()
  | Lazy, Lazy -> yes ()
  | Lazy, (Constant _ | Construct _ | Variant _ | Array _ | Record _ | Tuple _)
    ->
      no ()

let ncols = function
  | [] -> 0
  | ps :: _ -> List.length ps

module Context : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val start : int -> t

  val eprintf : t -> unit

  val specialize : Patterns.Head.t -> t -> t

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
      | _ :: xs -> { left = Patterns.omega :: left; right = xs }
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

  let start n : t = [ { left = []; right = Patterns.omegas n } ]

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

  let specialize head ctx =
    let non_empty = function
      | { Row.left = _; right = [] } ->
          fatal_error "Matching.Context.specialize"
      | { Row.left; right = p :: ps } -> (left, p, ps)
    in
    let ctx = List.map non_empty ctx in
    let rec filter_rec = function
      | [] -> []
      | (left, p, right) :: rem -> (
          let p = General.view p in
          match p.pat_desc with
          | `Or (p1, p2, _) ->
              filter_rec ((left, p1, right) :: (left, p2, right) :: rem)
          | `Alias (p, _, _) -> filter_rec ((left, p, right) :: rem)
          | `Var _ -> filter_rec ((left, Patterns.omega, right) :: rem)
          | #Simple.view as view -> (
              let p = { p with pat_desc = view } in
              match matcher head p right with
              | exception NoMatch -> filter_rec rem
              | right ->
                  let left = Patterns.Head.to_omega_pattern head :: left in
                  { Row.left; right }
                  :: filter_rec rem
            )
        )
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

let rec flatten_pat_line size p k =
  match p.pat_desc with
  | Tpat_any | Tpat_var _ -> Patterns.omegas size :: k
  | Tpat_tuple args -> args :: k
  | Tpat_or (p1, p2, _) ->
      flatten_pat_line size p1 (flatten_pat_line size p2 k)
  | Tpat_alias (p, _, _) ->
      (* Note: we are only called from flatten_matrix,
         which is itself only ever used in places
         where variables do not matter (default environments,
         "provenance", etc.). *)
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

  val specialize : Patterns.Head.t -> t -> t

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

  let specialize_matrix arity matcher pss =
    let rec filter_rec = function
      | [] -> []
      | (p, ps) :: rem -> (
          let p = General.view p in
          match p.pat_desc with
          | `Alias (p, _, _) -> filter_rec ((p, ps) :: rem)
          | `Var _ -> filter_rec ((Patterns.omega, ps) :: rem)
          | `Or (p1, p2, _) -> filter_rec_or p1 p2 ps rem
          | #Simple.view as view -> (
              let p = { p with pat_desc = view } in
              match matcher p ps with
              | exception NoMatch -> filter_rec rem
              | specialized ->
                  assert (List.length specialized = List.length ps + arity);
                  specialized :: filter_rec rem
            )
        )

    (* Filter just one row, without a `rem` accumulator
       of further rows to process.
       The following equality holds:
         filter_rec ((p :: ps) :: rem)
         = filter_one p ps @ filter_rec rem
    *)
    and filter_one p ps =
      filter_rec [ (p, ps) ]

    and filter_rec_or p1 p2 ps rem =
      match arity with
      | 0 -> (
          (* if K has arity 0, specializing ((K|K)::rem) returns just (rem):
             if either sides works (filters into a non-empty list),
             no need to keep the other. *)
          match filter_one p1 ps with
          | [] -> filter_rec ((p2, ps) :: rem)
          | matches -> matches @ filter_rec rem
        )
      | 1 -> (
          (* if K has arity 1, ((K p | K q) :: rem) can be expressed
             as ((p | q) :: rem): even if both sides of an or-pattern
             match, we can compress the output in a single row,
             instead of duplicating the row.

             In particular, filtering a single row (the filter_one calls)
             returns a result that respects the following properties:
             - "row count": the result is either an empty list or a single row
             - "row shape": if there is a row in the result, it contains one
               pattern consed to the tail [ps] of our input row; in particular
               the row is not empty. *)
          match (filter_one p1 ps, filter_one p2 ps) with
          | [], row
          | row, [] ->
              row @ filter_rec rem
          | [ (arg1 :: _) ], [ (arg2 :: _) ] ->
              (* By the row shape property,
                 the wildcard patterns can only be ps. *)
              (* The output below is a single row,
                  respecting the row count property. *)
              ({ arg1 with
                 pat_desc = Tpat_or (arg1, arg2, None);
                 pat_loc = Location.none
               }
              :: ps
              )
              :: filter_rec rem
          | (_ :: _ :: _), _
          | _, (_ :: _ :: _) ->
              (* Cannot happen from the row count property. *)
              assert false
          | [ [] ], _
          | _, [ [] ] ->
              (* Cannot happen from the row shape property. *)
              assert false
        )
      | _ ->
          (* we cannot preserve the or-pattern as in the arity-1 case,
             because we cannot express
                (K (p1, .., pn) | K (q1, .. qn))
             as (p1 .. pn | q1 .. qn) *)
          filter_rec ((p1, ps) :: (p2, ps) :: rem)
    in
    filter_rec pss

  let specialize_ arity matcher env =
    let rec make_rec = function
      | [] -> []
      | (([] :: _), i) :: _ -> [ ([ [] ], i) ]
      | (pss, i) :: rem -> (
          (* we already handled the empty-row case
             so we know that all rows in pss are non-empty *)
          let non_empty = function
            | [] -> assert false
            | p :: ps -> (p, ps)
          in
          let pss = List.map non_empty pss in
          match specialize_matrix arity matcher pss with
          | [] -> make_rec rem
          | [] :: _ -> [ ([ [] ], i) ]
          | pss -> (pss, i) :: make_rec rem
        )
    in
    make_rec env

  let specialize head def =
    specialize_ (Patterns.Head.arity head) (matcher head) def

  let pop_column def = specialize_ 0 (fun _p rem -> rem) def

  let pop_compat p def =
    let compat_matcher q rem =
      if may_compat p (General.erase q) then
        rem
      else
        raise NoMatch
    in
    specialize_ 0 compat_matcher def

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

type 'row pattern_matching = {
  mutable cases : 'row list;
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
  pm : initial_clause pattern_matching
}

type ('head_pat, 'matrix) pm_or_compiled = {
  body : 'head_pat Non_empty_row.t clause pattern_matching;
  handlers : handler list;
  or_matrix : 'matrix
}

(* Pattern matching after application of both the or-pat rule and the
   mixture rule *)

type pm_half_compiled =
  | PmOr of (Simple.pattern, matrix) pm_or_compiled
  | PmVar of { inside : pm_half_compiled }
  | Pm of Simple.clause pattern_matching

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

let erase_cases f cases =
  List.map (fun ((p, ps), act) -> (f p :: ps, act)) cases

let erase_pm pm =
  { pm with cases = erase_cases General.erase pm.cases }

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
      pretty_pm (erase_pm pm)
  | PmVar x ->
      Format.eprintf "++++ VAR ++++\n";
      pretty_precompiled x.inside
  | PmOr x ->
      Format.eprintf "++++ OR ++++\n";
      pretty_pm (erase_pm x.body);
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

let safe_before ((p, ps), act_p) l =
  (* Test for swapping two clauses *)
  let same_actions act1 act2 =
    match (make_key act1, make_key act2) with
    | Some key1, Some key2 -> key1 = key2
    | None, _
    | _, None ->
        false
  in
  List.for_all
    (fun ((q, qs), act_q) ->
      same_actions act_p act_q
      || not (may_compats (General.erase p :: ps) (General.erase q :: qs)))
    l

let half_simplify_nonempty ~arg (cls : Typedtree.pattern Non_empty_row.t clause)
  : Half_simple.clause =
  cls
  |> map_on_row (Non_empty_row.map_first General.view)
  |> Half_simple.of_clause ~arg

let half_simplify_clause ~arg (cls : Typedtree.pattern list clause) =
  cls
  |> map_on_row Non_empty_row.of_initial
  |> half_simplify_nonempty ~arg

(* Once matchings are *fully* simplified, one can easily find
   their nature. *)

let rec what_is_cases ~skip_any cases =
  match cases with
  | [] -> Patterns.Head.omega
  | ((p, _), _) :: rem -> (
      let head = Simple.head p in
      match head.pat_desc with
      | Patterns.Head.Any when skip_any -> what_is_cases ~skip_any rem
      | _ -> head
    )

let what_is_first_case = what_is_cases ~skip_any:false

let what_is_cases = what_is_cases ~skip_any:true

let pm_free_variables { cases } =
  List.fold_right
    (fun (_, act) r -> Ident.Set.union (free_variables act) r)
    cases Ident.Set.empty

(* Basic grouping predicates *)

let can_group discr pat =
  let open Patterns.Head in
  match (discr.pat_desc, (Simple.head pat).pat_desc) with
  | Any, Any
  | Constant (Const_int _), Constant (Const_int _)
  | Constant (Const_char _), Constant (Const_char _)
  | Constant (Const_string _), Constant (Const_string _)
  | Constant (Const_float _), Constant (Const_float _)
  | Constant (Const_int32 _), Constant (Const_int32 _)
  | Constant (Const_int64 _), Constant (Const_int64 _)
  | Constant (Const_nativeint _), Constant (Const_nativeint _) ->
      true
  | Construct { cstr_tag = Cstr_extension _ as discr_tag }, Construct pat_cstr
    ->
      (* Extension constructors with distinct names may be equal thanks to
         constructor rebinding. So we need to produce a specialized
         submatrix for each syntactically-distinct constructor (with a threading
         of exits such that each submatrix falls back to the
         potentially-compatible submatrices below it).  *)
      Types.equal_tag discr_tag pat_cstr.cstr_tag
  | Construct _, Construct _
  | Tuple _, (Tuple _ | Any)
  | Record _, (Record _ | Any)
  | Array _, Array _
  | Variant _, Variant _
  | Lazy, Lazy ->
      true
  | ( _,
      ( Any
      | Constant
          ( Const_int _ | Const_char _ | Const_string _ | Const_float _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
      | Construct _ | Tuple _ | Record _ | Array _ | Variant _ | Lazy ) ) ->
      false

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

let simple_omega_like p =
  match (Simple.head p).pat_desc with
  | Any -> true
  | _ -> false

let equiv_pat p q = le_pat p q && le_pat q p

let rec extract_equiv_head p l =
  match l with
  | (((q, _), _) as cl) :: rem ->
      if equiv_pat p (General.erase q) then
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
      (fun ((p, ps), act_p) ->
        let p = General.erase p in
        match p.pat_desc with
        | Tpat_or _ -> disjoint p q || safe_below (ps, act_p) qs
        | _ -> true)
      l

  (* Insert or append a clause in the Or matrix:
     - insert: adding the clause in the middle of the or_matrix
     - append: adding the clause at the bottom of the or_matrix

     If neither are possible we add to the bottom of the No matrix.
   *)
  let insert_or_append (head, ps, act) rev_ors rev_no =
    let safe_to_insert rem (p, ps) seen =
      let _, not_e = extract_equiv_head p rem in
      (* check append condition for head of O *)
      safe_below_or_matrix not_e (p, ps)
      && (* check insert condition for tail of O *)
         List.for_all (fun ((q, _), _) -> disjoint p (General.erase q)) seen
    in
    let rec attempt seen = function
      (* invariant: the new clause is safe to append at the end of
         [seen] (but maybe not [rem] yet) *)
      | [] -> (((head, ps), act) :: rev_ors, rev_no)
      | (((q, qs), act_q) as cl) :: rem ->
          let p = General.erase head in
          let q = General.erase q in
          if (not (is_or q)) || disjoint p q then
            attempt (cl :: seen) rem
          else if
            Typedtree.pat_bound_idents p = []
            && Typedtree.pat_bound_idents q = []
            && equiv_pat p q
          then
            (* attempt insertion, for equivalent orpats with no variables *)
            if safe_to_insert rem (p, ps) seen then
              (List.rev_append seen (((head, ps), act) :: cl :: rem), rev_no)
            else
              (* fail to insert or append *)
              (rev_ors, ((head, ps), act) :: rev_no)
          else if safe_below (qs, act_q) ps then
            attempt (cl :: seen) rem
          else
            (rev_ors, ((head, ps), act) :: rev_no)
    in
    attempt [] rev_ors
end

(* Reconstruct default information from half_compiled  pm list *)

let as_matrix cases =
  get_mins le_pats (List.map (fun ((p, ps), _) -> General.erase p :: ps) cases)

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
    in actions (cf. Half_simple.of_clause).

    Additionally, if the match argument is a variable, matchings whose
    first column is made of variables only are split further
    (cf. precompile_var).

  ---

  Note: we assume that the first column of each pattern is coherent -- all
  patterns match values of the same type. This comes from the fact that
  we make aggressive splitting decisions, splitting pattern heads that
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
  aggressively: submatrices will contain not just the arguments of
  a given pattern head, but also other lines that may be compatible with
  it, in particular those with a leftmost omega and those starting with
  an extension constructor that may be equal to it.

*)

let rec split_or ~arg (cls : Half_simple.clause list) args def =
  let rec do_split (rev_before : Simple.clause list) rev_ors rev_no = function
    | [] ->
        cons_next (List.rev rev_before) (List.rev rev_ors) (List.rev rev_no)
    | cl :: rem when not (safe_before cl rev_no) ->
        do_split rev_before rev_ors (cl :: rev_no) rem
    | (((p, ps), act) as cl) :: rem -> (
        match p.pat_desc with
        | #Simple.view as view when safe_before cl rev_ors ->
            do_split
              ((({ p with pat_desc = view }, ps), act) :: rev_before)
              rev_ors rev_no rem
        | _ ->
            let rev_ors, rev_no =
              Or_matrix.insert_or_append (p, ps, act) rev_ors rev_no
            in
            do_split rev_before rev_ors rev_no rem
      )
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
    | _ -> precompile_or ~arg yes yesor args def nexts
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
  let rec split (cls : Simple.clause list) =
    let discr = what_is_first_case cls in
    collect discr [] [] cls
  and collect group_discr rev_yes rev_no = function
    | [ (((p, ps), _) as cl) ]
      when rev_yes <> [] && simple_omega_like p && List.for_all omega_like ps ->
        (* This enables an extra division in some frequent cases:
               last row is made of variables only

           Splitting a matrix there creates two default environments (instead of
           one for the non-split matrix), the first of which often gets
           specialized away by further refinement, and the second one jumping
           directly to the catch-all case -- this produces better code.

           This optimisation is tested in the first part of
           testsuite/tests/basic/patmatch_split_no_or.ml *)
        collect group_discr rev_yes (cl :: rev_no) []
    | (((p, _), _) as cl) :: rem ->
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
      match group_discr.pat_desc with
      | Patterns.Head.Any -> precompile_var
      | _ -> do_not_precompile
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
    | Patterns.Head.Construct { cstr_tag = Cstr_extension _ } ->
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
          let var_args = arg :: rargs in
          let var_cls =
            List.map
              (fun ((p, ps), act) ->
                assert (simple_omega_like p);

                (* we learned by pattern-matching on [args]
                   that [p::ps] has at least two arguments,
                   so [ps] must be non-empty *)
                half_simplify_clause ~arg:(fst arg) (ps, act))
              cls
          and var_def = Default_environment.pop_column def in
          let { me = first; matrix }, nexts =
            split_or ~arg:(Lvar v) var_cls var_args var_def
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
                   As we would be losing information: [def] is more precise
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

and precompile_or ~arg (cls : Simple.clause list) ors args def k =
  (* Example: if [cls] is a single-row matrix

       s11        p12 .. p1n -> act1

     and [ors] has three rows

       (s21|s'21) p22 .. p2n -> act2
       (s31|s'31) p32 .. p3n -> act3
       s41        p42 .. p4n -> act4

     where the first and second rows start with disjoint or-patterns
     of simple patterns, binding the variables x2, y2, z2 and x3, y3
     respectively, we precompile into the following:

     catch
       ( match arg1 .. argn with
       | s11  p12 .. p1n -> act1
       | s21  _   .. _   -> exit 2 x2 y2 z2
       | s'21 _   .. _   -> exit 2 x2 y2 z2
       | s31  _   .. _   -> exit 3 x3 y3
       | s'31 _   .. _   -> exit 3 x3 y3
       | s41  p42 .. p4n -> act4 )
     with
     | exit 2 x2 y2 z2 ->
       ( match arg2 .. argn with
       | p22 .. p2n -> act2 )
     | exit 3 x3 y3 ->
       ( match arg2 .. argn with
       | p32 .. p3n -> act3 )

     Note that if arg1 matches s21 or s'21, we exit to a submatrix
     that will never try any of the following rows; this relies on the
     disjointness-like properties documented in the {!Or_matrix}
     module.

     The code below builds this catch/exit structure, The splitting of
     the or-patterns is done in [Simple.explode_or_pat] -- it turns
     half-simple clauses into simple clauses.
  *)
  let rec do_cases = function
    | [] -> ([], [])
    | ((p, patl), action) :: rem -> (
        match p.pat_desc with
        | #Simple.view as view ->
            let new_ord, new_to_catch = do_cases rem in
            ( (({ p with pat_desc = view }, patl), action) :: new_ord,
              new_to_catch )
        | `Or _ ->
            let orp = General.erase p in
            let others, rem = extract_equiv_head orp rem in
            let orpm =
              { cases =
                  (patl, action)
                  :: List.map (fun ((_, ps), action) -> (ps, action)) others;
                args =
                  ( match args with
                  | _ :: r -> r
                  | _ -> assert false
                  );
                default = Default_environment.pop_compat orp def
              }
            in
            let pm_fv = pm_free_variables orpm in
            let patbound_action_vars =
              (* variables bound in the or-pattern
                 that are used in the orpm actions *)
              Typedtree.pat_bound_idents_full orp
              |> List.filter (fun (id, _, _) -> Ident.Set.mem id pm_fv)
              |> List.map (fun (id, _, ty) ->
                     (id, Typeopt.value_kind orp.pat_env ty))
            in
            let or_num = next_raise_count () in
            let new_patl = Patterns.omega_list patl in
            let mk_new_action ~vars =
              Lstaticraise (or_num, List.map (fun v -> Lvar v) vars)
            in
            let new_cases =
              Simple.explode_or_pat ~arg p
                ~mk_action:mk_new_action
                ~patbound_action_vars:(List.map fst patbound_action_vars)
              |> List.map (fun (p, act) -> ((p, new_patl), act)) in
            let handler =
              { provenance = [ [ orp ] ];
                exit = or_num;
                vars = patbound_action_vars;
                pm = orpm
              }
            in
            let rem_cases, rem_handlers = do_cases rem in
            (new_cases @ rem_cases, handler :: rem_handlers)
      )
  in
  let cases, handlers = do_cases ors in
  let matrix =
    as_matrix
      ((cls : Simple.clause list :> General.clause list)
      @ (ors : Half_simple.clause list :> General.clause list)
      )
  and body = { cases = cls @ cases; args; default = def } in
  ( { me = PmOr { body; handlers; or_matrix = matrix };
      matrix;
      top_default = def
    },
    k )

let dbg_split_and_precompile pm next nexts =
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
    pretty_pm (erase_pm pm);
    pretty_precompiled_res next nexts
  )

let split_and_precompile_simplified pm =
  let { me = next }, nexts = split_no_or pm.cases pm.args pm.default [] in
  dbg_split_and_precompile pm next nexts;
  (next, nexts)

let split_and_precompile_half_simplified ~arg pm =
  let { me = next }, nexts = split_or ~arg pm.cases pm.args pm.default in
  dbg_split_and_precompile pm next nexts;
  (next, nexts)

(* General divide functions *)

type cell = {
  pm : initial_clause pattern_matching;
  ctx : Context.t;
  discr : Patterns.Head.t
}
(** a submatrix after specializing by discriminant pattern;
    [ctx] is the context shared by all rows. *)

let make_matching get_expr_args head def ctx = function
  | [] -> fatal_error "Matching.make_matching"
  | arg :: rem ->
      let def = Default_environment.specialize head def
      and args = get_expr_args head arg rem
      and ctx = Context.specialize head ctx in
      { pm = { cases = []; args; default = def }; ctx; discr = head }

let make_line_matching get_expr_args head def = function
  | [] -> fatal_error "Matching.make_line_matching"
  | arg :: rem ->
      { cases = [];
        args = get_expr_args head arg rem;
        default = Default_environment.specialize head def
      }

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

let divide get_expr_args eq_key get_key get_pat_args ctx
    (pm : Simple.clause pattern_matching) =
  let add ((p, patl), action) division =
    let ph = Simple.head p in
    let p = General.erase p in
    add_in_div
      (make_matching get_expr_args ph pm.default ctx)
      eq_key (get_key p)
      (get_pat_args p patl, action)
      division
  in
  List.fold_right add pm.cases { args = pm.args; cells = [] }

let add_line patl_action pm =
  pm.cases <- patl_action :: pm.cases;
  pm

let divide_line make_ctx get_expr_args get_pat_args discr ctx
    (pm : Simple.clause pattern_matching) =
  let add ((p, patl), action) submatrix =
    let p = General.erase p in
    add_line (get_pat_args p patl, action) submatrix
  in
  let pm =
    List.fold_right add pm.cases
      (make_line_matching get_expr_args discr pm.default pm.args)
  in
  { pm; ctx = make_ctx ctx; discr }

let drop_pat_arg _p rem = rem
let drop_expr_arg _head _arg rem = rem

(* Then come various functions,
   There is one set of functions per matching style
   (constants, constructors etc.)

   - get_{expr,pat}_args and get_key are for the compiled matrices,
     note that selection and getting arguments are separated.

   - make_*_matching combines the previous functions for producing
   new  ``pattern_matching'' records.
*)

(* Matching against a constant *)

let get_key_constant caller = function
  | { pat_desc = Tpat_constant cst } -> cst
  | p ->
      Format.eprintf "BAD: %s" caller;
      pretty_pat p;
      assert false

let get_pat_args_constant = drop_pat_arg
let get_expr_args_constant = drop_expr_arg

let divide_constant ctx m =
  divide
    get_expr_args_constant
    (fun c d -> const_compare c d = 0)
    (get_key_constant "divide")
    get_pat_args_constant ctx m

(* Matching against a constructor *)

let get_key_constr = function
  | { pat_desc = Tpat_construct (_, cstr, _, _) } -> cstr
  | _ -> assert false

let get_pat_args_constr p rem =
  match p with
  | { pat_desc = Tpat_construct (_, _, args, _) } -> args @ rem
  | _ -> assert false

let get_expr_args_constr ~scopes head (arg, _mut) rem =
  let cstr =
    match head.pat_desc with
    | Patterns.Head.Construct cstr -> cstr
    | _ -> fatal_error "Matching.get_expr_args_constr"
  in
  let loc = head_loc ~scopes head in
  let make_field_accesses binding_kind first_pos last_pos argl =
    let rec make_args pos =
      if pos > last_pos then
        argl
      else
        (Lprim (Pfield (pos, Pointer, Immutable), [ arg ], loc),
               binding_kind) :: make_args (pos + 1)
    in
    make_args first_pos
  in
  if cstr.cstr_inlined <> None then
    (arg, Alias) :: rem
  else
    match cstr.cstr_tag with
    | Cstr_constant _
    | Cstr_block _ ->
        make_field_accesses Alias 0 (cstr.cstr_arity - 1) rem
    | Cstr_unboxed -> (arg, Alias) :: rem
    | Cstr_extension _ -> make_field_accesses Alias 1 cstr.cstr_arity rem

let divide_constructor ~scopes ctx pm =
  divide
    (get_expr_args_constr ~scopes)
    (fun cstr1 cstr2 -> Types.equal_tag cstr1.cstr_tag cstr2.cstr_tag)
    get_key_constr
    get_pat_args_constr
    ctx pm

(* Matching against a variant *)

let get_expr_args_variant_constant = drop_expr_arg

let get_expr_args_variant_nonconst ~scopes head (arg, _mut) rem =
  let loc = head_loc ~scopes head in
  (Lprim (Pfield (1, Pointer, Immutable), [ arg ], loc), Alias) :: rem

let divide_variant ~scopes row ctx { cases = cl; args; default = def } =
  let rec divide = function
    | [] -> { args; cells = [] }
    | ((p, patl), action) :: rem
      -> (
        let lab, pato = match p.pat_desc with
          | `Variant (lab, pato, _) -> lab, pato
          | _ -> assert false
        in
        let head = Simple.head p in
        let variants = divide rem in
        if row_field_repr (get_row_field lab row) = Rabsent then
          variants
        else
          let tag = Btype.hash_variant lab in
          match pato with
          | None ->
              add_in_div
                (make_matching get_expr_args_variant_constant head def ctx)
                ( = ) (Cstr_constant tag) (patl, action) variants
          | Some pat ->
              add_in_div
                (make_matching
                   (get_expr_args_variant_nonconst ~scopes)
                   head def ctx)
                ( = ) (Cstr_block tag)
                (pat :: patl, action)
                variants
      )
  in
  divide cl

(*
  Three ``no-test'' cases
  *)

(* Matching against a variable *)

let get_pat_args_var = drop_pat_arg
let get_expr_args_var = drop_expr_arg

let divide_var ctx pm =
  divide_line Context.lshift
    get_expr_args_var
    get_pat_args_var
    Patterns.Head.omega ctx pm

(* Matching and forcing a lazy value *)

let get_pat_args_lazy p rem =
  match p with
  | { pat_desc = Tpat_any } -> Patterns.omega :: rem
  | { pat_desc = Tpat_lazy arg } -> arg :: rem
  | _ -> assert false

(* Inlining the tag tests before calling the primitive that works on
   lazy blocks. This is also used in translcore.ml.
   No other call than Obj.tag when the value has been forced before.
*)

let prim_obj_tag = Primitive.simple ~name:"caml_obj_tag" ~arity:1 ~alloc:false

let get_mod_field modname field =
  lazy
    (let mod_ident = Ident.create_persistent modname in
     let env =
       Env.add_persistent_structure mod_ident Env.initial
     in
     match Env.open_pers_signature modname env with
     | Error `Not_found ->
         fatal_error ("Module " ^ modname ^ " unavailable.")
     | Ok env -> (
         match Env.find_value_by_name (Longident.Lident field) env with
         | exception Not_found ->
             fatal_error ("Primitive " ^ modname ^ "." ^ field ^ " not found.")
         | path, _ -> transl_value_path Loc_unknown env path
       ))

let code_force_lazy_block = get_mod_field "CamlinternalLazy" "force_lazy_block"

let code_force_lazy = get_mod_field "CamlinternalLazy" "force_gen"

(* inline_lazy_force inlines the beginning of the code of Lazy.force. When
   the value argument is tagged as:
   - forward, take field 0
   - lazy || forcing, call the primitive that forces
   - anything else, return it

   Using Lswitch below relies on the fact that the GC does not shortcut
   Forward(val_out_of_heap).
*)

let call_force_lazy_block varg loc =
  (* The argument is wrapped with [Popaque] to prevent the rest of the compiler
     from making any assumptions on its contents (see comments on
     [CamlinternalLazy.force_gen], and discussions on PRs #9998 and #10909).
     Alternatively, [ap_inlined] could be set to [Never_inline] to achieve a
     similar result. *)
  let force_fun = Lazy.force code_force_lazy_block in
  Lapply
    { ap_tailcall = Default_tailcall;
      ap_loc = loc;
      ap_func = force_fun;
      ap_args = [ Lprim (Popaque, [ varg ], loc) ];
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise
    }

let inline_lazy_force_cond arg loc =
  let idarg = Ident.create_local "lzarg" in
  let varg = Lvar idarg in
  let tag = Ident.create_local "tag" in
  let test_tag t =
    Lprim(Pintcomp Ceq, [Lvar tag; Lconst(Const_base(Const_int t))], loc)
  in

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
              test_tag Obj.forward_tag,
              Lprim (Pfield (0, Pointer, Mutable), [ varg ], loc),
              Lifthenelse
                (
                  (* ... if tag == Obj.lazy_tag || tag == Obj.forcing_tag then
                         Lazy.force varg
                       else ... *)
                  Lprim (Psequor,
                       [test_tag Obj.lazy_tag; test_tag Obj.forcing_tag], loc),
                  call_force_lazy_block varg loc,
                  (* ... arg *)
                  varg ) ) ) )

let inline_lazy_force_switch arg loc =
  let idarg = Ident.create_local "lzarg" in
  let varg = Lvar idarg in
  Llet
    ( Strict,
      Pgenval,
      idarg,
      arg,
      Lifthenelse
        ( Lprim (Pisint, [ varg ], loc),
          varg,
          Lswitch
            ( Lprim (Pccall prim_obj_tag, [ varg ], loc),
              { sw_numblocks = 0;
                sw_blocks = [];
                sw_numconsts = 256;
                (* PR#6033 - tag ranges from 0 to 255 *)
                sw_consts =
                  [ (Obj.forward_tag, Lprim (Pfield(0, Pointer, Mutable),
                                             [ varg ], loc));
                    (Obj.lazy_tag, call_force_lazy_block varg loc);
                    (Obj.forcing_tag, call_force_lazy_block varg loc)
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
      { ap_tailcall = Default_tailcall;
        ap_loc = loc;
        ap_func = Lazy.force code_force_lazy;
        ap_args = [ Lconst (Const_base (Const_int 0)); arg ];
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

let get_expr_args_lazy ~scopes head (arg, _mut) rem =
  let loc = head_loc ~scopes head in
  (inline_lazy_force arg loc, Strict) :: rem

let divide_lazy ~scopes head ctx pm =
  divide_line (Context.specialize head)
    (get_expr_args_lazy ~scopes)
    get_pat_args_lazy
    head ctx pm

(* Matching against a tuple pattern *)

let get_pat_args_tuple arity p rem =
  match p with
  | { pat_desc = Tpat_any } -> Patterns.omegas arity @ rem
  | { pat_desc = Tpat_tuple args } -> args @ rem
  | _ -> assert false

let get_expr_args_tuple ~scopes head (arg, _mut) rem =
  let loc = head_loc ~scopes head in
  let arity = Patterns.Head.arity head in
  let rec make_args pos =
    if pos >= arity then
      rem
    else
      (Lprim (Pfield (pos, Pointer, Immutable), [ arg ], loc),
             Alias) :: make_args (pos + 1)
  in
  make_args 0

let divide_tuple ~scopes head ctx pm =
  let arity = Patterns.Head.arity head in
  divide_line (Context.specialize head)
    (get_expr_args_tuple ~scopes)
    (get_pat_args_tuple arity)
    head ctx pm

(* Matching against a record pattern *)

let record_matching_line num_fields lbl_pat_list =
  let patv = Array.make num_fields Patterns.omega in
  List.iter (fun (_, lbl, pat) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
  Array.to_list patv

let get_pat_args_record num_fields p rem =
  match p with
  | { pat_desc = Tpat_any } -> record_matching_line num_fields [] @ rem
  | { pat_desc = Tpat_record (lbl_pat_list, _) } ->
      record_matching_line num_fields lbl_pat_list @ rem
  | _ -> assert false

let get_expr_args_record ~scopes head (arg, _mut) rem =
  let loc = head_loc ~scopes head in
  let all_labels =
    let open Patterns.Head in
    match head.pat_desc with
    | Record (lbl :: _) -> lbl.lbl_all
    | Record []
    | _ ->
        assert false
  in
  let rec make_args pos =
    if pos >= Array.length all_labels then
      rem
    else
      let lbl = all_labels.(pos) in
      let ptr = Typeopt.maybe_pointer_type head.pat_env lbl.lbl_arg in
      let access =
        match lbl.lbl_repres with
        | Record_regular
        | Record_inlined _ ->
            Lprim (Pfield (lbl.lbl_pos, ptr, lbl.lbl_mut), [ arg ], loc)
        | Record_unboxed _ -> arg
        | Record_float -> Lprim (Pfloatfield lbl.lbl_pos, [ arg ], loc)
        | Record_extension _ ->
            Lprim (Pfield (lbl.lbl_pos + 1, ptr, lbl.lbl_mut), [ arg ], loc)
      in
      let str =
        match lbl.lbl_mut with
        | Immutable -> Alias
        | Mutable -> StrictOpt
      in
      (access, str) :: make_args (pos + 1)
  in
  make_args 0

let divide_record all_labels ~scopes head ctx pm =
  (* There is some redundancy in the expansions here, [head] is
     expanded here and again in the matcher. It would be
     nicer to have a type-level distinction between expanded heads
     and non-expanded heads, to be able to reason confidently on
     when expansions must happen. *)
  let head = expand_record_head head in
  divide_line (Context.specialize head)
    (get_expr_args_record ~scopes)
    (get_pat_args_record (Array.length all_labels))
    head ctx pm

(* Matching against an array pattern *)

let get_key_array = function
  | { pat_desc = Tpat_array patl } -> List.length patl
  | _ -> assert false

let get_pat_args_array p rem =
  match p with
  | { pat_desc = Tpat_array patl } -> patl @ rem
  | _ -> assert false

let get_expr_args_array ~scopes kind head (arg, _mut) rem =
  let len =
    let open Patterns.Head in
    match head.pat_desc with
    | Array len -> len
    | _ -> assert false
  in
  let loc = head_loc ~scopes head in
  let rec make_args pos =
    if pos >= len then
      rem
    else
      ( Lprim
          (Parrayrefu kind, [ arg; Lconst (Const_base (Const_int pos)) ], loc),
        StrictOpt )
      :: make_args (pos + 1)
  in
  make_args 0

let divide_array ~scopes kind ctx pm =
  divide
    (get_expr_args_array ~scopes kind)
    ( = )
    get_key_array get_pat_args_array
    ctx pm

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

  type loc = Lambda.scoped_location
  type arg = Lambda.lambda
  type test = Lambda.lambda
  type act = Lambda.lambda

  let make_prim p args = Lprim (p, args, Loc_unknown)

  let make_offset arg n =
    match n with
    | 0 -> arg
    | _ -> Lprim (Poffsetint n, [ arg ], Loc_unknown)

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

  let make_isout h arg = Lprim (Pisout, [ h; arg ], Loc_unknown)

  let make_isin h arg = Lprim (Pnot, [ make_isout h arg ], Loc_unknown)

  let make_is_nonzero arg =
    if !Clflags.native_code then
      Lprim (Pintcomp Cne,
             [arg; Lconst (Const_base (Const_int 0))],
             Loc_unknown)
    else
      arg

  let arg_as_test arg = arg

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
  | constr :: _ as constrs ->
      let constr_of_pat cstr_pat =
        cstr_pat.pat_desc in
      let pat_of_constr cstr =
        let open Patterns.Head in
        to_omega_pattern { constr with pat_desc = Construct cstr } in
      List.map pat_of_constr
        (complete_constrs constr (List.map constr_of_pat constrs))
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
              | Const_string (s, _, _) -> (s, act)
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
    | (cstr_tag, act) :: rem -> (
        let consts, nonconsts = split_rec rem in
        match cstr_tag with
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
    | (cstr_tag, act) :: rem -> (
        let consts, nonconsts = split_rec rem in
        match cstr_tag with
        | Cstr_extension (path, true) -> ((path, act) :: consts, nonconsts)
        | Cstr_extension (path, false) -> (consts, (path, act) :: nonconsts)
        | _ -> assert false
      )
  in
  split_rec tag_lambda_list

let combine_constructor loc arg pat_env cstr partial ctx def
    (descr_lambda_list, total1, pats) =
  let tag_lambda (cstr, act) = (cstr.cstr_tag, act) in
  match cstr.cstr_tag with
  | Cstr_extension _ ->
      (* Special cases for extensions *)
      let fail, local_jumps = mk_failaction_neg partial ctx def in
      let lambda1 =
        let consts, nonconsts =
          split_extension_cases (List.map tag_lambda descr_lambda_list) in
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
                    let ext = transl_extension_path loc pat_env path in
                    Lifthenelse
                      (Lprim (Pintcomp Ceq, [ Lvar tag; ext ], loc), act, rem))
                  nonconsts default
              in
              Llet (Alias, Pgenval, tag,
                    Lprim (Pfield (0, Pointer, Immutable), [ arg ], loc), tests)
        in
        List.fold_right
          (fun (path, act) rem ->
            let ext = transl_extension_path loc pat_env path in
            Lifthenelse (Lprim (Pintcomp Ceq, [ arg; ext ], loc), act, rem))
          consts nonconst_lambda
      in
      (lambda1, Jumps.union local_jumps total1)
  | _ ->
      (* Regular concrete type *)
      let ncases = List.length descr_lambda_list
      and nconstrs = cstr.cstr_consts + cstr.cstr_nonconsts in
      let sig_complete = ncases = nconstrs in
      let fail_opt, fails, local_jumps =
        if sig_complete then
          (None, [], Jumps.empty)
        else
          let constrs =
            List.map2 (fun (constr, _act) p -> { p with pat_desc = constr })
              descr_lambda_list pats in
          mk_failaction_pos partial constrs ctx def
      in
      let descr_lambda_list = fails @ descr_lambda_list in
      let consts, nonconsts =
        split_cases (List.map tag_lambda descr_lambda_list) in
      let lambda1 =
        match (fail_opt, same_actions descr_lambda_list) with
        | None, Some act -> act (* Identical actions, no failure *)
        | _ -> (
            match
              (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts)
            with
            | 1, 1, [ (0, act1) ], [ (0, act2) ] ->
                if !Clflags.native_code then
                  Lifthenelse(Lprim (Pisint, [ arg ], loc), act1, act2)
                else
                  (* PR#10681: we use [arg] directly as the test here;
                     it generates better bytecode for this common case
                     (typically options and lists), but would prevent
                     some optimizations with the native compiler. *)
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
      Lprim (Pfield (0, Pointer, Immutable), [ arg ], loc),
      call_switcher loc fail (Lvar v) min_int max_int int_lambda_list )

let combine_variant loc row arg partial ctx def (tag_lambda_list, total1, _pats)
    =
  let num_constr = ref 0 in
  if row_closed row then
    List.iter
      (fun (_, f) ->
        match row_field_repr f with
        | Rabsent
        | Reither (true, _ :: _, _) ->
            ()
        | _ -> incr num_constr)
      (row_fields row)
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
            begin match fail with
            | None ->
              make_test_sequence_variant_constant fail arg consts
            | Some act ->
              test_int_or_block arg
                (make_test_sequence_variant_constant fail arg consts)
                act
            end
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
        else begin
          match compile_fun cell.ctx cell.pm with
          | exception Unused -> c_rec totals rem
          | lambda1, total1 ->
            let c_rem, total, new_discrs =
              c_rec (Jumps.map Context.combine total1 :: totals) rem
            in
            ( (key, lambda1) :: c_rem,
              total,
              Patterns.Head.to_omega_pattern cell.discr :: new_discrs )
        end
      )
  in
  c_rec [] division

let compile_orhandlers compile_fun lambda1 total1 ctx to_catch =
  let rec do_rec r total_r = function
    | [] -> (r, total_r)
    | { provenance = mat; exit = i; vars; pm } :: rem -> (
        let ctx = Context.select_columns mat ctx in
        match compile_fun ctx pm with
        | exception Unused ->
          do_rec (Lstaticcatch (r, (i, vars), lambda_unit)) total_r rem
        | handler_i, total_i ->
          begin match raw_action r with
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
          end
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
            else begin
              let partial = match rem with
                | [] -> partial
                | _ -> Partial
              in
              match comp_fun partial ctx_i pm with
              | li, total_i ->
                c_rec
                  (Lstaticcatch (body, (i, []), li))
                  (Jumps.union total_i total_rem)
                  rem
              | exception Unused ->
                c_rec
                  (Lstaticcatch (body, (i, []), lambda_unit))
                  total_rem rem
            end
          )
      in
      match comp_fun Partial ctx first_match with
      | first_lam, total ->
        c_rec first_lam total rem
      | exception Unused -> (
        match next_matchs with
        | [] -> raise Unused
        | (_, x) :: xs -> comp_match_handlers comp_fun partial ctx x xs
      )
    )

(* To find reasonable names for variables *)

let rec name_pattern default = function
  | ((pat, _), _) :: rem -> (
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

let rec compile_match ~scopes repr partial ctx
    (m : initial_clause pattern_matching) =
  match m.cases with
  | ([], action) :: rem ->
      if is_guarded action then
        let lambda, total =
          compile_match ~scopes None partial ctx { m with cases = rem }
        in
        (event_branch repr (patch_guarded lambda action), total)
      else
        (event_branch repr action, Jumps.empty)
  | nonempty_cases ->
      compile_match_nonempty ~scopes repr partial ctx
        { m with cases = map_on_rows Non_empty_row.of_initial nonempty_cases }

and compile_match_nonempty ~scopes repr partial ctx
    (m : Typedtree.pattern Non_empty_row.t clause pattern_matching) =
  match m with
  | { cases = []; args = [] } -> comp_exit ctx m
  | { args = (arg, str) :: argl } ->
      let v, newarg = arg_to_var arg m.cases in
      let args = (newarg, Alias) :: argl in
      let cases = List.map (half_simplify_nonempty ~arg:newarg) m.cases in
      let m = { m with args; cases } in
      let first_match, rem =
        split_and_precompile_half_simplified ~arg:newarg m in
      combine_handlers ~scopes repr partial ctx (v, str, arg) first_match rem
  | _ -> assert false

and compile_match_simplified ~scopes repr partial ctx
    (m : Simple.clause pattern_matching) =
  match m with
  | { cases = []; args = [] } -> comp_exit ctx m
  | { args = ((Lvar v as arg), str) :: argl } ->
      let args = (arg, Alias) :: argl in
      let m = { m with args } in
      let first_match, rem = split_and_precompile_simplified m in
      combine_handlers ~scopes repr partial ctx (v, str, arg) first_match rem
  | _ -> assert false

and combine_handlers ~scopes repr partial ctx (v, str, arg) first_match rem =
  let lam, total =
    comp_match_handlers
      (( if dbg then
         do_compile_matching_pr ~scopes
       else
         do_compile_matching ~scopes
       )
         repr)
      partial ctx first_match rem
  in
  (bind_check str v arg lam, total)

(* verbose version of do_compile_matching, for debug *)
and do_compile_matching_pr ~scopes repr partial ctx x =
  Format.eprintf "COMPILE: %s\nMATCH\n"
    ( match partial with
    | Partial -> "Partial"
    | Total -> "Total"
    );
  pretty_precompiled x;
  Format.eprintf "CTX\n";
  Context.eprintf ctx;
  let ((_, jumps) as r) = do_compile_matching ~scopes repr partial ctx x in
  Format.eprintf "JUMPS\n";
  Jumps.eprintf jumps;
  r

and do_compile_matching ~scopes repr partial ctx pmh =
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
      let ph = what_is_cases pm.cases in
      let pomega = Patterns.Head.to_omega_pattern ph in
      let ploc = head_loc ~scopes ph in
      let open Patterns.Head in
      match ph.pat_desc with
      | Any ->
          compile_no_test ~scopes
            divide_var
            Context.rshift repr partial ctx pm
      | Tuple _ ->
          compile_no_test ~scopes
            (divide_tuple ~scopes ph)
            Context.combine repr partial ctx pm
      | Record [] -> assert false
      | Record (lbl :: _) ->
          compile_no_test ~scopes
            (divide_record ~scopes lbl.lbl_all ph)
            Context.combine repr partial ctx pm
      | Constant cst ->
          compile_test
            (compile_match ~scopes repr partial)
            partial divide_constant
            (combine_constant ploc arg cst partial)
            ctx pm
      | Construct cstr ->
          compile_test
            (compile_match ~scopes repr partial)
            partial (divide_constructor ~scopes)
            (combine_constructor ploc arg ph.pat_env cstr partial)
            ctx pm
      | Array _ ->
          let kind = Typeopt.array_pattern_kind pomega in
          compile_test
            (compile_match ~scopes repr partial)
            partial (divide_array ~scopes kind)
            (combine_array ploc arg kind partial)
            ctx pm
      | Lazy ->
          compile_no_test ~scopes
            (divide_lazy ~scopes ph)
            Context.combine repr partial ctx pm
      | Variant { cstr_row = row } ->
          compile_test
            (compile_match ~scopes repr partial)
            partial (divide_variant ~scopes !row)
            (combine_variant ploc !row arg partial)
            ctx pm
    )
  | PmVar { inside = pmh } ->
      let lam, total =
        do_compile_matching ~scopes repr partial (Context.lshift ctx) pmh
      in
      (lam, Jumps.map Context.rshift total)
  | PmOr { body; handlers } ->
      let lam, total =
        compile_match_simplified ~scopes repr partial ctx body in
      compile_orhandlers (compile_match ~scopes repr partial)
        lam total ctx handlers

and compile_no_test ~scopes divide up_ctx repr partial ctx to_match =
  let { pm = this_match; ctx = this_ctx } = divide ctx to_match in
  let lambda, total =
    compile_match ~scopes repr partial this_ctx this_match in
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

let is_lazy_pat p =
  match p.pat_desc with
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

let has_lazy p = Typedtree.exists_pattern is_lazy_pat p

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

let has_mutable p = Typedtree.exists_pattern is_record_with_mutable_field p

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

let check_partial_list pats_act_list =
  check_partial (List.exists has_mutable) (List.exists has_lazy) pats_act_list

let check_partial pat_act_list =
  check_partial has_mutable has_lazy pat_act_list

(* have toplevel handler when appropriate *)

type failer_kind =
  | Raise_match_failure
  | Reraise_noloc of lambda

let failure_handler ~scopes loc ~failer () =
  match failer with
  | Reraise_noloc exn_lam ->
    Lprim (Praise Raise_reraise, [ exn_lam ], Scoped_location.Loc_unknown)
  | Raise_match_failure ->
    let sloc = Scoped_location.of_location ~scopes loc in
    let slot =
      transl_extension_path sloc
        Env.initial Predef.path_match_failure
    in
    let fname, line, char =
      Location.get_pos_info loc.Location.loc_start in
    Lprim
      ( Praise Raise_regular,
        [ Lprim
            ( Pmakeblock (0, Immutable, None),
              [ slot;
                Lconst
                  (Const_block
                     ( 0,
                       [ Const_base (Const_string (fname, loc, None));
                         Const_base (Const_int line);
                         Const_base (Const_int char)
                       ] ))
              ],
              sloc )
        ],
        sloc )

let check_total ~scopes loc ~failer total lambda i =
  if Jumps.is_empty total then
    lambda
  else
    Lstaticcatch (lambda, (i, []),
                  failure_handler ~scopes loc ~failer ())

let toplevel_handler ~scopes loc ~failer partial args cases compile_fun =
  match partial with
  | Total when not !Clflags.safer_matching ->
      let default = Default_environment.empty in
      let pm = { args; cases; default } in
      let (lam, total) = compile_fun Total pm in
      assert (Jumps.is_empty total);
      lam
  | Partial | Total (* when !Clflags.safer_matching *) ->
      let raise_num = next_raise_count () in
      let default =
        Default_environment.cons [ Patterns.omega_list args ] raise_num
          Default_environment.empty in
      let pm = { args; cases; default } in
      begin match compile_fun Partial pm with
      | exception Unused -> assert false
      | (lam, total) ->
          check_total ~scopes loc ~failer total lam raise_num
      end

let compile_matching ~scopes loc ~failer repr arg pat_act_list partial =
  let partial = check_partial pat_act_list partial in
  let args = [ (arg, Strict) ] in
  let rows = map_on_rows (fun pat -> (pat, [])) pat_act_list in
  toplevel_handler ~scopes loc ~failer partial args rows (fun partial pm ->
    compile_match_nonempty ~scopes repr partial (Context.start 1) pm)

let for_function ~scopes loc repr param pat_act_list partial =
  compile_matching ~scopes loc ~failer:Raise_match_failure
    repr param pat_act_list partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith ~scopes loc param pat_act_list =
  (* Note: the failure action of [for_trywith] corresponds
     to an exception that is not matched by a try..with handler,
     and is thus reraised for the next handler in the stack.

     It is important to *not* include location information in
     the reraise (hence the [_noloc]) to avoid seeing this
     silent reraise in exception backtraces. *)
  compile_matching ~scopes loc ~failer:(Reraise_noloc param)
    None param pat_act_list Partial

let simple_for_let ~scopes loc param pat body =
  compile_matching ~scopes loc ~failer:Raise_match_failure
    None param [ (pat, body) ] Partial

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
  | Lmutlet (k, id, l1, l2) -> Lmutlet (k, id, l1, map_return f l2)
  | Lletrec (l1, l2) -> Lletrec (l1, map_return f l2)
  | Lifthenelse (lcond, lthen, lelse) ->
      Lifthenelse (lcond, map_return f lthen, map_return f lelse)
  | Lsequence (l1, l2) -> Lsequence (l1, map_return f l2)
  | Levent (l, ev) -> Levent (map_return f l, ev)
  | Ltrywith (l1, id, l2) -> Ltrywith (map_return f l1, id, map_return f l2)
  | Lstaticcatch (l1, b, l2) ->
      Lstaticcatch (map_return f l1, b, map_return f l2)
  | Lswitch (s, sw, loc) ->
      let map_cases cases =
        List.map (fun (i, l) -> (i, map_return f l)) cases
      in
      Lswitch
        ( s,
          { sw with
            sw_consts = map_cases sw.sw_consts;
            sw_blocks = map_cases sw.sw_blocks;
            sw_failaction = Option.map (map_return f) sw.sw_failaction
          },
          loc )
  | Lstringswitch (s, cases, def, loc) ->
      Lstringswitch
        ( s,
          List.map (fun (s, l) -> (s, map_return f l)) cases,
          Option.map (map_return f) def,
          loc )
  | (Lstaticraise _ | Lprim (Praise _, _, _)) as l -> l
  | ( Lvar _ | Lmutvar _ | Lconst _ | Lapply _ | Lfunction _ | Lsend _ | Lprim _
    | Lwhile _ | Lfor _ | Lassign _ | Lifused _ ) as l ->
      f l

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

let assign_pat ~scopes opt nraise catch_ids loc pat lam =
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
           refresh them here to guarantee binders uniqueness *)
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
  let push_sublet code (_ids, pat, lam) =
    simple_for_let ~scopes loc lam pat code in
  List.fold_left push_sublet exit rev_sublets

let for_let ~scopes loc param pat body =
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
      let bind =
        map_return (assign_pat ~scopes opt nraise ids loc pat) param in
      if !opt then
        Lstaticcatch (bind, (nraise, ids_with_kinds), body)
      else
        simple_for_let ~scopes loc param pat body

(* Handling of tupled functions and matchings *)

(* Easy case since variables are available *)
let for_tupled_function ~scopes loc paraml pats_act_list partial =
  let partial = check_partial_list pats_act_list partial in
  let args = List.map (fun id -> (Lvar id, Strict)) paraml in
  let handler =
    toplevel_handler ~scopes loc ~failer:Raise_match_failure
      partial args pats_act_list in
  handler (fun partial pm ->
    compile_match ~scopes None partial
      (Context.start (List.length paraml)) pm
  )

let flatten_pattern size p =
  match p.pat_desc with
  | Tpat_tuple args -> args
  | Tpat_any -> Patterns.omegas size
  | _ -> raise Cannot_flatten

let flatten_simple_pattern size (p : Simple.pattern) =
  match p.pat_desc with
  | `Tuple args -> args
  | `Any -> Patterns.omegas size
  | `Array _
  | `Variant _
  | `Record _
  | `Lazy _
  | `Construct _
  | `Constant _ ->
      (* All calls to this function originate from [do_for_multiple_match],
         where we know that the scrutinee is a tuple literal.

         Since the PM is well typed, none of these cases are possible. *)
      let msg =
        Format.fprintf Format.str_formatter
          "Matching.flatten_pattern: got '%a'" top_pretty (General.erase p);
        Format.flush_str_formatter ()
      in
      fatal_error msg

let flatten_cases size cases =
  List.map
    (function
      | (p, []), action -> (
          match flatten_simple_pattern size p with
          | p :: ps -> ((p, ps), action)
          | [] -> assert false
        )
      | _ -> fatal_error "Matching.flatten_hc_cases")
    cases

let flatten_pm size args pm =
  { args;
    cases = flatten_cases size pm.cases;
    default = Default_environment.flatten size pm.default
  }

let flatten_handler size handler =
  { handler with provenance = flatten_matrix size handler.provenance }

type pm_flattened =
  | FPmOr of (pattern, unit) pm_or_compiled
  | FPm of pattern Non_empty_row.t clause pattern_matching

let flatten_precompiled size args pmh =
  match pmh with
  | Pm pm -> FPm (flatten_pm size args pm)
  | PmOr { body = b; handlers = hs; or_matrix = _ } ->
      FPmOr
        { body = flatten_pm size args b;
          handlers = List.map (flatten_handler size) hs;
          or_matrix = ();
        }
  | PmVar _ -> assert false

(*
   compiled_flattened is a ``comp_fun'' argument to comp_match_handlers.
   Hence it needs a fourth argument, which it ignores
*)

let compile_flattened ~scopes repr partial ctx pmh =
  match pmh with
  | FPm pm -> compile_match_nonempty ~scopes repr partial ctx pm
  | FPmOr { body = b; handlers = hs } ->
      let lam, total = compile_match_nonempty ~scopes repr partial ctx b in
      compile_orhandlers (compile_match ~scopes repr partial) lam total ctx hs

let do_for_multiple_match ~scopes loc paraml pat_act_list partial =
  let repr = None in
  let arg =
    let sloc = Scoped_location.of_location ~scopes loc in
    Lprim (Pmakeblock (0, Immutable, None), paraml, sloc) in
  let handler =
    let partial = check_partial pat_act_list partial in
    let rows = map_on_rows (fun p -> (p, [])) pat_act_list in
    toplevel_handler ~scopes loc ~failer:Raise_match_failure
      partial [ (arg, Strict) ] rows in
  handler (fun partial pm1 ->
    let pm1_half =
      { pm1 with cases = List.map (half_simplify_nonempty ~arg) pm1.cases }
    in
    let next, nexts = split_and_precompile_half_simplified ~arg pm1_half in
    let size = List.length paraml
    and idl = List.map (function
      | Lvar id -> id
      | _ -> Ident.create_local "*match*") paraml in
    let args = List.map (fun id -> (Lvar id, Alias)) idl in
    let flat_next = flatten_precompiled size args next
    and flat_nexts =
      List.map (fun (e, pm) -> (e, flatten_precompiled size args pm)) nexts
    in
    let lam, total =
      comp_match_handlers (compile_flattened ~scopes repr) partial
        (Context.start size) flat_next flat_nexts
    in
    List.fold_right2 (bind Strict) idl paraml lam, total
  )

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

let for_multiple_match ~scopes loc paraml pat_act_list partial =
  let v_paraml = List.map param_to_var paraml in
  let paraml = List.map (fun (v, _) -> Lvar v) v_paraml in
  List.fold_right bind_opt v_paraml
    (do_for_multiple_match ~scopes loc paraml pat_act_list partial)
