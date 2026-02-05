(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types
open Format_doc

type position = First | Second
type order = Less | Equal | More

let swap_position = function
  | First -> Second
  | Second -> First

let swap_order = function
  | Less -> More
  | Equal -> Equal
  | More -> Less

let print_pos ppf = function
  | First -> fprintf ppf "first"
  | Second -> fprintf ppf "second"

type expanded_type = { ty: type_expr; expanded: type_expr }

let trivial_expansion ty = { ty; expanded = ty }

type 'a diff = { got: 'a; expected: 'a }

let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected }

let swap_diff x = { got = x.expected; expected = x.got }

type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Module of Ident.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

let map_escape f esc =
  {esc with kind = match esc.kind with
     | Equation eq -> Equation (f eq)
     | (Constructor _ | Univ _ | Self | Module_type _
        | Module _ | Constraint) as c -> c}

let explain trace f =
  let rec explain = function
    | [] -> None
    | [h] -> f ~prev:None h
    | h :: (prev :: _ as rem) ->
      match f ~prev:(Some prev) h with
      | Some _ as m -> m
      | None -> explain rem in
  explain (List.rev trace)

(* Type indices *)
type unification = private Unification
type comparison  = private Comparison

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Arity_mismatch : string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row :
      position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Presence_not_guaranteed_for : position * string -> comparison variant
  | Openness : position (* Always [Second] for Moregen *) -> comparison variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type first_class_module =
    | Package_cannot_scrape of Path.t
    | Package_inclusion of Format_doc.doc
    | Package_coercion of Format_doc.doc

type univar =
  | Var_mismatch of { order:order; diff:type_expr diff }
  | Quantification_mismatch of type_expr list

type ctx =
  | In_method of string
  | In_tag of string
type 'a ctx_diff = { ctx: ctx option; d: 'a diff }
let map_ctx f x = { x with d = map_diff f x.d }
let swap_ctx x = { x with d = swap_diff x.d }

type ('a, 'variety) root  =
  (* Common *)
  | Variant : 'variety variant -> ('a, 'variety) root
  | Obj : 'variety obj -> ('a, 'variety) root
  | Escape : 'a escape -> ('a, _) root
  | Function_label_mismatch of Asttypes.arg_label diff
  | Tuple_label_mismatch of string option diff
  | First_class_module: first_class_module -> ('a,_) root
  | Univar of univar
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) root

type ('a, 'variety) t = {
  path: 'a ctx_diff list;
  root: ('a, 'variety) root option
}

let root root = { root= Some root; path = [] }
let empty_root = { root = None; path = [] }

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

let map_root (type variety) f : ('a, variety) root -> ('b, variety) root =
  function
  | Escape {kind = Equation x; context} ->
      Escape { kind = Equation (f x); context }
  | Escape {kind = (Univ _ | Self | Constructor _
      | Module_type _ | Module _ | Constraint);
            _}
  | Variant _ | Obj _ | Function_label_mismatch _ | Tuple_label_mismatch _
  | Rec_occur (_, _) | First_class_module _  as x -> x
  | Univar _  as x -> x

let map f t =
  { path = List.map (map_ctx f) t.path; root = Option.map (map_root f) t.root }
let diff ?ctx ~got ~expected trace =
  { trace with path = { ctx; d = {got;expected} } :: trace.path }

let incompatible_fields ~name ~got ~expected  t =
  diff ~ctx:(In_method name) ~got ~expected t
let in_tag ~l t = match t.path with
  | { ctx = None; d} :: rem ->
      { t with path = { ctx = Some(In_tag l); d } :: rem }
  | _ -> t
let variant_arity_mismatch l = Variant (Arity_mismatch l)

let swap_root (type variety) : ('a, variety) root -> ('a, variety) root =
  function
  | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
  | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
  | Variant (Fixed_row(pos,k,f)) ->
    Variant (Fixed_row(swap_position pos,k,f))
  | Variant (No_tags(pos,f)) ->
    Variant (No_tags(swap_position pos,f))
  | Univar (Var_mismatch d) ->
      Univar (Var_mismatch {
        order = swap_order d.order;
        diff = swap_diff d.diff
      })
  | Univar (Quantification_mismatch _) as x -> x
  | x -> x

let swap_trace t = {
  root = Option.map swap_root t.root;
  path = List.map swap_ctx t.path
}

type unification_error = { trace : unification error } [@@unboxed]

type equality_error =
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = { trace : comparison error } [@@unboxed]

let non_empty trace = trace.root <> None || trace.path <> []
let unification_error ~trace : unification_error =
  assert (non_empty trace);
  { trace }

let equality_error ~trace ~subst : equality_error =
    assert (non_empty trace);
    { trace; subst }

let moregen_error ~trace : moregen_error =
  assert (non_empty trace);
  { trace }

type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

let swap_unification_error ({trace} : unification_error) =
  ({trace = swap_trace trace} : unification_error)

module Subtype = struct

  type 'a t = 'a ctx_diff list

  type trace       = type_expr t
  type error_trace = expanded_type t

  type unification_error_trace = unification error (** To avoid shadowing *)

  type nonrec error =
    { trace             : error_trace
    ; unification_trace : unification error }

  let diff ?ctx ~got ~expected t = { ctx; d = { got; expected }} :: t
  let error ~trace ~unification_trace =
  assert (trace <> []);
  { trace; unification_trace }

  let map f t = List.map (map_ctx f) t
end

module Structured = struct

type 'a extended_explanation =
  | Promoted of Format_doc.t
  | Standard of 'a

type ('a,'b) s = {
  top: ('a ctx_diff * bool) option;
  tr: 'a ctx_diff list;
  expl: ('a,'b) root extended_explanation option;
}
(**
This module contains helper functions to split the error trace into three parts:
- {!top} the first element of the trace
- {!tr} a list of meaningful context difference element
- {!expl} a root explanation for a type error
*)

(** The first intermediary representation splits the trace into four parts:
- the {!head} element of the trace
- the {!top_to_ctx} trace elements in reverse order situated before the last
  method or tag difference
- {!last_ctx}: the last method or tag context, and the list of elements after it
- {!optional}: the last element that might be printed if we don't discover a
  better element to print later on.
*)
type ('a,'b) segments =
  {
    head: 'a;
    before:'a list;
    last_ctx:('a * 'a list) option;
    optional: 'a option;
    expl:  'b option;
  }

let head_segment hd expl =
  { head = hd; before=[]; last_ctx = None; optional = None; expl }

(** Add a non-contextual element to the active context *)
let add x s = match s.last_ctx with
  | None ->  { s with optional = None; before = x :: s.before }
  | Some (ctx, rest)->
      { s with optional = None; last_ctx = Some (ctx, x :: rest)}

(** Add a new context, add the last context contents to the {!before} trace *)
let add_ctx c s = match s.last_ctx with
  | None -> { s with optional = None; last_ctx = Some (c,[]) }
  | Some (_ctx, rest) -> {
      head = s.head;
      optional = None;
      before = rest @ s.before;
      last_ctx = Some (c,[]);
      expl = s.expl
    }

type printing_status =
  | Discard
  | Keep
  | Context
    (** A {!Context} element marks the entry inside a method or a polymorphic
        variant tag *)
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

(** Construct a segment from an unstructured trace *)
let segment status path expl = match path with
  | [] -> assert false
  | hd :: tr ->
      List.fold_left (fun s x ->
      match status x with
      | Discard -> s
      | Keep -> add x s
      | Optional_refinement -> { s with optional = Some x }
      | Context -> add_ctx x s
    ) (head_segment hd expl) tr

let promoted x = Option.map (fun p -> Promoted p) x
let merge promote s =
  (* First, we commit the last contextualized segment of the trace to the
     trace *)
  let rtr, opt, maybe_compact = match s.last_ctx with
    | None -> s.before, s.optional, false
    | Some (ctx, rest) -> rest @ ctx :: s.before, None, true
  in
  let head, rtr, expl = match s.expl, rtr with
    | Some expl, _ -> Some s.head, rtr, Some (Standard expl)
    | None, [] -> Some s.head, [], promoted (promote s.head.d)
    | None, d :: _ ->
        match promote d.d with
        | Some p -> Some s.head, rtr, Some (Promoted p)
        | None -> Some s.head, rtr, None
  in
  let tr = match expl, opt with
    | None, Some opt -> List.rev (opt :: rtr)
    | Some _, _ | None, _  -> List.rev rtr
  in
  let compact head = match tr, maybe_compact with
    | [], _ | [_], true -> head, true
    | _ -> head, false
  in
  { top=Option.map compact head; tr; expl }

  let parse ~promote ~status s =
    match s.path with
    | [] ->
        let expl = Option.map (fun s -> Standard s) s.root in
        { top = None; tr = []; expl}
    | path -> merge promote (segment status path s.root)

  let parse_simple status tr =
    match tr with
    | [] -> None
    | _ ->
        let s = segment status tr None in
        let rtr = match s.last_ctx with
          | None -> s.before
          | Some (ctx, rest) -> rest @ ctx :: s.before
        in
        let rtr = match s.optional with
          | Some x -> x :: rtr
          | None -> rtr
        in
        Some (s.head, List.rev rtr)

end
