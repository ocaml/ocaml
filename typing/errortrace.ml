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
type ctx =
  | In_method of string
  | In_tag of string
type 'a ctx_diff = { ctx: ctx option; d: 'a diff }

let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected }
let swap_diff x = { got = x.expected; expected = x.got }

let map_cdiff f x = { x with d = map_diff f x.d }
let swap_cdiff x = { x with d = swap_diff x.d }

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

type highlight_target =
  | Type of Outcometree.highlight_kind * type_expr
  | Type_constructor of Path.t

type highlight_hint = highlight_target option diff

type ('a, 'variety) root  =
  (* Common *)
  | Variant : 'variety variant -> ('a, 'variety) root
  | Obj : 'variety obj -> ('a, 'variety) root
  | Escape : 'a escape -> ('a, _) root
  | Function_label_mismatch of Asttypes.arg_label diff
  | Tuple_label_mismatch of string option diff
  | First_class_module: first_class_module -> ('a,_) root
  | Univar of univar
  | Highlight_hint of highlight_hint
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) root

type ('a, 'variety) t = {
  path: 'a ctx_diff list;
  root: ('a, 'variety) root option
}

let root root = { root= Some root; path = [] }
let empty_root = { root = None; path = [] }
let no_ctx d = { ctx = None; d }
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
  | Highlight_hint _ as x -> x

let map f t ={
  path = List.map (map_cdiff f) t.path;
  root = Option.map (map_root f) t.root
}
let diff ?ctx ~got ~expected trace =
  { trace with path = { ctx; d = {got;expected} } :: trace.path }

let incompatible_fields ~name ~got ~expected  t =
  diff ~ctx:(In_method name) ~got ~expected t
let in_tag ~l t = match t.path with
  | { ctx = None; d} :: rem ->
      { t with path = { ctx = Some(In_tag l); d } :: rem }
  | _ -> t
let variant_arity_mismatch l = Variant (Arity_mismatch l)
let highlight_type k ty = Some (Type(k,ty))


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
  | Highlight_hint d -> Highlight_hint (swap_diff d)
  | x -> x

let swap_trace t = {
  root = Option.map swap_root t.root;
  path = List.map swap_cdiff t.path
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

  let map f t = List.map (map_cdiff f) t
end

module Structured = struct
(** This module contains helper functions to parse the error trace into the more
    structured type {!Stuctured.t} *)

(** We extend the core explanation type with promoted explanation generated from
    the main trace *)
type 'a extended_explanation =
  | Standard of 'a
  | Promoted of highlight_hint option * Format_doc.t
  | Hint of highlight_hint

type ('a,'b,'c) s = {
  top: ('a ctx_diff * bool) option;
  tr: 'a ctx_diff list;
  expl: ('b,'c) root extended_explanation option;
}
(** The structured version of the trace is split in three parts:
- {!top} the first element of the trace
- {!tr} a list of meaningful context difference element
- {!expl} a root explanation for a type error
*)

(**  The first intermediary representation splits the trace into four parts:
- the {!head} element of the trace
- {!before}: trace elements appearing before the last method or tag
  difference, in reverse order.
- {!last_ctx}: the last method or tag context, and the list of elements after it
  in reverse order.
- {!optional}: the last element that might be useful to print, if we do not
  discover a better element to print later on.
Contrarily to the {!t} format this form can be built element by element. *)
type ('a,'b) segments =
  {
    head: 'a;
    before:'a list;
    last_ctx:('a * 'a list) option;
    optional: 'a option;
    expl: 'b option;
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

(** Construct a segmented trace from an unstructured trace *)
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

let promoted ?hint x = match x, hint with
  | None, None -> None
  | Some x, _ -> Some (Promoted (hint,x))
  | None, Some h -> Some (Hint h)

type 'a visibility =
  | Visible of 'a
  | Info of highlight_hint
  | Invisible

let explanation_visibility = function
  | None -> Invisible
  | Some (Highlight_hint h) -> Info h
  | Some e -> Visible e

let split_hint = function
  | Highlight_hint h -> Hint h
  | e -> Standard e

let merge promote s =
  (* First, we commit the last contextualized segment of the trace to the trace
     if there one. We also discard the optional last element in this
     case. *)
  let rtr, opt = match s.last_ctx with
    | None -> s.before, s.optional
    | Some (ctx, rest) -> rest @ ctx :: s.before, None
  in
  let rtr, expl =
    (* If there are no root explanation, we try to promote one from the last
       element*)
    match explanation_visibility s.expl, rtr, s.head with
    | Visible expl, _, _ -> rtr, Some (Standard expl)
    | Invisible, [], last | Invisible, last :: _, _ ->
        rtr, promoted (promote last.d)
    | Info hint, [], last | Info hint, last :: _, _ ->
        rtr, promoted ~hint (promote last.d)
  in
  (* Finally, we keep the last optional element only if there were no
     explanation at all.*)
  let tr = match expl, opt with
    | (None | Some (Hint _)), Some opt -> List.rev (opt :: rtr)
    | Some _, _ | None, None  -> List.rev rtr
  in
  (* We use a compact presentation for the top element only if the trace is
     empty, or is a singleton contextual element (?). *)
  let compact_head = match tr with
    | [] | [{ ctx = Some _; _}] -> s.head, true
    | _ -> s.head, false
  in
  { top = Some compact_head; tr; expl }

  let parse ~promote ~status s =
    match s.path with
    | [] ->
        let expl = Option.map split_hint s.root in
        { top = None; tr = []; expl }
    | path -> merge promote (segment status path s.root)

end
