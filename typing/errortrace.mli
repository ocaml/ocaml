open Types

type position = First | Second

val swap_position : position -> position
val print_pos : Format.formatter -> position -> unit

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

(** [map_diff f {expected;got}] is [{expected=f expected; got=f got}] *)
val map_diff: ('a -> 'b) -> 'a diff -> 'b diff

(** Scope escape related errors *)
type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

val short : type_expr -> desc

val explain: 'a list ->
  (prev:'a option -> 'a -> 'b option) ->
  'b option

(* Type indices *)
type unification     = private Unification
type non_unification = private Non_unification

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Incompatible_types_for : string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row : position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Openness : position (* Always [Second] for Moregen *) -> non_unification variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type ('a, 'variety) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, _) elt
  | Variant :  'variety variant -> ('a, 'variety) elt
  | Obj :  'variety obj -> ('a, 'variety) elt
  | Escape : 'a escape -> ('a, _) elt
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, _) elt
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) elt

type 'variety t =
  (desc, 'variety) elt list

val diff : type_expr -> type_expr -> (desc, _) elt

(** [flatten f trace] flattens all elements of type {!desc} in
    [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
    or [f x.t x.t] otherwise *)
val flatten: (type_expr -> type_expr -> 'a) -> 'variant t -> ('a, 'variant) elt list

val map : ('a -> 'b) -> ('a, 'variant) elt list -> ('b, 'variant) elt list

val incompatible_fields : string -> type_expr -> type_expr -> (desc, _) elt

val swap_unification_trace : unification t -> unification t

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val flatten : (type_expr -> type_expr -> 'a) -> t -> 'a elt list
end
