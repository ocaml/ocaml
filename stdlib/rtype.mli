module Ident : sig
  type t = string * int
end

module Path : sig
  type t =
      Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t
  val name : t -> string
end

type mutable_flag = Immutable | Mutable

type label = string

type private_flag = Private | Public

type record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

type type_expr = (Path.t * type_declaration) raw_type_expr
and type_desc = (Path.t * type_declaration) raw_type_desc
and type_declaration = (Path.t * type_declaration) raw_type_declaration
and type_kind = (Path.t * type_declaration)  raw_type_kind

and 'a raw_type_expr =
  { (* mutable *) desc: 'a raw_type_desc; 
    (* mutable level: int; *)
    (* mutable id: int *) }

and 'a raw_type_desc =
    Tvar
  | Tarrow of label * 'a raw_type_expr * 'a raw_type_expr (* * commutable *)
  | Ttuple of 'a raw_type_expr list
  | Tconstr of 'a * 'a raw_type_expr list (* * abbrev_memo ref *)
(*
  | Tobject of 'a raw_type_expr * (Path.t * 'a raw_type_expr list) option ref
  | Tfield of string * field_kind * 'a raw_type_expr * 'a raw_type_expr
  | Tnil
  | Tlink of 'a raw_type_expr
  | Tsubst of 'a raw_type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar
  | Tpoly of 'a raw_type_expr * 'a raw_type_expr list
*)

(* Type definitions *)

and 'a raw_type_declaration =
  { type_params: 'a raw_type_expr list;
    type_arity: int;
    type_kind: 'a raw_type_kind;
    type_manifest: 'a raw_type_expr option;
    type_variance: (bool * bool * bool) list }
            (* covariant, contravariant, weakly contravariant *)

and 'a raw_type_kind =
    Type_abstract
  | Type_variant of (string * 'a raw_type_expr list) list * private_flag
  | Type_record of (string * mutable_flag * 'a raw_type_expr) list
                 * record_representation * private_flag

(* type equality *)
val equal : type_expr -> type_expr -> bool
val raw_equal : ('a -> 'a -> bool) -> 'a raw_type_expr -> 'a raw_type_expr -> bool

(* substitution *)
val raw_subst : ('a raw_type_expr * 'a raw_type_expr) list -> 'a raw_type_expr -> 'a raw_type_expr
val subst : (type_expr * type_expr) list -> type_expr -> type_expr

(* printers *)
val reset_names : unit -> unit
val print_path : Format.formatter -> Path.t -> unit
val raw_print : (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a raw_type_expr -> unit
val print : Format.formatter -> type_expr -> unit
