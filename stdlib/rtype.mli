module Ident : sig
  type t = string * int
end

module Path : sig
  type t =
      Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t
end

type mutable_flag = Immutable | Mutable

type label = string

type private_flag = Private | Public

type record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

type type_expr =
  { (* mutable *) desc: type_desc; 
    (* mutable level: int; *)
    (* mutable id: int *) }

and type_desc =
    Tvar
  | Tarrow of label * type_expr * type_expr (* * commutable *)
  | Ttuple of type_expr list
  | Tconstr of (Path.t * type_declaration) * type_expr list (* * abbrev_memo ref *)
(*
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar
  | Tpoly of type_expr * type_expr list
*)

(* Type definitions *)

and type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_manifest: type_expr option;
    type_variance: (bool * bool * bool) list }
            (* covariant, contravariant, weakly contravariant *)

and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list * private_flag
  | Type_record of (string * mutable_flag * type_expr) list
                 * record_representation * private_flag

(* type equality *)
val equal : type_expr -> type_expr -> bool

(* printers *)
val reset_names : unit -> unit
val print_path : Format.formatter -> Path.t -> unit
val print : Format.formatter -> type_expr -> unit
