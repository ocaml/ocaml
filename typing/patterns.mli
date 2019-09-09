open Asttypes
open Typedtree
open Types

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list
(** [List.init (fun _ -> omega)] *)

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

module Head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of int
    | Lazy

  type t = desc pattern_data

  val arity : t -> int

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raises [Invalid_arg _] if [p] is an or- or an exception-pattern.  *)
  val deconstruct : pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : t

end
