val enabled : bool

module Desc : sig
  (* Descriptors are metadata that can often be recovered from OCaml values to
     print a meaningful representation. *)

  (* An approximation for immediate values *)
  type approx =
    | Any
    | Char
    | Int
    | Constants of string array
    | Polymorphic_variants

  type view =
    | Unknown
    | Array of approx
    | Tuple of { name: string; tag: int; fields: (string * approx) array }
    | Record of { name: string; tag: int; fields: (string * approx) array }
    | Polymorphic_variant
    | Polymorphic_variant_constant of string

  type t = view

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val view : t -> view

  val hash : t -> int

  val hash_variant : string -> int

  val dump : (string -> unit) -> t -> unit

  val to_string : t -> string

  external read_self_descriptors : unit -> t list =
    "caml_read_bdsc_section"

  external compiler_descriptors : unit -> t list ref =
    "caml_compiler_block_descs"
end

module Index : sig
  (* An index is a database of descriptors.
     From an OCaml value, we can get the hash of its metadata.  Looking-up the
     index turns the hash into actual metadata.

     1) Reserved bits let us get metadata hash for any value:
          Obj.get_reserved : Obj.t -> hash
     2) With index, we turn the hash into potential descriptors
          Index.lookup : Index.t -> hash -> descriptor list
     3) From a value and a descriptor, we can get dynval: a value tagged with
        dynamic information about its representation.
          Introspect: Obj.t -> descriptor -> Introspect.dynval
  *)
  type t

  val make : unit -> t
  val register : t -> Desc.t -> unit
  val register_list : t -> Desc.t list -> unit
  val lookup : t -> int -> Desc.t list
  val lookup_by_reserved_bits : t -> Obj.t -> Desc.t list
  val lookup_variant : t -> int -> string list

  (* Returns an index populated with descriptors of the current process.
     The function can be called multiple times, the same index will be
     returned.
     If new descriptors are available (for instance because of dynlink or
     dynamic compilation), the index is updated in place. *)
  val self_index : unit -> t

  (* Descriptors of dynamic libraries loaded in the current process.
     (automatically added to [self_index ()]) *)
  val dynamic_libraries : unit -> Desc.t list list
  val add_dynamic_library : Desc.t list -> unit
end

module Dyn : sig
  (* "dynamic objects" are OCaml objects paired with metadata *)

  type 'a fields
  val field_count : 'a fields -> int
  val field_get : 'a fields -> int -> 'a

  (* Dynamic object: the pair of an approximation and an arbitrary ocaml value.
     If it is immediate, the approximation is used to get better printing.
     If it is a block, an index can be used to recover printing information. *)
  type t
  val get_approx : t -> Desc.approx
  val get_obj : t -> Obj.t
  val lift : ?approx:Desc.approx -> Obj.t -> t
  val lift_any : ?approx:Desc.approx -> 'a -> t

  (* A view on OCaml values that is easy to print nicely. *)
  type view =
    | String of string
    | Float of float
    | Char of char
    | Int_or_constant of int * string list
    | Constant of string list
    | Array of t fields
    | Tuple of { name : string; fields : (string * t) fields; }
    | Record of { name : string; fields : (string * t) fields; }
    | Polymorphic_variant of string * t
    | Extension of string * int * t fields
    | Closure
    | Lazy
    | Abstract
    | Custom
    | Unknown

  (* Introspect an object with an index.
     If no index is explicitly provided, it defaults to [Index.self_index ()] *)
  val view : ?index:Index.t -> t -> view

  (* Helpers for quick introspection *)
  val view_obj : ?index:Index.t -> ?approx:Desc.approx -> Obj.t -> view
  val view_any : ?index:Index.t -> ?approx:Desc.approx -> 'a -> view
end

module Print : sig
  (* High-level printing API *)

  val format_any : ?depth:int -> ?steps:int ref -> Format.formatter -> 'a -> unit

  val print_any : ?depth:int -> ?steps:int ref -> 'a -> unit
  val prerr_any : ?depth:int -> ?steps:int ref -> 'a -> unit
  val print_any_endline : ?depth:int -> ?steps:int ref -> 'a -> unit
  val prerr_any_endline : ?depth:int -> ?steps:int ref -> 'a -> unit
end
