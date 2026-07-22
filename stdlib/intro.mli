module Index : sig
  (* Descriptors are the metadata that can help to print a meaningful
     representation of OCaml values. *)
  type descriptor = Obj.Tag_descriptor.t

  (* An index is a database of descriptors.
     From an OCaml value, we can get the hash of its metadata.  Looking-up the
     index turns the hash into actual metadata.

     1) Reserved bits let us get metadata hash for any value:
          Obj.get_reserved : Obj.t -> hash
     2) With index, we turn the hash into a descriptor
          Index.lookup : Index.t -> hash -> descriptor
     3) From a value and a descriptor, we can get dynval: a value tagged with
        dynamic information about its representation.
          Introspect: Obj.t -> descriptor -> Introspect.dynval
  *)
  type t

  val make : unit -> t
  val register : t -> descriptor -> unit
  val register_list : t -> descriptor list -> unit
  val lookup : t -> int -> descriptor list
  val lookup_by_reserved_bits : t -> Obj.t -> descriptor list
  val lookup_variant : t -> int -> string list

  (* Returns an index populated with descriptors of the current process.
     The function can be called multiple times, the same index will be
     returned.
     If new descriptors are available (for instance because of dynlink),
     it will also be updated. *)
  val self_index : unit -> t
end

module Introspect : sig
  type 'a fields
  val field_count : 'a fields -> int
  val field_get : 'a fields -> int -> 'a

  (* An approximation for immediate values *)
  type approx = Obj.Tag_descriptor.approx =
    | Any
    | Char
    | Int
    | Constants of string array
    | Polymorphic_variants

  (* Dynamic object: the pair of an approximation and an arbitrary ocaml value.
     If it is immediate, the approximation is used to get better printing.
     If it is a block, an index can be used to recover printing information. *)
  type dynobj
  val get_approx : dynobj -> approx
  val get_obj : dynobj -> Obj.t
  val lift : ?approx:approx -> Obj.t -> dynobj

  (* Dynval: a view on OCaml values that is easy to print nicely. *)
  type dynval =
    | String of string
    | Float of float
    | Char of char
    | Int_or_constant of int * string list
    | Constant of string list
    | Array of dynobj fields
    | Tuple of { name : string; fields : dynobj fields; }
    | Record of { name : string; fields : (string * dynobj) fields; }
    | Polymorphic_variant of string * dynobj
    | Closure
    | Lazy
    | Abstract
    | Custom
    | Unknown

  (* Introspect an object, without using an index *)
  val raw_dynval : Obj.t -> dynval

  (* Introspect an object using a user-provided index *)
  val dynval : Index.t -> dynobj -> dynval

  (* Introspect an object [Index.self_index] *)
  val self_dynval : dynobj -> dynval
end
