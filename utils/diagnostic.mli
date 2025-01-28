(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


 (** The definition of a representation scheme for a type *)
type 'a t
type 'a diagnostic = 'a t

(** Version and update types *)
type 'a update = 'a Diagnostic_history.update
type version = Diagnostic_history.version

type ('id,'a) field
(** A field of type ['a] for the a ['id log]. *)

(** Embeded sum and record types *)
type !'id sum
type !'a record

(** Extensions identify specific custom types in order to let log backends
    implement ad-hoc printing functions for those types.*)
type _ extension = ..

(** Basic type representation *)
type 'a typ =
  | Unit: unit typ   (** Primitive types *)
  | Bool: bool typ
  | Int: int typ
  | Float: float typ
  | String: string typ

  | List: 'a typ -> 'a list typ (** Combinators *)
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  (** Specialize (2,3,4)-tuples to avoid defining tuples as heterogeneous
      lists*)
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ
  (** Nominal types *)
  | Sum: 'a t -> 'a sum typ
  | Record: 'id t -> 'id record typ
  (** Bridge between OCaml type and reflected types *)
  | Custom: {
      id :'b extension;
      pull: (Diagnostic_history.version option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ

(** {2:diagnostic_definition Definitions of new nominal diagnostic types }*)
module type Def = sig
  (** version type tag *)
  type vl

  (** type tag for the definition *)
  type id

  (** Record field or variant constructor *)
  type 'a label

  type definition
  type t = id diagnostic
  type raw_type = definition

  val scheme: t
  val raw_type: definition typ

  (** Common functions on both record fields and variant constructors *)
  val deprecate: vl update -> 'a label -> 'a label
  val delete: vl update -> 'a label -> 'a label
  val seal: vl update -> unit
end

module type Record = sig

  (** type tag for the record *)
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition = id record
     and type 'a label := 'a field

  (** {1:record_field_definition Field definition } *)

  val new_field:
    ?opt:bool -> ?desc:string -> vl update -> string -> 'a typ -> 'a field
  (** [new_field ?opt ?desc u name typ] creates a new field named [name] for the
      record at update [u] with an optional description [?desc]. The field is
      optional if [opt] is [Some true] *)

  val new_field_opt: ?desc:string -> vl update  -> string -> 'a typ -> 'a field
  (** [new_field_opt] is a short-hand for [new_field ~opt:true] *)

  (** {1:record_field_update Field update } *)
  val make_required: vl update -> 'a field -> unit
  (** [make_required u f] marks the field [f] as required starting with update
      [u]. This is not a breaking change.*)

  (** {1:record_creation Record creation } *)

  (** a [record_fragment] represents a potential binding of a field to a
      value *)
  type record_fragment

  val make: version option -> record_fragment list -> definition
  (** [make vo fragments] construct a [definition] record. If [vo=Some v], the
      constructed record is compatible with the definition at version [v]. *)

  val (^=): 'a field -> 'a -> record_fragment
  (** [f ^= v] constructs a [record_fragment] for the field [f] with value
      [v]. *)

  val (^=?): 'a field -> 'a option -> record_fragment
  (** [ f ^=? Some v] is [f ^= v] while [ f ^=? None] is an empty record
      fragment. *)

end

module type Sum = sig
  (** type tag *)
  type id

  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor

  (** [app vo constructor arg] constructs a [definition] variant by applying
      [constructor] to its argument [arg].

      If [vo=Some v], the constructed variant is expanded to be exactly as
      defined in version [v]: arguments and related constructors are expanded to
      fit the definition at version [v]. *)
  val app: Diagnostic_history.version option -> 'a constructor -> 'a -> raw_type


  (** {1:constructor_creation Constructor creation }*)
  val new_constr:
    ?desc:string -> vl update -> string -> 'a typ -> 'a constructor
  val new_constr0: ?desc:string -> vl update -> string -> unit constructor

  (** {1:constructor_updates Constructor updates } *)
  val refine:
    ?desc:string -> vl update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  (** [refine u parent_constr conv name new_type] creates a derived constructor
      [name] with argument of type [new_type] from the [parent_constr]
      constructor. The [conv] function is used to convert the new constructor to
      its approximation as a [patent_constr] when the constructor is viewed at a
      version [v] preceding the update [u]. *)

  val expand:
    vl update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor
  (** [expand u constr conv new_typ] expands the argument of the constructor
      [constr] to a [new_typ] record where the previous argument is stored in
      the [contents] field to provide a view at versions [v] preceding [u].*)

  (** [publish u constr] for a derived constructor [constr] make public the new
      constructor and remove the approximative view when viewed at versions [v]
      posterior to [u]. *)
  val publish: vl update -> 'a constructor -> 'a constructor

end

(** Information about nominal types *)
module type Info = sig
  type vl
  val name: string
  val description: string
  val update: vl update
end

module New_record (Vl:Diagnostic_history.S):
  (Info with type vl:=Vl.id)-> () -> (Record with type vl := Vl.id)
module New_sum (Vl:Diagnostic_history.S):
  (Info with type vl:=Vl.id) -> () -> (Sum with type vl := Vl.id)


(** {2 Instrospection } *)

val field_name: _ field -> string
val field_type: ('ty,_) field -> 'ty typ
val version_range: _ field -> Diagnostic_history.Lifetime.t

val record_scheme: 'a record typ -> 'a t
val record_list_scheme: 'a record list typ -> 'a t

type typed_val = V: 'a typ * 'a -> typed_val
type 'id bound_field = F: ('ty,'id) field * 'ty -> 'id bound_field
type any_typ = T: 'a typ -> any_typ
type typed_record = R: 'a t * 'a record -> typed_record
type label_metadata = {
  ltyp: any_typ;
  optional: bool;
  parent: string option;
  desc: string option;
  status:Diagnostic_history.Lifetime.t
}

module Record_introspection: sig
  val empty: unit -> 'id record
  val all_fields: 'id record -> 'id bound_field Seq.t
  val get: 'r record -> ('ty,'r) field -> 'ty option
  val dynamic_get: 'r record -> string -> typed_val option
  val set: 'r record -> version option -> field:('ty,'r) field -> 'ty -> unit
  val cons:
    'r record -> version option -> field:('ty list, 'r) field -> 'ty -> unit
  val reset: 'r record -> unit
end

val label_metadata:
  desc:string option -> optional:bool -> ?parent:string ->
  'v update -> 't typ -> label_metadata
val destruct: 'a sum -> ((string * typed_val) Array.t -> 'b) -> 'b
val field_infos:
  version:version option -> 'a t -> (string * label_metadata) list
val field_names: 'a t -> string list

val scheme_name: 'a t -> string
val scheme_description: 'a t -> string

val fields: string list -> 'a record -> (string * bool * typed_val) List.t
val is_optional: label_metadata -> bool
val field_info: 'id t -> (_,'id) field -> label_metadata option
val field_dyninfo: _ t -> string -> label_metadata option

(* {2:diagnostic_metadata Universal metadata field }*)

module Metadata_versions: sig
  include Diagnostic_history.S
  val v1: id update
end
module Metadata: Record with type vl := Metadata_versions.id
val universal_metafield: unit -> (Metadata.id record, 'id) field
val metakey: string * label_metadata
