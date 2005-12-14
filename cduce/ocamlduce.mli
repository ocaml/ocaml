(** Operations on x-values.

   This module provides several functionality on x-values.
   It complies with signatures [Set.OrderedType] and [Hashtbl.HashedType].
   It can thus directly be used as arguments to the [Set.Make], [Map.Make]
   and [Hashtbl.Make] functors.
*)

type t = {{ Any }}
  (** The universal type for x-values. *)

val print: Format.formatter -> {{ Any }} -> unit
  (** Pretty-print a value as in the toplevel. *)

val to_string: {{ Any }} -> string
  (** Same as [print] but returns the value in a string. *)

val hash: t -> int
  (** Hash function on x-values. *)

val equal: t -> t -> bool
  (** Equality test on x-values. *)

val compare: t -> t -> int
  (** A total ordering on x-values. *)


(** Coercions between x-integers and their x-string representation. *)
module Int : sig
  type t = {{ Int }}
  type repr = {{ [('-'|'+')? '0'--'9'+] }}

  val make: repr -> t
  val get: t -> repr  
end

(** Coercions between non-negative x-integers and their x-string representation. *)
module NonnegInt : sig
  type t = {{ 0--** }}
  type repr = {{ ['0'--'9'+] }}

  val make: repr -> t
  val get: t -> repr
end

(** Coercions between x-strings and OCaml [string] type (whose values
    are interpreted as being UTF-8 encoded). *)
module Utf8 : sig
  type t = {{ String }}

  val make: string -> t
    (** Raises the exception [Failure "Utf8.make"] if the argument
	is not a valid UTF-8 encoded string. *)
  val get: t -> string
end

(** Coercions between Latin1 x-strings and OCaml [string] type (whose values
    are interpreted as being iso-8859-1 encoded). *)
module Latin1 : sig
  type t = {{ Latin1 }}

  val make: string -> t
  val get: t -> string
end



(** Operations on namespaces. *)
module Namespace : sig
  type t
  val make: {{ String }} -> t
  val get: t -> {{ String }}

  val empty: t
  val xml: t

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

(** Operations on atoms. *)
module Atom : sig
  type t = {{ Atom }}
  type qname = Namespace.t * {{ String }}
  val make: qname -> t
  val get: t -> qname

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end


(** Operations on prefix->namespace tables. *)
module NamespaceTable : sig
  type t
    (** The type for prefix->namespace tables. *)

  val make: ({{ String }} * Namespace.t) list -> t
  val get: t -> ({{ String }} * Namespace.t) list

  val resolve_prefix: t -> {{ String }} -> Namespace.t
  val resolve_qname: t -> {{ String }} -> Atom.t
end

(** Operations on namespaces scopes (attached to XML elements).

    One namespace table can optionnaly be attached to an XML element.
*)
module NamespaceScope : sig
  type elt = {{ <(Any) (Any)>Any }}

  val get: elt -> NamespaceTable.t
    (** [get v] extracts the namespace bindings from the value [v]
	or raise [Not_found] if [v] has no attached namespace bindings. *)
    
  val set: elt -> NamespaceTable.t -> elt
    (** [set v ns] returns a value equal to [v] except for the attached 
	namespace bindings which are set to [ns]. *)

  val resolve_prefix: elt -> {{ String }} -> Namespace.t
  val resolve_qname: elt -> {{ String }} -> Atom.t
end

(** Importing XML documents. *)
module Load : sig
  (** This module provides an abstract type of XML loaders.
      An XML parser can interact with such an object to produce an x-value.
      After creating the loader with the [make] function, the XML parser
      must call [start_elem], [end_elem] and [text] for each
      corresponding event found in the input document. The loader
      object is responsible for XML Namespaces resolution.
      All the strings must be encoded in UTF-8. *)

  type t
    (** The type of loader objects. *)
  type anyxml = {{ <_ ..>[ (Char | anyxml)* ] }}
      (** The type for the values produced by the loader. *)
      
  exception Error of string
    
  val make: ?ns:bool -> unit -> t
    (** [make ~ns ()] creates a fresh loader. If [ns] is [true],
	the loader will attach a namespace bindings to each XML element. *)
  val get: t -> anyxml
    (** [get l] is to be called when the whole XML document has been parsed.
	It returns the root element of the loaded XML documents, or
	raises [Error] if the document is not well-formed. *)

  val start_elem: t -> string -> (string * string) list -> unit
    (** [start_elem l tag attr] is to be called when the XML parser
	encounters the opening markup for an XML element
	with specified tag and attributes.
	The function raises [Error] if the same attribute label
	appears several times. *)

  val end_elem: t -> 'a -> unit
    (** [end_element l x] is to be called when the XML parser
	encounters the closing markup for an XML element. *)
 
  val text: t -> string -> unit
    (** [text l txt] is to be called when the XML parser
	encounters some textual data (PCDATA). It is legal
	to call [text] several times in a row. *)

  val sub: t -> anyxml -> unit
    (** [sub l v] inserts an already parsed XML element as a subtree. *)
end

(** Exporting XML documents. *)
module Print : sig
  exception Error of string * {{ Any }}

  val serialize:
    start_elem:(string -> (string * string) list -> unit) ->
    end_elem:(string -> unit) ->
    empty_elem:(string -> (string * string) list -> unit) ->
    text:(string -> unit) ->
    ?ns:NamespaceTable.t -> 
    Load.anyxml ->
    unit
      (** [serialize ~start_elem ~end_elem ~empty_elem ~text ~ns v]
	  traverses the XML tree [v] and produce a linear
	  representation of it by calling callback functions. All
	  the strings passed to the callbacks are encoded in UTF-8.

	  The XML declaration is not displayed.

	  The function attaches bindings for XML Namespaces used in
	  the tree to the root element. The optional argument [ns] can
	  be used to provide hints to choose the prefixes.
	  
	  If the value does not represent a valid XML tree,
	  the exception [Error] is raised. This can only happen
	  if the value of some attribute is not a string.
      *)
    

  val print_xml: 
    (string -> unit) ->
    ?data:(string -> unit) ->
    ?ns:NamespaceTable.t -> 
    Load.anyxml ->
    unit
      (** [print f ~data ~ns v] produces a textual representation of
	  the XML tree [v].  The function [f] is used to display
	  markup strings (tag and attribute names, brackets). An
	  alternate function [data] can be provided to display the
	  content (attribute value and PCDATA); if not given, these
	  strings are displayed using the function [f] and with
	  escaping for the special characters &,<,> and the double quote.
	  Strings passed to [f] and [data] are encoded in UTF-8.

	  The argument [ns] has the same meaning as for [serialize].
      *)
end

