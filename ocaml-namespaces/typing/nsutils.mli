
type error =
  | Unbound_namespace of Longident.t
  | Namespace_expected of Longident.t
  | Invalid_namespace_name of string
  | Inconsistent_namespace of string * string

exception Error of Location.t * error
val report_error: Format.formatter -> error -> unit

(* These two functions add the namespaces into the initial environment *)
val add_structure_namespaces :
  string option (* module_name *)
  -> Env.t (* initial env *)
  -> Parsetree.structure
  -> string (* filename *)
  -> Env.t
val add_signature_namespaces :
  Env.t
  -> Parsetree.signature
  -> string (* filename *)
  -> Env.t

(* Fully qualified module name, i.e. with namespace as prefix *)
val modulename : unit -> string

(* Define the non qualified module name of the current unit *)
val set_unit_name : string -> unit

(* returns the rightest module name in the path  (X.Y.Z -> Z) *)
val basename : string -> string
