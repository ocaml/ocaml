(* $Id$ *)

val start_env : Env.t ref
val module_list : string list ref
val longident_of_path :  Path.t ->Longident.t

type pkind =
    Pvalue
  | Ptype
  | Plabel
  | Pconstructor
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype

val string_of_kind :  pkind -> string

exception Error of int * int

val search_string_type :
      string -> mode:[`exact|`included] -> (Longident.t * pkind) list
val search_pattern_symbol : string -> (Longident.t * pkind) list
val search_string_symbol : string -> (Longident.t * pkind) list

val search_structure :
    Parsetree.structure ->
    name:string -> kind:pkind -> prefix:string list -> int
val search_signature :
    Parsetree.signature ->
    name:string -> kind:pkind -> prefix:string list -> int
