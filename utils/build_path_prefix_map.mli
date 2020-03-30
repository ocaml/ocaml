type path = string
type path_prefix = string
type error_message = string

val encode_prefix : path_prefix -> string
val decode_prefix : string -> (path_prefix, error_message) result

type pair = { target: path_prefix; source : path_prefix }

val encode_pair : pair -> string
val decode_pair : string -> (pair, error_message) result

type map = pair option list

val encode_map : map -> string
val decode_map : string -> (map, error_message) result

val rewrite_opt : map -> path -> path option
(** [rewrite_opt map path] tries to find a source in [map]
    that is a prefix of the input [path]. If it succeeds,
    it replaces this prefix with the corresponding target.
    If it fails, it just returns [None]. *)

val rewrite : map -> path -> path
