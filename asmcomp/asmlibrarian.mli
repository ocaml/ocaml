(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Archiver_error

exception Error of error

val report_error: error -> unit
