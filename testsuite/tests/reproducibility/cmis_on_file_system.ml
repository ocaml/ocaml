(* TEST
  readonly_files = "cmis_on_file_system.ml cmis_on_file_system_companion.mli"
  * setup-ocamlc.byte-build-env
  ** ocamlc.byte
  compile_only = "true"
  module = "cmis_on_file_system.ml"
  flags="-bin-annot -no-alias-deps -w '-49'"
  *** script
  script= "mv cmis_on_file_system.cmt lone.cmt"
  **** ocamlc.byte
  module = "cmis_on_file_system_companion.mli"
  compile_only="true"
  ***** ocamlc.byte
  compile_only = "true"
  flags="-bin-annot -no-alias-deps -w '-49'"
  module="cmis_on_file_system.ml"
  ****** compare-binary-files
  program="cmis_on_file_system.cmt"
  program2="lone.cmt"
*)


(** Test that we are not recording the cmis present on the file system
    at a given point in time *)
type t = int
let () = ()

module M = Cmis_on_file_system_companion
