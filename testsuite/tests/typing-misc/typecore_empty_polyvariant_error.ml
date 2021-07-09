(* TEST
  readonly_files = "empty_ppx.ml"
  * setup-ocamlc.byte-build-env
  ** ocamlc.byte with ocamlcommon
  all_modules="empty_ppx.ml"
  program="ppx.exe"
  *** toplevel
  all_modules="${test_file}"
  flags="-ppx '${ocamlrun} ${test_build_directory_prefix}/ocamlc.byte/ppx.exe'"
*)

type t = [%empty_polyvar];;
let f: 'a. t -> 'a = function #t -> . ;;
