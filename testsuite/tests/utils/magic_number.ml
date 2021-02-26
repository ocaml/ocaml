(* TEST
include config
binary_modules = "config build_path_prefix_map misc"
* bytecode
*)

open Misc
open Magic_number

(* sanity checking: the magic number at a given kind can be parsed back *)
let error kind test =
  fatal_errorf
    "Internal compiler error (%s): there is a magic number mismatch on kind %s"
    test
    (string_of_kind kind)

let check_raw_kind kind =
  let valid =
    match parse_kind (raw_kind kind) with
      | None -> false
      | Some kind_roundtrip ->
         kind_roundtrip = kind
  in
  if not valid then error kind "raw_kind"

let check_current_raw kind =
  let valid =
    match parse (current_raw kind) with
      | Error _ -> false
      | Ok magic ->
         magic.kind = kind
         && raw magic = current_raw kind
  in
  if not valid then error kind "current_raw"

let () =
  all_kinds
  |> List.iter (fun kind -> check_raw_kind kind; check_current_raw kind)
