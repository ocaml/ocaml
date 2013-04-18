type t =
  | A
  | B [@IFDEF DEBUG]

[%%IFDEF DEBUG]
let debug s = prerr_endline ([%GETENV DEBUG] ^ ":" ^ s)
[%%ELSE]
let debug _ = ()
[%%END]

let () = debug "ABC"

let () =
  Printf.printf "compiled by user %s in directory %s\n%!"
    [%GETENV USER]
    [%GETENV PWD]
