(* TEST
   flags = "-json"
*)

let None = None
let kas a = match a with
  | "a" -> String.uppercase a
  | "d" -> String.lowercase a
