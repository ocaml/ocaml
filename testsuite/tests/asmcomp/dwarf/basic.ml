(* TEST
 flags = "-g";
 native;
*)

(* Test basic DWARF emission with -g flag *)
let add x y = x + y

let () = Printf.printf "%d\n" (add 10 20)
