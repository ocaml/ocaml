(* TEST
 ocamlrunparam += "l=100000";
 no-tsan; (* TSan does not support call stacks bigger than 64k frames *)
 {
   bytecode;
 }
 {
   native;
 }
*)

let rec f x =
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in  let x = x+x in  let x = x+x in  let x = x+x in
  let x = x+x in
  let _ = f x in
  ()

let _ =
  try f 1
  with Stack_overflow -> Printf.printf "OK\n"
