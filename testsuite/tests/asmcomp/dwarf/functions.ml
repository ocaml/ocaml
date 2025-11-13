(* TEST
 flags = "-g";
 native;
*)

(* Test DWARF emission for multiple functions *)
let multiply a b = a * b

let factorial n =
  let rec loop acc i =
    if i <= 1 then acc
    else loop (multiply acc i) (i - 1)
  in
  loop 1 n

let () = Printf.printf "%d\n" (factorial 5)
