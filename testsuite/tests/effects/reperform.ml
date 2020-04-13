(* TEST
 *)

effect E : int -> int
effect F : unit

let rec nest = function
  | 0 -> perform (E 42)
  | n ->
     match Printf.printf "[%d\n" n; nest (n - 1) with
     | effect F k -> assert false
     | exception e -> Printf.printf " !%d]\n" n; raise e
     | x -> Printf.printf " %d]\n" n; x

let () =
  match nest 5 with
  | effect (E n) k -> continue k (n + 100)
  | x -> Printf.printf "= %d\n" x

let () =
  match nest 5 with
  | x -> assert false
  | effect F k -> assert false
  | exception e -> Printf.printf "%s\n" (Printexc.to_string e)
