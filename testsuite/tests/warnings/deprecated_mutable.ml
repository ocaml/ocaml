(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

type t = {mutable x : int [@deprecated_mutable]}

let y : t = {x = 5}

let () = y.x <- 42

(* TEST
 flags = "-w +A-70";
 bytecode;
*)
