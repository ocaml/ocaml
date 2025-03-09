(* TEST *)

let[@tail_mod_cons] rec test_1 = function
  | [] -> []
  | hd :: tl ->
     try int_of_string hd :: (test_1[@tailcall false]) tl
     with Failure _ -> -1 :: (test_1[@tailcall true]) tl

let () =
  assert (test_1 ["1"; "2"; "foo"; "3"; "4"] = [1; 2; -1; 3; 4])
