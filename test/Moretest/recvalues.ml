(* Recursive value definitions *)

let _ =
  let rec x = 1 :: x in
  if match x with
       1 :: x' -> x == x'
     | _ -> false
  then print_string "Test 1: passed\n"
  else print_string "Test 1: FAILED\n";
  let one = 1 in
  let rec y = (one, one+1) :: y in  
  if match y with
       (1,2) :: y' -> y == y'
     | _ -> false
  then print_string "Test 2: passed\n"
  else print_string "Test 2: FAILED\n";
  let rec z = (Gc.minor(); (one, one+1)) :: z in
  (* Trash the minor generation *)
  for i = 0 to 50000 do ref 0 done;
  if match z with
       (1,2) :: z' -> z == z'
     | _ -> false
  then print_string "Test 3: passed\n"
  else print_string "Test 3: FAILED\n";
  exit 0

