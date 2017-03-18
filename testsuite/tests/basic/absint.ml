
(* Testing built-in implementation of abs. *)


let abs_reference n =
  if n > 0 then n else -n

let test_n n =
  let a1 = abs n in
  let a2 = abs_reference n in
  if a1 <> a2 then
    Printf.printf "Test failed: abs %d = %d, while abs_reference %d = %d"
      n a1 n a2

let () =
  for i = min_int+1 to min_int+100 do
    test_n i;
  done;
  for i = max_int-100 to max_int do
    test_n i;
  done;
  for i = -100 to 100 do
    test_n i;
  done;
