(* TEST
*)

(* filling minor heaps in parallel to trigger
   minor heap exhaustion codepath organically *)

let rec tak (x, y, z as _tuple) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
  else z

let work () =
  for _ = 1 to 100 do
    assert (7 = tak (18, 12, 6));
  done

let _ =
  let a = Array.init 4 (fun _ -> Domain.spawn work) in
  work ();
  Array.iter (fun d -> Domain.join d) a;
  print_endline "OK"
