(* TEST
*)

let clear_elements a m =
  for i = 1 to m do
    let n = ((i * 34872841) lsr 13) land 0xffff in
    a.(n) <- [];
  done

let populate_elements a m =
  let nums = Array.init (1 lsl 16) (fun i -> ref i) in
  for i = 1 to m do
    let n = ((i * 34872841) lsr 13) land 0xffff in
    a.(n) <- nums.(i land 0xffff) :: a.(n);
  done

let make_arr () =
  let a = Array.init (1 lsl 16) (fun _ -> []) in
  populate_elements a 100_000;
  a

let arr = make_arr ()

let calc_sum a =
  let sum_lst xs = List.fold_left (fun acc r -> acc + !r) 0 xs in
  Array.fold_left (fun acc xs -> acc + (sum_lst xs)) 0 a

let () =
  for i = 1 to 3 do
    clear_elements arr 1000;
    populate_elements arr 100;
    Gc.full_major ();
    populate_elements arr 20_000;
    ignore (Sys.opaque_identity (calc_sum arr))
  done;

  print_endline "OK"
