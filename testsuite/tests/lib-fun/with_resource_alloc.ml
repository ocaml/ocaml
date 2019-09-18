(* TEST
   * native
*)

exception Stat of Gc.stat

let count_alloc f =
  let gc_before_stat = Gc.stat () in
  let gc_before_minor = Gc.minor_words () in
  let gc_after =
    try f (); Gc.stat () with
    | Stat x -> x
  in
  let result =
    if { gc_after with stack_size = gc_before_stat.stack_size }
       = { gc_before_stat with minor_words = gc_before_minor }
    then "Passed" else "Failed"
  in
  print_endline result

let _ =
  let rec loop f n = if n = 0 then f () else loop f (n - 1) in
  let loop_ret () = loop (fun () -> ()) 10 in
  let loop_exn () = loop (fun () -> raise_notrace Exit) 10 in
  let control1 () = loop_ret () ; try loop_exn () with Exit -> () in
  let control2 () = Array.make 5 5 |> Sys.opaque_identity |> ignore in
  let test1 () =
    Fun.with_resource
      ~acquire:loop_ret ()
      ~scope:loop_ret
      ~release:loop_ret
  in
  let test2 () =
    try Fun.with_resource
          ~acquire:loop_ret ()
          ~scope:loop_exn
          ~release:loop_ret
    with Exit -> ()
  in
  print_endline "Control 1:";
  count_alloc control1 ;
  print_endline "Control 2:";
  count_alloc control2 ;
  print_endline "Test 1:";
  count_alloc test1 ;
  print_endline "Test 2:";
  count_alloc test2
