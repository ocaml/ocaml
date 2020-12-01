(* TEST
   * hasunix
   include unix
   ** native
   ** bytecode

(* Test only implemented on Unix. *)

*)


exception Alarm

type _mask_kind = Mask_none | Mask_uninterruptible | Mask_nonpreemptible

external set_mask_prim : _mask_kind -> _mask_kind = "caml_sys_set_mask" [@@noalloc]
external unset_mask_prim : _mask_kind -> unit = "caml_sys_unset_mask" [@@noalloc]

let set_mask () = ignore (set_mask_prim Mask_uninterruptible)
let unset_mask () = unset_mask_prim Mask_none

let mask : ('a -> 'b) -> 'a -> 'b = Sys.mask

let check_urgent () = ignore (Sys.opaque_identity (ref 1))

let blocked = ref true

type pass = Pass | Fail

let report b =
  print_endline (if b == Pass then "Passed" else "Failed")

let with_unblock f x =
  set_mask () ;
  (try
     blocked := false ;
     unset_mask () ;
     f x ;
     set_mask () ;
     blocked := true
   with (* BEGIN ATOMIC *) Alarm ->
     blocked := true ;
     (* END ATOMIC *)
     set_mask () ;
  ) ;
  unset_mask ()

let with_resource ~acquire ~(release : _ -> unit) work x =
  let resource_ref = ref None in
  let initialise () = resource_ref := Some (acquire x) in
  let borrow () =
    match !resource_ref with
    | None -> assert false
    | Some res -> res
  in
  let release_ref_no_exn () =
    Printexc.catch release (borrow ())
  in
  match mask initialise () ; work (borrow ()) with
  | (* BEGIN ATOMIC *) result -> (
      (* Sys.mask inlined for bytecode polling behaviour *)
      let old_mask = set_mask_prim Mask_uninterruptible in
      release_ref_no_exn () ;
      unset_mask_prim old_mask ;
      (* END ATOMIC *)
      result
    )
  | (* BEGIN ATOMIC *) exception e -> (
      if !resource_ref = None then raise e
      else (
        (* Sys.mask inlined for bytecode polling behaviour *)
        let old_mask = set_mask_prim Mask_uninterruptible in
        let work_bt = Printexc.get_raw_backtrace () in
        mask release_ref_no_exn () ;
        unset_mask_prim old_mask ;
        Printexc.raise_with_backtrace e work_bt
        (* END ATOMIC *)
      )
    )

(* Unix-specific *)

let () = if Sys.os_type <> "Unix" then failwith "not implemented"

let time = Unix.gettimeofday

(* check with_unblock *)
let check_receive_signal tick () =
  set_mask () ;
  (try
     blocked := false ;
     unset_mask () ;
     let start = time () in
     let x = ref (0,1) in
     while (time ()) -. start < 0.01 (* 10 ms *) do
       x := match !x with (a,b) -> (b,a)
     done ;
     ignore (Sys.opaque_identity !x) ;
     report Fail ;
     set_mask () ;
     blocked := true
   with (* BEGIN ATOMIC *) Alarm ->
     blocked := true ;
     (* END ATOMIC *)
     set_mask () ;
     report Pass
  ) ;
  unset_mask ()

let wait tock =
  let start = time () in
  while (time ()) -. start < tock do check_urgent () done

(* control *)
let check_block_signal tick () =
  try
    mask (fun () ->
      blocked := false ;
      wait (300. *. tick) ;
      blocked := true) () ;
    report Pass
  with (* BEGIN ATOMIC *) Alarm ->
     blocked := true ;
     (* END ATOMIC *)
     report Fail


let repeat_test tick test =
  let start = time () in
  let tock = 300.1 *. tick in
  let x = ref 0 in
  let y = ref 0 in
  let _ =
    while (time ()) -. start < tock do
      test tick true x ;
      test tick false y
    done
  in
  report (if !x = 0 && !y = 0 then Pass else Fail)

(* control *)
let repeat_control tick () =
  repeat_test tick (fun _ _ _ ->
    with_unblock (fun () -> ()) ()
  )

(* test Sys.mask *)
let test1 tick _ _ =
  let tock = 5.1 *. tick in
  with_unblock
    (fun () -> mask wait tock)
    ()

let repeat_test1 tick () = repeat_test tick test1

let test_with_resource tick branch count ~inside ~outside =
  let tock = 5.1 *. tick in
  with_unblock
    (fun () ->
    with_resource
      ~acquire:(fun () ->
        check_urgent () ;
        incr count ;
        if branch then (wait tock ; inside ()) )
      outside
      ~release:(fun r ->
        if not branch then (wait tock ; inside ()) ;
        decr count ;
        check_urgent () )
      () )
    ()

let repeat_test_with_resource tick ~inside ~outside =
  repeat_test tick (test_with_resource ~inside ~outside)

let run_contain (f : _ -> unit) x =
  try f x ; blocked := true
  with (* BEGIN ATOMIC *) e -> (
      blocked := true ;
      (* END ATOMIC *)
      Printf.printf "Escaped: %s\n" (Printexc.to_string e) ;
      report Fail
    )

let test_signal tick pid =
  (* check that signal handlers work as expected *)
  print_endline "---Control 1:---" ;
  run_contain (check_receive_signal tick) () ;
  (* check that blocking signals works as expected *)
  print_endline "---Control 2:---" ;
  run_contain (check_block_signal tick) () ;
  print_endline "---Control 3:---" ;
  run_contain (repeat_control tick) () ;
  print_endline "---Test 1:---" ;
  run_contain (repeat_test1 tick) () ;
  let funs =
    [ "id", (fun () -> ())
    ; "print",
      (let out = open_out "/dev/null" in
      (fun () -> output_string out "a" ; flush out) )
    ; "wait", (fun _ -> wait tick)
    ; "openclose",
      (fun _ ->
         match open_in "with_resource_signal.ml" with
         | f -> close_in f
         | exception Unix.Unix_error _ -> () )
    ] in
  let test_pair (x,f) (y,g) =
    Printf.printf "---Test inside=%s, outside=%s---\n" x y ;
    run_contain
      (fun () -> repeat_test_with_resource tick ~inside:f ~outside:g)
      ()
  in
  List.iter (fun x -> List.iter (test_pair x) funs) funs

let send_signals tick ppid =
  try
    while true do
      Unix.sleepf tick;
      Unix.kill ppid Sys.sigalrm
    done
  with Unix.Unix_error _ -> ()

let test_run tick =
  try
    match Unix.fork () with
    | 0 -> send_signals tick (Unix.getppid ())
    | pid -> (
        Printf.printf "\n=== Test Run, tick = %.0f us ===\n"
          (tick *. 1000000.);
        (* ensure that we receive from the other thread *)
        let alrm = [Sys.sigalrm] in
        Unix.sigprocmask SIG_UNBLOCK alrm |> ignore ;
        Unix.pause () ;
        test_signal tick pid ;
        Unix.kill pid Sys.sigkill
      )
  with e -> Printexc.print_backtrace stderr ; raise e

let _ =
  Printexc.record_backtrace true ;
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> if not !blocked then raise Alarm)) ;
  test_run 0.0001 ; (* 100 us *)
  test_run 0.00004 ; (* 40 us *)
  test_run 0.00001 ; (* 10 us *)
  test_run 0.000004 ; (* 4 us *)
  test_run 0.000001   (* 1 us *)
