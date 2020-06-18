(* TEST
   modules = "signals.c"
   * hasunix
   include unix
   ** native

(* Fails on bytecode for now *)

*)

exception Alarm

external set_mask : unit -> unit = "caml_sys_set_mask" [@@noalloc]
external unset_mask : unit -> unit = "caml_sys_unset_mask" [@@noalloc]

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
   with Alarm ->
     (* critical section *)
     set_mask () ;
     blocked := true
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
  | result -> (
      (* critical section *)
      mask release_ref_no_exn () ;
      result
    )
  | exception e -> (
      (* critical section *)
      if !resource_ref = None then raise e
      else (
        let work_bt = Printexc.get_raw_backtrace () in
        mask release_ref_no_exn () ;
        Printexc.raise_with_backtrace e work_bt
      )
    )

(* control *)
let check_block_signal () =
  try
    (* critical section *)
    blocked := false ;
    mask check_urgent () ;
    blocked := true ;
    Pass
  with Alarm -> Fail

(* control, must fail *)
let check_unblock_signal () =
  try
    (* critical section *)
    blocked := false ;
    check_urgent () ;
    blocked := true ;
    Pass
  with Alarm -> Fail

let test_with_resource branch ~inside ~outside =
  let count = ref 0 in
  begin
    try
      with_unblock
        (fun () ->
           with_resource
             ~acquire:(fun () ->
               check_urgent () ;
               incr count ;
               if branch then (inside ()) )
             outside
             ~release:(fun _ ->
               if not branch then (inside ()) ;
               decr count ;
               check_urgent () )
             () )
        () ;
    with _ -> ()
  end ;
  if !count = 0 then Pass else Fail

let run_contain (f : _ -> pass) x =
  try let res = f x in blocked := true ; res
  with e -> (
      (* critical section *)
      blocked := true ;
      Printf.printf "Escaped: %s\n" (Printexc.to_string e) ;
      Fail
    )

external record_signal : int -> unit = "caml_sys_record_signal"
external process_uninterrupting : unit -> unit = "process_uninterrupting"

let iterations = ref 0

let handler s =
  let restart () =
    record_signal s;
    process_uninterrupting ();
  in
  if not !blocked then (
    decr iterations;
    if !iterations = -1 then raise Alarm
    else restart ()
  ) else restart ()

let test_all f =
  let test_up_to n =
    iterations := n ;
    record_signal Sys.sigalrm ;
    try run_contain f () with _ -> Fail
  in
  let rec test_all n =
    if test_up_to n = Fail then report Fail
    else if !iterations > 0 then report Pass
    else test_all (n+1)
  in
  test_all 0

let test_pair ~inside ~outside =
  test_all (fun () ->
    if Pass = test_with_resource true ~inside ~outside
    then test_with_resource false ~inside ~outside
    else Fail)

let () =
  Printexc.record_backtrace true ;
  blocked := true ;
  Sys.set_signal Sys.sigalrm (Signal_handle handler);
  Printf.printf "---Control 1---\n" ;
  test_all check_block_signal ;
  Printf.printf "---Control 2---\n" ;
  test_all check_unblock_signal ;
  let funs =
    [ "id", (fun () -> ())
    ; "print",
      (let null = if Sys.os_type = "Win32" then "NUL" else "/dev/null" in
       let out = open_out null in
       (fun () -> output_string out "a" ; flush out) )
    ; "openclose",
      (fun _ ->
         match open_in "with_resource_signal2.ml" with
         | f -> close_in f
         | exception Sys_error _ -> () )
    ] in
  let test_pair (x,f) (y,g) =
    Printf.printf "---Test inside=%s, outside=%s---\n" x y ;
    test_pair ~inside:f ~outside:g
  in
  List.iter (fun x -> List.iter (test_pair x) funs) funs
