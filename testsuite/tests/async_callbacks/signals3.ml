(* TEST
   arguments = "signals3.ml"
   * bytecode
     reference = "${test_source_directory}/signals3.byte.reference"
   * native
     reference = "${test_source_directory}/signals3.opt.reference"

*)

type pass = Pass | Fail

let report b =
  print_endline (if b == Pass then "Passed" else "Failed")

let check_pending () = ignore (Sys.opaque_identity (ref 1))

external record_signal : int -> unit = "caml_sys_record_signal"

let test_catch result f =
  try f () with Sys.Break -> report result

(* polling breaks the loop *)
let test1 () =
  test_catch Pass @@ fun () ->
  record_signal Sys.sigint ;
  while true do check_pending () done ;
  assert false

(* no more signal after test1 and test5 *)
let test2 () =
  test_catch Fail @@ fun () ->
  check_pending () ;
  report Pass

type _mask_kind = Mask_none | Mask_uninterruptible | Mask_nonpreemptible

external set_mask_prim : _mask_kind -> _mask_kind = "caml_sys_set_mask" [@@noalloc]
external unset_mask_prim : _mask_kind -> unit = "caml_sys_unset_mask" [@@noalloc]

let set_mask () = ignore (set_mask_prim Mask_uninterruptible)
let unset_mask () = unset_mask_prim Mask_none

(* set_mask and unset_mask *)
let test3 () =
  test_catch Pass (fun () ->
    ignore (set_mask ()) ;
    test_catch Fail (fun () ->
      record_signal Sys.sigint ;
      check_pending () ;
    ) ;
    unset_mask () ;
    check_pending () ;
    report Fail
  )

let reset_mask () =
  try unset_mask () ; check_pending ()
  with _ -> ()

(* same with Sys.mask *)
let test4 () =
  test_catch Pass (fun () ->
    Sys.mask (fun () ->
      test_catch Fail (fun () ->
        record_signal Sys.sigint ;
        check_pending () ;
      )
    ) () ;
    check_pending () ;
    report Fail
  )

let rec loop n =
  if n = 0 then ()
  else (
    record_signal Sys.sigint ;
    check_pending () ;
    loop (n - 1)
  )

(* same over a long time period *)
let test5 () =
  test_catch Pass @@ fun () ->
  Sys.mask (fun () ->
    test_catch Fail @@ fun () ->
    loop 1000
  ) () ;
  check_pending () ;
  report Fail

(* Tests critical sections : the moment before or after masking

   This is a property of the noalloc dialect in native code; it is
   expected to fail in bytecode currently because it is currently
   impossible to reason about safe points in the same way there. *)
let test6 () =
  test_catch Pass @@ fun () ->
  test_catch Fail (fun () ->
    (* critical section *)
    record_signal Sys.sigint ;
    Sys.mask check_pending ()
  ) ;
  check_pending ()

let count_chars in_channel =
  let chars = ref 0 in
  try
    while true do ignore (input_char in_channel) ; incr chars done ;
    assert false
  with
  | End_of_file -> !chars

let basic_io () =
  let chan =
    if Array.length Sys.argv <= 1 then stdin
    else open_in Sys.argv.(1)
  in
  print_int (count_chars chan) ;
  print_newline ()

(* check interaction with enter/leave_blocking_section *)
let test7 () =
  test_catch Pass @@ fun () ->
  Sys.mask (fun () ->
    record_signal Sys.sigint ;
    test_catch Fail basic_io) () ;
  check_pending () ;
  report Fail

let _ =
  Printexc.record_backtrace true ;
  Sys.catch_break true ;
  test1 () ;
  test2 () ;
  test3 () ;
  reset_mask () ;
  test4 () ;
  reset_mask () ;
  test5 () ;
  test2 () ;
  reset_mask () ;
  test6 () ;
  reset_mask () ;
  test7 () ;
  reset_mask ()
