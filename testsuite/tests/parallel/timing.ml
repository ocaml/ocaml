(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Domain
open Domain.Sync

let millis_since event =
  Int64.(to_int (div (sub (timer_ticks ()) event) (of_int 1_000_000)))

let after_millis n event =
  Int64.(add event (mul (of_int n) (of_int 1_000_000)))

(* Waiting until a time that already passed should not take long. *)
let () =
  let start = timer_ticks () in
  for i = 0 to 2000 do
    critical_section (fun () ->
      let r = wait_until start in
      assert (r = Timeout));
  done;
  assert (millis_since start < 1000) (* can spuriously fail, in principle *)

(* Waiting for a negative interval should not take long. *)
let () =
  let start = timer_ticks () in
  for i = 0 to 2000 do
    critical_section (fun () ->
      let r = wait_for (Int64.of_int (-50_000_000)) in
      assert (r = Timeout));
  done;
  assert (millis_since start < 1000) (* can spuriously fail, in principle *)

(* Waiting until a time 50ms in the future should take at least 50ms *)
let () =
  let start = timer_ticks () in
  critical_section (fun () ->
    let r = wait_until (after_millis 50 start) in
    assert (r = Timeout));
  assert (millis_since start >= 50)

(* Waiting for 50ms should take at least 50ms *)
let () =
  let start = timer_ticks () in
  critical_section (fun () ->
    let r = wait_for (Int64.of_int 50_000_000) in
    assert (r = Timeout));
  assert (millis_since start >= 50)

(* Test that notifications interrupt waits *)
let () =
  let start = timer_ticks () in
  let flag = Atomic.make false in
  let d1 = spawn (fun () ->
    critical_section (fun () ->
      let r = wait_until Int64.(add start (of_int 1000_000_000)) in
      assert (r = Notified);
      Atomic.set flag true)) in
  let d2 = spawn (fun () ->
    notify (get_id d1);
    assert (Atomic.get flag)) in
  join d1;
  join d2;
  assert (millis_since start < 1000)

let () = print_endline "ok"
