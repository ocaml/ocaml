open Domain
open Domain.Sync

(* Since this test checks timing, it's possible for it to
   spuriously fail on a very loaded system *)

let startup = timer_ticks ()

let step = 50_000_000           (* 50 ms *)
let at n = Int64.(add startup (mul (of_int step) (of_int n)))

let check_after n =
  let late = Int64.sub (timer_ticks ()) (at n) in
  if late < Int64.zero then
    failwith ("Early by " ^ (Int64.(to_string (neg (div late (of_int 1_000_000))))) ^ " ms")

let check_before n =
  let late = Int64.sub (timer_ticks ()) (Int64.(add (at n) (of_int (step/2)))) in
  if late > Int64.zero then
    failwith ("Late by " ^ (Int64.(to_string ((div late (of_int 1_000_000))))) ^ " ms")


let flag = Atomic.make false

let d1 () =
  critical_section (fun () ->
      let r = wait_until (at (-1)) in
      assert (r = Timeout));
  check_before 1;
  critical_section (fun () ->
      let r = wait_until (at 1) in
      assert (r = Timeout));
  check_after 1;
  check_before 2;
  critical_section (fun () ->
      let r = wait_for (Int64.of_int (- step)) in
      assert (r = Timeout));
  check_before 2;
  critical_section (fun () ->
      let r = wait_for (Int64.of_int step) in
      assert (r = Timeout));
  check_after 2;
  critical_section (fun () ->
      check_before 3;
      let r = wait_until (at 4) in
      assert (r = Notified);
      Atomic.set flag true);
  check_after 3;
  check_before 4

let d2 d1 =
  critical_section (fun () ->
      let r = wait_until (at 3) in
      assert (r = Timeout));
  check_after 3;
  notify d1;
  assert (Atomic.get flag);
  check_before 4

let () =
  let d1 = spawn d1 in
  let d2 = spawn (fun () -> d2 (get_id d1)) in
  join d1;
  join d2;
  print_endline "ok"

