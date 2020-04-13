(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Domain
open Domain.Sync

let rec await r =
  if Atomic.get r then () else (cpu_relax(); await r)

external critical_adjust : int -> unit
  = "caml_ml_domain_critical_section"

let go () =
  let in_crit = Atomic.make false in
  (* notify does actually notify *)
  let d = spawn (fun () ->
    critical_section (fun () ->
      Atomic.set in_crit true;
      wait ();
      Atomic.set in_crit false)) in
  await in_crit;
  notify (get_id d);
  (* notify does not return early *)
  assert (not (Atomic.get in_crit));
  join d;
  (* notify works even if it arrives before wait *)
  let entered_crit = Atomic.make false in
  let woken = Atomic.make false in
  let d = spawn (fun () ->
    critical_section (fun () ->
      Atomic.set entered_crit true;
      await woken;
      wait ())) in
  await entered_crit;
  Atomic.set woken true;
  notify (get_id d);
  join d;
  (* a single notification wakes all waits in a single crit sec *)
  let entered_crit = Atomic.make false in
  let in_second_crit = Atomic.make false in
  let d = spawn (fun () ->
    critical_section (fun () ->
      Atomic.set entered_crit true;
      wait ();
      wait ());
    critical_section (fun () ->
      Atomic.set in_second_crit true;
      wait ();
      Atomic.set in_second_crit false)) in
  await entered_crit;
  notify (get_id d);
  await in_second_crit;
  join (spawn (fun () -> ())); (* some busywork *)
  assert (Atomic.get in_second_crit);
  notify (get_id d);
  (* interrupt returns only after crit ends *)
  assert (not (Atomic.get in_second_crit));
  join d;
  (* critical sections end at termination *)
  let in_crit = Atomic.make false in
  let d = Domain.spawn (fun () ->
     critical_adjust (+1);
     Atomic.set in_crit true;
     wait ();
     Atomic.set in_crit false) in
  await in_crit;
  for i = 1 to 10000 do
    assert (Atomic.get in_crit)
  done;
  notify (get_id d);
  assert (not (Atomic.get in_crit));
  join d


let () =
  for i = 1 to 1000 do go () done;
  print_endline "ok"
