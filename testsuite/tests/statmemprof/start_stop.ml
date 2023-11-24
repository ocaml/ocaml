(* TEST *)

(* Tests various valid and invalid orderings of start/stop/discard
statmemprof calls. Doesn't test any callbacks or count any allocations,
etc.*)

module MP = Gc.Memprof

let prof () = MP.start  ~sampling_rate:1. MP.null_tracker

(* Null test: start/stop/discard *)
let _ =
  let profile = prof () in
  MP.stop ();
  MP.discard profile;
  print_endline "Null test."

(* Stop without starting *)
let _ = try
  MP.stop ()
with
  Failure s -> Printf.printf "Stop without starting fails with \"%s\"\n" s

(* Second start without stopping. *)
let _ =
  try
    Fun.protect ~finally:MP.stop
      (fun () -> (ignore (prof ());
                  ignore (prof ())))
  with
    Failure s -> Printf.printf "Start without stopping fails with \"%s\"\n" s

(* Discard without stopping. *)
let _ =
  try
    Fun.protect ~finally:MP.stop
      (fun () -> MP.discard (prof()))
  with
    Failure s -> Printf.printf "Discard without stopping fails with \"%s\"\n" s

(* Discard same profile twice. *)
let _ =
  let profile = prof () in
  MP.stop ();
  MP.discard profile;
  try
      MP.discard profile;
  with
    Failure s -> Printf.printf "Second discard fails with \"%s\"\n" s

(* Double profile *)
let _ =
  ignore (prof ());
  MP.stop ();
  ignore (prof ());
  MP.stop ();
  print_endline "Double profile."

(* Double profile with intervening discard *)
let _ =
  let prof1 = prof () in
  MP.stop ();
  MP.discard prof1;
  ignore (prof ());
  MP.stop ();
  print_endline "Double profile with single discard."

(* Double profile, both discarded *)
let _ =
  let prof1 = prof () in
  MP.stop ();
  MP.discard prof1;
  let prof2 = prof () in
  MP.stop ();
  MP.discard prof2;
  print_endline "Double profile, discarding both."

(* Double profile, discard both at end *)
let _ =
  let prof1 = prof () in
  MP.stop ();
  let prof2 = prof () in
  MP.stop ();
  MP.discard prof1;
  MP.discard prof2;
  print_endline "Double profile, discarding both at end."

(* Double profile, discard in reverse order *)
let _ =
  let prof1 = prof () in
  MP.stop ();
  let prof2 = prof () in
  MP.stop ();
  MP.discard prof2;
  MP.discard prof1;
  print_endline "Double profile, discarding in reverse order."

(* Double profile, discard first while second is sampling *)
let _ =
  let prof1 = prof () in
  MP.stop ();
  let prof2 = prof () in
  MP.discard prof1;
  MP.stop ();
  MP.discard prof2;
  print_endline "Discarding old profile while sampling."
