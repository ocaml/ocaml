(* TEST
  include unix;
  hasunix;
  { bytecode; } { native; }
 *)

open Domain

(* This test looks to spawn domains while doing a bunch of explicit minor and
   major GC and Gc.set calls from parallel domains. *)

let rec set_gc l =
  if l > 16 then ()
  else
    let g1 = Gc.get() in
      Gc.set { g1 with
        minor_heap_size = ((l mod 4) + 1) * (1 lsl 18);
      };
      set_gc (l + 1)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 2

let (list_size, num_domains) =
  if test_size >= 2 then (14, 8) else (13, 4)

let rec burn l =
  if List.hd l > list_size then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_parallel_spawn () =
    Array.init num_domains (fun _ -> Domain.spawn (fun () -> burn [0]))
    |> Array.iter join

let () =
  let running = Atomic.make true in
  let rec run_until_stop fn () =
    while Atomic.get running do
      fn ();
    done
  in

  let domain_minor_gc =
    Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.minor ())) in
  let domain_major_gc =
    Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.major ())) in
  let domain_set_gc = Domain.spawn (run_until_stop (fun () -> set_gc 1; )) in
  let domain_set_gc2 = Domain.spawn (run_until_stop (fun () -> set_gc 3; )) in
  let domain_parallel_spawn = Domain.spawn test_parallel_spawn in

  Unix.sleep 3;

  Atomic.set running false;
  join domain_minor_gc;
  join domain_set_gc;
  join domain_major_gc;
  join domain_set_gc2;
  join domain_parallel_spawn;

  print_endline "ok"
