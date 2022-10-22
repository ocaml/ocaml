(* TEST
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
  with Not_found | Failure _ -> 0

let (list_size, num_domains, niters) =
  if test_size >= 2 then (14, 8, 20) else (13, 4, 5)

(* Don't run the test if we have only 2 cores available, it times out often. *)

let _  =
  if test_size <= 1 then begin print_endline "ok"; exit 0 end

let rec burn l =
  if List.hd l > list_size then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_parallel_spawn () =
  for i = 1 to niters do
    Array.init num_domains (fun _ -> Domain.spawn (fun () -> burn [0]))
    |> Array.iter join
  done

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

  test_parallel_spawn ();

  Atomic.set running false;
  join domain_minor_gc;
  join domain_set_gc;
  join domain_major_gc;
  join domain_set_gc2;

  print_endline "ok"
