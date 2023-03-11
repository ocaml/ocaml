(* TEST
*)

open Domain

(* This test looks to spawn domains while doing a bunch of explicit minor and major GC calls
   from parallel domains *)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

(* Don't run the test if we have only 2 cores available, it times out often. *)

let _  =
  if test_size <= 1 then begin print_endline "ok"; exit 0 end

let (list_size, num_domains) =
  if test_size >= 2 then (14, 25) else (13, 12)

let rec burn l =
  if List.hd l > list_size then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_parallel_spawn () =
  for i = 1 to 20 do
    Array.init num_domains (fun _ -> Domain.spawn (fun () -> burn [0]))
    |> Array.iter join
  done

let () =
  let running = ref true in
  let rec run_until_stop fn () =
    while !running do
      fn ();
    done
  in

  let domain_minor_gc = Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.minor ())) in
  let domain_major_gc = Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.major ())) in

  test_parallel_spawn ();

  running := false;
  join domain_minor_gc;
  join domain_major_gc;

  print_endline "ok"
