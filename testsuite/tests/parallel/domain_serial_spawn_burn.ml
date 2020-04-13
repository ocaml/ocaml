(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Domain

(* This test looks to spawn domains while doing a bunch of explicit minor and major GC calls
   from parallel domains *)

let rec burn l =
  if List.hd l > 14 then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_serial_domain_spawn () =
  for i = 1 to 250 do
  	let d = Domain.spawn (fun () -> burn [0]) in
  	join d
  done

let test_parallel_spawn () =
  for i = 1 to 10 do
	  let a = Array.init 25 (fun _ -> Domain.spawn (fun () -> burn [0])) in
	  for j = 0 to 24 do
	  	join a.(j)
	  done
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

  test_serial_domain_spawn ();

  running := false;
  join domain_minor_gc;
  join domain_major_gc;

  print_endline "ok"
