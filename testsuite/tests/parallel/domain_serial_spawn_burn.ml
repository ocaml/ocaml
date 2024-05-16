(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

open Domain

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 2


(* This test looks to spawn domains while doing a bunch of explicit
   minor and major GC calls from parallel domains *)

let list_size =
  if test_size = 2 then 14
  else 15

let rec burn l =
  if List.hd l > test_size then ()
  else
    burn (l @ l |> List.map (fun x -> x + 1))

let test_serial_domain_spawn () =
  for i = 1 to 10 do
    let d = Domain.spawn (fun () -> burn [0]) in
    join d
  done

let () =
  let running = Atomic.make true in
  let rec run_until_stop fn () =
    while Atomic.get running do
      fn ();
    done
  in

  let domain_minor_gc =
    Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.minor ()))
  in
  let domain_major_gc =
    Domain.spawn (run_until_stop (fun () -> burn [8]; Gc.major ()))
  in
  let domain_serial_domain_spawn = Domain.spawn test_serial_domain_spawn in

  Unix.sleep 3;

  Atomic.set running false;
  join domain_minor_gc;
  join domain_major_gc;
  join domain_serial_domain_spawn;

  print_endline "ok"
