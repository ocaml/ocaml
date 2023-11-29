(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

open Effect

type e = effect E : unit

type inc = effect Inc : unit

let e = Effect.create ()

let inc = Effect.create ()

let blorp () =
  perform inc Inc;
  perform e E;
  42

let baz () =
  run_with inc blorp ()
    { result = Fun.id;
      exn = raise;
      operation =
        (fun (type a) (Inc : (a, inc) operation) (k : (a, _) continuation) ->
          1 + continue k ()) }

let f () =
  run_with e baz ()
    { result = (fun x -> Printf.printf "%d\n" x);
      exn = raise;
      operation =
        (fun (type a) (E : (a, e) operation) (k : (a, _) continuation) ->
          Effect.get_callstack k 100 |>
          Printexc.raw_backtrace_to_string |>
          print_string;
          continue k ()) }

let () = f ()

(* TEST
 flags = "-g";
 {
   reference = "${test_source_directory}/backtrace_effects_nested.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/backtrace_effects_nested.opt.reference";
   no-flambda;
   native;
 }{
   reference = "${test_source_directory}/backtrace_effects_nested.flambda.reference";
   flambda;
   native;
 }
*)
