(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

open Effect

type e = effect E : unit

let e = Effect.create ()

let bar i =
  if i < 0 then begin
    print_endline "(** raise **)";
    raise Exit
  end else begin
    print_endline "(** get_callstack **)";
    let bt = Printexc.get_callstack 100 in
    print_string @@ Printexc.raw_backtrace_to_string bt;
    Effect.perform e E;
    20
  end

let foo i =
  ignore @@ bar i;
  bar (-1)

let baz () =
  Effect.run_with e foo 10
    { result = (fun x -> ());
      exn = (fun e -> raise e);
      operation =
        (fun (type a) (E : (a, e) operation) (k : (a, _) continuation) ->
          print_endline "(** get_continuation_callstack **)";
          let bt = Effect.get_callstack k 100 in
          print_string @@ Printexc.raw_backtrace_to_string bt;
          continue k ()) }

let _ = baz ()

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 exit_status = "2";
 {
   reference = "${test_source_directory}/backtrace_effects.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/backtrace_effects.opt.reference";
   native;
 }
*)
