(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   exit_status = "2"
*)

open Effect
open Effect.Deep

type _ t += E : unit t

let bar i =
  if i < 0 then begin
    print_endline "(** raise **)";
    raise Exit
  end else begin
    print_endline "(** get_callstack **)";
    let bt = Printexc.get_callstack 100 in
    print_string @@ Printexc.raw_backtrace_to_string bt;
    perform E;
    20
  end

let foo i =
  ignore @@ bar i;
  bar (-1)

let baz () =
  match_with foo 10
  { retc = (fun x -> ());
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun (k : (a, _) continuation) ->
          print_endline "(** get_continuation_callstack **)";
          let bt = Deep.get_callstack k 100 in
          print_string @@ Printexc.raw_backtrace_to_string bt;
          continue k ())
      | _ -> None }

let _ = baz ()
