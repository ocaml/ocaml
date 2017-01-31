effect E : unit

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
  match foo 10 with
  | x -> ()
  | effect E k -> 
      print_endline "(** get_continuation_callstack **)";
      let bt = Printexc.get_continuation_callstack k 100 in
      print_string @@ Printexc.raw_backtrace_to_string bt;
      continue k ()

let _ = baz () 
