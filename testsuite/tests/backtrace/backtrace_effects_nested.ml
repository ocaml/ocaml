effect E : unit
effect Inc : unit

let blorp () = 
  perform Inc;
  perform E;
  42


let baz () =
    match
      blorp ()
    with
    | x -> x
    | effect Inc k -> 1 + continue k ()

let f () =
  match baz () with
  | x -> Printf.printf "%d\n" x
  | effect E k ->
     Printexc.(get_continuation_callstack k 100 |> raw_backtrace_to_string |> print_string);
     continue k ()

let () = f ()
