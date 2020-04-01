let p : Format.formatter -> int -> unit = fun fmt n ->
  (* We use `max_printer_depth` to tweak the output so that
     this test shows that the printer not only compiles
     against the debugger's code, but also uses its state. *)
  for _i = 1 to min n !Printval.max_printer_depth do
    Format.pp_print_string fmt "S ";
  done;
  Format.pp_print_string fmt "O"
