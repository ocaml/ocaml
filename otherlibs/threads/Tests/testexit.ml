(* Test Thread.exit *)

let somethread (name, limit, last) =
  let counter = ref 0 in
  while true do
    incr counter;
    if !counter >= limit then begin
      print_string (name ^ " exiting\n");
      flush stdout;
      if last then exit 0 else Thread.exit()
    end;
    print_string (name ^ ": " ^ string_of_int !counter ^ "\n");
    flush stdout;
    Thread.delay 0.5
  done

let _ =
  let _ = Thread.create somethread ("A", 5, false) in
  let _ = Thread.create somethread ("B", 8, false) in
  let _ = Thread.create somethread ("C", 11, true) in
  somethread ("Main", 3, false)

