external mycallback1 : ('a -> 'b) -> 'a -> 'b = "mycallback1"
external mycallback2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = "mycallback2"
external mycallback3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd = "mycallback3"

let rec tak (x, y, z) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let tak2 x (y, z) = tak (x, y, z)

let tak3 x y z = tak (x, y, z)

let raise_exit () = (raise Exit : unit)

let trapexit () =
  begin try
    mycallback1 raise_exit ()
  with Exit ->
    ()
  end;
  tak (18, 12, 6)

external mypushroot : 'a -> ('b -> 'c) -> 'b -> 'a = "mypushroot"

let tripwire () =
  let s = String.make 5 'a' in
  mypushroot s trapexit ()

let _ =
  print_int(mycallback1 tak (18, 12, 6)); print_newline();
  print_int(mycallback2 tak2 18 (12, 6)); print_newline();
  print_int(mycallback3 tak3 18 12 6); print_newline();
  print_int(trapexit ()); print_newline();
  print_string(tripwire ()); print_newline()

