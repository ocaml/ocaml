open Test2
let g i =
  if i = 10 || i = 9 then (print_endline (string_of_int i); raise Not_found)

let h = Hashtbl.create 42

let major_full_gc ()=
  for i = 1 to 15 do
    try
      let x = {
        f = 1.1 +. (float_of_int i);
        t = T (I (42+i), F 42.);
      } in
      let y = create () in

      Hashtbl.replace h i x;
      Hashtbl.replace h (i + 100) y;
      g i;
      Hashtbl.remove h i;
      Hashtbl.remove h (i + 100);
      Gc.full_major ();
      Gc.dump_heap ()
    with _ ->
      Gc.dump_heap ();
  done;
  Gc.full_major ();
  Gc.dump_heap ()

let minor_gc () =
  for i = 1 to 15 do
    try
      let x = {
        f = 1.1 +. (float_of_int i);
        t = T (I (42+i), F 42.);
      } in
      Hashtbl.replace h i x;
      g i;
      Hashtbl.remove h i;
      Gc.minor ();
      Gc.dump_heap ()
    with _ ->
      Gc.dump_heap ();
  done;
  Gc.full_major ();
  Gc.dump_heap ()


let _ =
  (* minor_gc (); *)
  major_full_gc ()
