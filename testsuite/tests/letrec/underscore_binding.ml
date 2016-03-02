let p = print_endline

(* Test "_" bindings in `let rec` constructs. All bindings produce the same
 * effect to avoid testing evaluation order. *)
let test =
  let rec _ = p "1"; p "2"
      and x = p "1\n2"; 3
      and _ = p "1\n2"
      and y = 17
      and _ = ()
   in
   assert (x == 3); assert (y == 17);
   ()
