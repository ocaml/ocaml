let p n = print_int n; print_newline ()
let h2 l = List.hd (List.tl l)

(* Test "_" bindings in `let rec` constructs. All bindings produce the same
 * effect to avoid testing evaluation order. *)
let test =
  let rec _ = p (h2 y); p (h2 x)
      and x = print_endline "0\n1"; 0 :: y
      and _ = p (List.hd x); p (List.hd y)
      and y = 1 :: x
      and _ = ()
  in
  assert (h2 x == 1); assert (h2 y == 0);
  ()
