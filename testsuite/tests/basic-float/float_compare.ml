(* TEST *)

let equal (x : float) (y : float) =
  x, "=", y, (x = y)
[@@inline never]

let not_equal (x : float) (y : float) =
  x, "!=", y, (x <> y)
[@@inline never]

let less_than (x : float) (y : float) =
  x, "<", y, (x < y)
[@@inline never]

let not_less_than (x : float) (y : float) =
  x, "!<", y, not (x < y)
[@@inline never]

let less_equal (x : float) (y : float) =
  x, "<=", y, (x <= y)
[@@inline never]

let not_less_equal (x : float) (y : float) =
  x, "!<=", y, not (x <= y)
[@@inline never]

let greater_than (x : float) (y : float) =
  x, ">", y, (x > y)
[@@inline never]

let not_greater_than (x : float) (y : float) =
  x, "!>", y, not (x > y)
[@@inline never]

let greater_equal (x : float) (y : float) =
  x, ">=", y, (x >= y)
[@@inline never]

let not_greater_equal (x : float) (y : float) =
  x, "!>=", y, not (x >= y)
[@@inline never]

let show (x, op, y, b) =
  print_float x;
  print_string " ";
  print_string op;
  print_string " ";
  print_float y;
  print_string ": ";
  print_endline (string_of_bool b)

let print_line () =
  print_endline "------------------"

let () = show (equal 1.0 2.0)
let () = show (equal 1.0 1.0)
let () = show (equal 2.0 1.0)
let () = show (equal 1.0 nan)
let () = print_line ()

let () = show (not_equal 1.0 2.0)
let () = show (not_equal 1.0 1.0)
let () = show (not_equal 2.0 1.0)
let () = show (not_equal 1.0 nan)
let () = print_line ()

let () = show (less_than 1.0 2.0)
let () = show (less_than 1.0 1.0)
let () = show (less_than 2.0 1.0)
let () = show (less_than 1.0 nan)
let () = print_line ()

let () = show (not_less_than 1.0 2.0)
let () = show (not_less_than 1.0 1.0)
let () = show (not_less_than 2.0 1.0)
let () = show (not_less_than 1.0 nan)
let () = print_line ()

let () = show (less_equal 1.0 2.0)
let () = show (less_equal 1.0 1.0)
let () = show (less_equal 2.0 1.0)
let () = show (less_equal 1.0 nan)
let () = print_line ()

let () = show (not_less_equal 1.0 2.0)
let () = show (not_less_equal 1.0 1.0)
let () = show (not_less_equal 2.0 1.0)
let () = show (not_less_equal 1.0 nan)
let () = print_line ()

let () = show (greater_than 1.0 2.0)
let () = show (greater_than 1.0 1.0)
let () = show (greater_than 2.0 1.0)
let () = show (greater_than 1.0 nan)
let () = print_line ()

let () = show (not_greater_than 1.0 2.0)
let () = show (not_greater_than 1.0 1.0)
let () = show (not_greater_than 2.0 1.0)
let () = show (not_greater_than 1.0 nan)
let () = print_line ()

let () = show (greater_equal 1.0 2.0)
let () = show (greater_equal 1.0 1.0)
let () = show (greater_equal 2.0 1.0)
let () = show (greater_equal 1.0 nan)
let () = print_line ()

let () = show (not_greater_equal 1.0 2.0)
let () = show (not_greater_equal 1.0 1.0)
let () = show (not_greater_equal 2.0 1.0)
let () = show (not_greater_equal 1.0 nan)
let () = print_line ()
