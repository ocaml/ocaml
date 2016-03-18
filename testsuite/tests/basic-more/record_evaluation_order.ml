
type r =
  { a : unit;
    b : int;
    c : char;
    d : float; }

let r1 =
  {
    c = (print_endline "c1"; 'c');
    a = print_endline "a1";
    d = (print_endline "d1"; 1.);
    b = (print_endline "b1"; 2);
  }

let r2 =
  {
    b = (print_endline "b2"; 2);
    d = (print_endline "d2"; 1.);
    a = print_endline "a2";
    c = (print_endline "c2"; 'c');
  }

let r3 =
  { (print_endline "default"; r1) with
    d = (print_endline "d3"; 1.);
    c = (print_endline "c3"; 'c');
    a = print_endline "a3";
  }

