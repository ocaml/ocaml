(* TEST
*)

let check_contents (h: (string, int) Hashtbl.t)
                   (expected: (string * int) list) =
  List.iter
    (fun (k, v) -> assert (Hashtbl.find_opt h k = Some v))
    expected;
  List.iter
    (fun k -> assert (Hashtbl.find_opt h k = None))
    [""; "n"; "no"; "non"; "none"];
  Hashtbl.iter
    (fun k v -> assert (List.assoc_opt k expected = Some v))
    h

let check_failure (h: (string, int) Hashtbl.t) =
  try
    ignore (Hashtbl.find_opt h ""); assert false
  with Invalid_argument _ ->
    ()

let check_table supported h expected =
  if supported
  then check_contents h expected
  else check_failure h;
  check_contents (Hashtbl.rebuild h) expected

(* Hash table version 1, produced with OCaml 3.12.1 *)
let h1 : (string, int) Hashtbl.t =
  Marshal.from_string
    "\132\149\166\190\000\000\000/\000\000\000\n\000\000\000+\000\000\000)\
     \160D\b\000\0004\000@@@@@\176%threeC@@@@\176#twoB@@@\176$fourD\176#oneA@"
  0

(* Hash table version 2, produced with OCaml 4.09.0 *)
let h2 : (string, int) Hashtbl.t =
  Marshal.from_string
    "\132\149\166\190\000\000\000;\000\000\000\012\000\000\0008\000\000\0004\
     \192E\b\000\000@\000@@@@@@@@@\176$septG\176#sixF@\176$cinqE@\176$neufI\
     \176$huitH@@@@@@P"
  0

let _ =
  check_table false h1 ["one", 1; "two", 2; "three", 3; "four", 4];
  check_table true  h2 ["cinq", 5; "six", 6; "sept", 7; "huit", 8; "neuf", 9]
