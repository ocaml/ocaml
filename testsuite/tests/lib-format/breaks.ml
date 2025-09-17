(* TEST
  expect;
*)

let test fmt =
  let open Format in
  let ppf = std_formatter in
  set_geometry ~margin:16 ~max_indent:15;
  pp_open_hovbox ppf 0;
  kfprintf (fun ppf -> pp_close_box ppf (); pp_print_newline ppf ()) ppf fmt
let newline ppf = Format.(pp_print_break ppf pp_infinity 0)
[%%expect {|
val test : ('a, Format.formatter, unit, unit) format4 -> 'a = <fun>
val newline : Format.formatter -> unit = <fun>
|}]

let () = test "0.@ 3.5@ 7..A@;<10 0>N"
[%%expect {|
0. 3.5 7..A
N
|}]

let () = test "0....5@ 7..A%tN" newline
[%%expect {|
0....5 7..A
N
|}]
