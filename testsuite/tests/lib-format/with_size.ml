(*TEST
expect;
*)

let () = Format.set_geometry ~margin:10 ~max_indent:9
let test fmt =
  Format.printf "@[";
  Format.kfprintf (fun ppf -> Format.fprintf ppf "@]@.-----------@.") Format.std_formatter
    fmt;;
[%%expect {|
val test : ('a, Format.formatter, unit, unit) format4 -> 'a = <fun>
|}]

let () = test "foo@ @<1000>bar"
let () = test "foo@ @<1000>%s" "bar"
let () = test "foo@ @<1000>%a" Format.pp_print_string "bar"
let () = test "foo@ %(%)%(%s%)" ("@<1000>": _ format6) ("%s": _ format6) "bar"
let () =
  test "foo@ %a%a"
    Format.pp_with_size 1000 Format.pp_print_string "bar";;
[%%expect {|
foo
bar
-----------
foo
bar
-----------
foo
bar
-----------
foo
bar
-----------
foo
bar
-----------
|}]


let () = test "foo@ @<0>bar@ baz"
let () = test "foo@ @<0>%s@ baz" "bar"
let () = test "foo@ @<0>%a@ baz" Format.pp_print_string "bar"
let () =
  test "foo@ %a%a@ baz"
    Format.pp_with_size 0 Format.pp_print_string "bar"
let () =
  test "foo@ %(%)%(%s%)@ baz"
    ("@<0>": _ format6) ("%s": _ format6) "bar";;
[%%expect {|
foo bar baz
-----------
foo bar baz
-----------
foo bar baz
-----------
foo bar baz
-----------
foo bar baz
-----------
|}]
