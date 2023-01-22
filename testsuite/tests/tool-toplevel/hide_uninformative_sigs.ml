(* TEST
   readonly_files = "foo.ml"
   flags = "-hide-uninformative-sigs"
   * expect
*)

(* uninformative signatures are not reprinted in interactive use *)
type t = Foo;;
[%%expect {|
|}];;

(* but they are when using code from a file *)
#use "foo.ml";;
[%%expect {|
type t = Foo
|}];;
