(* TEST
   flags = " -short-paths "
   * expect
*)

(* This is currently just a regression test for the bug
   reported here: https://github.com/ocaml/ocaml/issues/9828 *)

#show list;;
[%%expect {|
type 'a list = [] | (::) of 'a * 'a list
|}];;

type 'a t;;
#show t;;
[%%expect {|
type 'a t
type 'a t
|}];;
