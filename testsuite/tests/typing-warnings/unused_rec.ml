(* TEST
   * expect
*)

[@@@ocaml.warning "+39"]

let rec f () = 3;;
[%%expect{|
Line 3, characters 8-9:
3 | let rec f () = 3;;
            ^
Warning 39 [unused-rec-flag]: unused rec flag.
val f : unit -> int = <fun>
|}];;

let[@warning "-39"] rec g () = 3;;
[%%expect{|
val g : unit -> int = <fun>
|}];;

let[@warning "+39"] rec h () = 3;;
[%%expect{|
Line 1, characters 24-25:
1 | let[@warning "+39"] rec h () = 3;;
                            ^
Warning 39 [unused-rec-flag]: unused rec flag.
val h : unit -> int = <fun>
|}];;

[@@@ocaml.warning "-39"]

let rec f () = 3;;
[%%expect{|
val f : unit -> int = <fun>
|}];;

let[@warning "-39"] rec g () = 3;;
[%%expect{|
val g : unit -> int = <fun>
|}];;

let[@warning "+39"] rec h () = 3;;
[%%expect{|
Line 1, characters 24-25:
1 | let[@warning "+39"] rec h () = 3;;
                            ^
Warning 39 [unused-rec-flag]: unused rec flag.
val h : unit -> int = <fun>
|}];;
