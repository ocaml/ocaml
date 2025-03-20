(* TEST
   expect;
*)

let f = fun ?(call_loc = [%call_loc]) () -> call_loc
[%%expect{|
val f : ?call_loc:[%call_loc] -> unit -> textloc = <fun>
|}]

let _ = f ();;
[%%expect{|
- : textloc = <location: "", line 1, bytes 8-12>
|}]

let j = (f : unit -> textloc);;
[%%expect{|
val j : unit -> textloc = <fun>
|}]

let g = fun ?(a = [%call_loc]) ?(c = 0) ?(b = [%call_loc]) () -> a, b
[%%expect{|
val g :
  ?a:[%call_loc] -> ?c:int -> ?b:[%call_loc] -> unit -> textloc * textloc =
  <fun>
|}]

let _ = g () ;;
[%%expect{|
- : textloc * textloc =
(<location: "", line 1, bytes 8-12>, <location: "", line 1, bytes 8-12>)
|}]

let h ?(a = [%call_loc]) ?(b = [%call_loc]) ()
  : textloc * textloc
  = a, b
[%%expect{|
val h : ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> textloc * textloc = <fun>
|}]

(* Partial application *)
let x = h ~b:Textloc.dummy;;
[%%expect{|
val x : ?a:[%call_loc] -> unit -> textloc * textloc = <fun>
|}]

let y = x ();;
[%%expect{|
val y : textloc * textloc =
  (<location: "", line 1, bytes 8-12>, <location: "", line 0, bytes -1--1>)
|}]

let k = (f : unit -> textloc);;
[%%expect{|
val k : unit -> textloc = <fun>
|}]

let _ = j ();;
[%%expect{|
- : textloc = <location: "", line 1, bytes 9-10>
|}]

let _ = k ();;
[%%expect{|
- : textloc = <location: "", line 1, bytes 9-10>
|}]

let m ?(call_loc = [%call_loc]) = ()
[%%expect {|
Line 1, characters 8-16:
1 | let m ?(call_loc = [%call_loc]) = ()
            ^^^^^^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

val m : ?call_loc:[%call_loc] -> unit = <fun>
|}]
