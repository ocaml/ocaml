(* TEST
   expect;
*)

let f = fun ?(call_pos = [%call_pos]) () -> call_pos
[%%expect{|
val f : ?call_pos:[%call_pos] -> unit -> lexing_location = <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_location = <location: "", line 1, bytes 8-12>
|}]

let j = (f : unit -> lexing_location);;
[%%expect{|
val j : unit -> lexing_location = <fun>
|}]

let g = fun ?(a = [%call_pos]) ?(c = 0) ?(b = [%call_pos]) () -> a, b
[%%expect{|
val g :
  ?a:[%call_pos] ->
  ?c:int -> ?b:[%call_pos] -> unit -> lexing_location * lexing_location =
  <fun>
|}]

let _ = g () ;;
[%%expect{|
- : lexing_location * lexing_location =
(<location: "", line 1, bytes 8-12>, <location: "", line 1, bytes 8-12>)
|}]

let h ?(a = [%call_pos]) ?(b = [%call_pos]) ()
  : lexing_location * lexing_location
  = a, b
[%%expect{|
val h :
  ?a:[%call_pos] ->
  ?b:[%call_pos] -> unit -> lexing_location * lexing_location = <fun>
|}]

(* Partial application *)
let x = h ~b:Textloc.dummy;;
[%%expect{|
val x : ?a:[%call_pos] -> unit -> lexing_location * lexing_location = <fun>
|}]

let y = x ();;
[%%expect{|
val y : lexing_location * lexing_location =
  (<location: "", line 1, bytes 8-12>, <location: "", line 0, bytes -1--1>)
|}]

let k = (f : unit -> lexing_location);;
[%%expect{|
val k : unit -> lexing_location = <fun>
|}]

let _ = j ();;
[%%expect{|
- : lexing_location = <location: "", line 1, bytes 9-10>
|}]

let _ = k ();;
[%%expect{|
- : lexing_location = <location: "", line 1, bytes 9-10>
|}]

let m ?(call_pos = [%call_pos]) = ()
[%%expect {|
Line 1, characters 8-16:
1 | let m ?(call_pos = [%call_pos]) = ()
            ^^^^^^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

val m : ?call_pos:[%call_pos] -> unit = <fun>
|}]
