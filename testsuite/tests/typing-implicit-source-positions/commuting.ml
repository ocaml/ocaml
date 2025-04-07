(* TEST
   expect;
*)

let f = fun ?(a = [%call_pos]) ?(b = [%call_pos]) () -> a, b
[%%expect{|
val f :
  ?a:[%call_pos] ->
  ?b:[%call_pos] -> unit -> lexing_location * lexing_location = <fun>
|}]

let pos_a, _ = f ();;
let _, pos_b = f ();;
[%%expect{|
val pos_a : lexing_location = <location: "", line 1, bytes 15-19>
val pos_b : lexing_location = <location: "", line 2, bytes 15-19>
|}]

let _ = f ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : lexing_location * lexing_location =
(<location: "", line 1, bytes 15-19>, <location: "", line 2, bytes 15-19>)
|}]

(* Partial application *)
let x = f ~b:pos_b ;;
let y = x ~a:pos_a ;;
let z = y () ;;
[%%expect {|
val x : ?a:[%call_pos] -> unit -> lexing_location * lexing_location = <fun>
val y : unit -> lexing_location * lexing_location = <fun>
val z : lexing_location * lexing_location =
  (<location: "", line 1, bytes 15-19>, <location: "", line 2, bytes 15-19>)
|}]

let g = fun ?(a = [%call_pos]) ?(c = 0) ?(b = [%call_pos]) () -> a, b, c
[%%expect{|
val g :
  ?a:[%call_pos] ->
  ?c:int -> ?b:[%call_pos] -> unit -> lexing_location * lexing_location * int =
  <fun>
|}]

let _ = g ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : lexing_location * lexing_location * int =
(<location: "", line 1, bytes 15-19>, <location: "", line 2, bytes 15-19>, 0)
|}]

let h = fun ?(a = [%call_pos]) ~(b:int) () -> a, b
[%%expect{|
val h : ?a:[%call_pos] -> b:int -> unit -> lexing_location * int = <fun>
|}]

let _ = h ~b:0 ~a:pos_a ();;
[%%expect{|
- : lexing_location * int = (<location: "", line 1, bytes 15-19>, 0)
|}]

let k = fun ~(a:int) ?(a = [%call_pos])() -> a
[%%expect{|
val k : a:int -> ?a:[%call_pos] -> unit -> lexing_location = <fun>
|}]

let _ = k ~a:Textloc.dummy ~a:0 ();;
[%%expect{|
Line 1, characters 13-26:
1 | let _ = k ~a:Textloc.dummy ~a:0 ();;
                 ^^^^^^^^^^^^^
Error: The value "Textloc.dummy" has type "Textloc.t" = "lexing_location"
       but an expression was expected of type "int"
|}]

let _ = k ~a:0 ~a:Textloc.dummy ();;
[%%expect{|
- : Textloc.t = <location: "", line 0, bytes -1--1>
|}]

(* Labels on source positions can't commute in definitions *)
let m : ?a:[%call_pos] -> ?b:[%call_pos] -> unit -> unit = fun ?(b = [%call_pos]) ?(a = [%call_pos]) () -> ()
[%%expect{|
Line 1, characters 59-109:
1 | let m : ?a:[%call_pos] -> ?b:[%call_pos] -> unit -> unit = fun ?(b = [%call_pos]) ?(a = [%call_pos]) () -> ()
                                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type
         "?a:[%call_pos] -> ?b:[%call_pos] -> unit -> unit"
       but its first argument is "?(b = [%call_pos])"
       instead of "?(a = [%call_pos])"
|}]

(* Object system *)

class c ?(a = [%call_pos]) ?(b = [%call_pos]) () =
  object
    method x = a, b
  end
[%%expect{|
class c :
  ?a:[%call_pos] ->
  ?b:[%call_pos] ->
  unit -> object method x : lexing_location * lexing_location end
|}]

(* Object system partial application *)
let x = new c ~b:pos_b ;;
let y = x ~a:pos_a ;;
let a, b = (y ())#x ;;
[%%expect{|
val x : ?a:[%call_pos] -> unit -> c = <fun>
val y : unit -> c = <fun>
val a : lexing_location = <location: "", line 1, bytes 15-19>
val b : lexing_location = <location: "", line 2, bytes 15-19>
|}]

(* Labels on source positions can't commute in class definitions *)
class m : ?a:[%call_pos] -> ?b:[%call_pos] -> unit -> object end =
  fun ?(b = [%call_pos]) ?(a = [%call_pos]) () -> object end
[%%expect{|
Line 2, characters 6-60:
2 |   fun ?(b = [%call_pos]) ?(a = [%call_pos]) () -> object end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type ?b:[%call_pos] -> ?a:[%call_pos] -> unit -> object  end
       is not matched by the class type
         ?a:[%call_pos] -> ?b:[%call_pos] -> unit -> object  end
|}]

(* [%call_pos] is distinct from lexing_location *)
class c :
  a:lexing_location -> ?b:[%call_pos] -> unit -> object
    method x : lexing_location * lexing_location
  end = fun ?(a = [%call_pos]) ~b () -> object
    method x = a, b
  end
[%%expect{|
Lines 4-6, characters 12-5:
4 | ............?(a = [%call_pos]) ~b () -> object
5 |     method x = a, b
6 |   end
Error: The class type
         ?a:[%call_pos] -> b:'b -> unit -> object method x : 'a * 'b end
       is not matched by the class type
         a:lexing_location ->
         ?b:[%call_pos] ->
         unit -> object method x : lexing_location * lexing_location end
|}]
