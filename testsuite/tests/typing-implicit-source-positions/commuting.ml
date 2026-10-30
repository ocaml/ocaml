(* TEST
   expect;
*)

let f = fun ?(a = [%call_loc]) ?(b = [%call_loc]) () -> a, b
[%%expect{|
val f : ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> textloc * textloc = <fun>
|}]

let pos_a, _ = f ();;
let _, pos_b = f ();;
[%%expect{|
val pos_a : textloc = <location: File "", line 1, characters 15-19>
val pos_b : textloc = <location: File "", line 2, characters 15-19>
|}]

let _ = f ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : textloc * textloc =
(<location: File "", line 1, characters 15-19>,
 <location: File "", line 2, characters 15-19>)
|}]

(* Partial application *)
let x = f ~b:pos_b ;;
let y = x ~a:pos_a ;;
let z = y () ;;
[%%expect {|
val x : ?a:[%call_loc] -> unit -> textloc * textloc = <fun>
val y : unit -> textloc * textloc = <fun>
val z : textloc * textloc =
  (<location: File "", line 1, characters 15-19>,
   <location: File "", line 2, characters 15-19>)
|}]

let g = fun ?(a = [%call_loc]) ?(c = 0) ?(b = [%call_loc]) () -> a, b, c
[%%expect{|
val g :
  ?a:[%call_loc] ->
  ?c:int -> ?b:[%call_loc] -> unit -> textloc * textloc * int = <fun>
|}]

let _ = g ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : textloc * textloc * int =
(<location: File "", line 1, characters 15-19>,
 <location: File "", line 2, characters 15-19>, 0)
|}]

let h = fun ?(a = [%call_loc]) ~(b:int) () -> a, b
[%%expect{|
val h : ?a:[%call_loc] -> b:int -> unit -> textloc * int = <fun>
|}]

let _ = h ~b:0 ~a:pos_a ();;
[%%expect{|
- : textloc * int = (<location: File "", line 1, characters 15-19>, 0)
|}]

let k = fun ~(a:int) ?(a = [%call_loc])() -> a
[%%expect{|
val k : a:int -> ?a:[%call_loc] -> unit -> textloc = <fun>
|}]

let _ = k ~a:Textloc.Compact.none ~a:0 ();;
[%%expect{|
Line 1, characters 13-33:
1 | let _ = k ~a:Textloc.Compact.none ~a:0 ();;
                 ^^^^^^^^^^^^^^^^^^^^
Error: The value "Textloc.Compact.none" has type "Textloc.Compact.t" = "textloc"
       but an expression was expected of type "int"
|}]

let _ = k ~a:0 ~a:Textloc.Compact.none ();;
[%%expect{|
- : Textloc.Compact.t = <location: File "-">
|}]

(* Labels on source positions can't commute in definitions *)
let m : ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> unit = fun ?(b = [%call_loc]) ?(a = [%call_loc]) () -> ()
[%%expect{|
Line 1, characters 59-109:
1 | let m : ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> unit = fun ?(b = [%call_loc]) ?(a = [%call_loc]) () -> ()
                                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type
         "?a:[%call_loc] -> ?b:[%call_loc] -> unit -> unit"
       but its first argument is "?(b = [%call_loc])"
       instead of "?(a = [%call_loc])"
|}]

(* Object system *)

class c ?(a = [%call_loc]) ?(b = [%call_loc]) () =
  object
    method x = a, b
  end
[%%expect{|
class c :
  ?a:[%call_loc] ->
  ?b:[%call_loc] -> unit -> object method x : textloc * textloc end
|}]

(* Object system partial application *)
let x = new c ~b:pos_b ;;
let y = x ~a:pos_a ;;
let a, b = (y ())#x ;;
[%%expect{|
val x : ?a:[%call_loc] -> unit -> c = <fun>
val y : unit -> c = <fun>
val a : textloc = <location: File "", line 1, characters 15-19>
val b : textloc = <location: File "", line 2, characters 15-19>
|}]

(* Labels on source positions can't commute in class definitions *)
class m : ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> object end =
  fun ?(b = [%call_loc]) ?(a = [%call_loc]) () -> object end
[%%expect{|
Line 2, characters 6-60:
2 |   fun ?(b = [%call_loc]) ?(a = [%call_loc]) () -> object end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type ?b:[%call_loc] -> ?a:[%call_loc] -> unit -> object  end
       is not matched by the class type
         ?a:[%call_loc] -> ?b:[%call_loc] -> unit -> object  end
|}]

(* [%call_loc] is distinct from textloc *)
class c :
  a:textloc -> ?b:[%call_loc] -> unit -> object
    method x : textloc * textloc
  end = fun ?(a = [%call_loc]) ~b () -> object
    method x = a, b
  end
[%%expect{|
Lines 4-6, characters 12-5:
4 | ............?(a = [%call_loc]) ~b () -> object
5 |     method x = a, b
6 |   end
Error: The class type
         ?a:[%call_loc] -> b:'b -> unit -> object method x : 'a * 'b end
       is not matched by the class type
         a:textloc ->
         ?b:[%call_loc] -> unit -> object method x : textloc * textloc end
|}]
