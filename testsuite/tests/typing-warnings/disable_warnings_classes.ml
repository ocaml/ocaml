(* TEST
 flags = " -w +A ";
 expect;
*)

class c = object

  val a =
    let b = 5 in ()
  [@@warning "-26"]

  val x =
    let y = 5 in ()

end;;
[%%expect {|
Line 8, characters 8-9:
8 |     let y = 5 in ()
            ^
Warning 26 [unused-var]: unused variable "y".

class c : object val a : unit val x : unit end
|}];;

class c = object

  method a =
    let b = 5 in ()
  [@@warning "-26"]

  method x =
    let y = 5 in ()

end;;
[%%expect {|
Line 8, characters 8-9:
8 |     let y = 5 in ()
            ^
Warning 26 [unused-var]: unused variable "y".

class c : object method a : unit method x : unit end
|}];;

class c = object

  initializer
    let b = 5 in ()
  [@@warning "-26"]

  initializer
    let y = 5 in ()

end;;
[%%expect {|
Line 8, characters 8-9:
8 |     let y = 5 in ()
            ^
Warning 26 [unused-var]: unused variable "y".

class c : object  end
|}];;

class c = (object

  val a =
    let b = 5 in ()

end [@warning "-26"])
[%%expect {|
class c : object val a : unit end
|}];;

class c = object

  val a =
    let b = 5 in ()

  [@@@warning "-26"]

  val x =
    let y = 5 in ()

end;;
[%%expect {|
Line 4, characters 8-9:
4 |     let b = 5 in ()
            ^
Warning 26 [unused-var]: unused variable "b".

class c : object val a : unit val x : unit end
|}];;

type dep
[@@deprecated "deprecated"]

class type c = object

  val a : dep
  [@@warning "-3"]

  val x : dep

end;;
[%%expect {|
type dep
Line 9, characters 10-13:
9 |   val x : dep
              ^^^
Alert deprecated: dep
deprecated

class type c = object val a : dep val x : dep end
|}];;

class type c = object

  method a : dep
  [@@warning "-3"]

  method x : dep

end;;
[%%expect {|
Line 6, characters 13-16:
6 |   method x : dep
                 ^^^
Alert deprecated: dep
deprecated

class type c = object method a : dep method x : dep end
|}];;

class type c = object [@warning "-3"]

  val a : dep

end
[%%expect {|
class type c = object val a : dep end
|}];;

class type c = object

  val a : dep

  [@@@warning "-3"]

  val x : dep

end;;
[%%expect {|
Line 3, characters 10-13:
3 |   val a : dep
              ^^^
Alert deprecated: dep
deprecated

class type c = object val a : dep val x : dep end
|}];;
