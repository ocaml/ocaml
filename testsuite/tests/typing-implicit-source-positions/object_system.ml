(* TEST
   expect;
*)

let object_with_a_method_with_a_positional_parameter = object
  method m ?(call_loc = [%call_loc]) () = call_loc
end

[%%expect{|
val object_with_a_method_with_a_positional_parameter :
  < m : ?call_loc:[%call_loc] -> unit -> textloc > = <obj>
|}]

let position = object_with_a_method_with_a_positional_parameter#m ();;

[%%expect{|
val position : textloc = <location: File "", line 1, characters 15-68>
|}]

class class_with_a_method_with_a_positional_parameter = object
  method m ?(call_loc = [%call_loc]) () = call_loc
end

[%%expect{|
class class_with_a_method_with_a_positional_parameter :
  object method m : ?call_loc:[%call_loc] -> unit -> textloc end
|}]

let o = new class_with_a_method_with_a_positional_parameter;;

[%%expect{|
val o : class_with_a_method_with_a_positional_parameter = <obj>
|}]

let position = o#m ();;

[%%expect{|
val position : textloc = <location: File "", line 1, characters 15-21>
|}]

let position = (new class_with_a_method_with_a_positional_parameter)#m ();;

[%%expect{|
val position : textloc = <location: File "", line 1, characters 15-73>
|}]


class class_with_positional_parameter ?(call_loc = [%call_loc]) () = object
  method call_loc = call_loc
end

[%%expect{|
class class_with_positional_parameter :
  ?call_loc:[%call_loc] -> unit -> object method call_loc : textloc end
|}]

let o = new class_with_positional_parameter ()
let position = o#call_loc

[%%expect{|
val o : class_with_positional_parameter = <obj>
val position : textloc = <location: File "", line 1, characters 8-46>
|}]


(* Different kinds of shadowed parameters (both a class parameter is shadowed and a
   method parameter is shadowed) *)

class c ?(call_loc = [%call_loc]) () = object(self)
  method from_class_param = call_loc

  method m ?(call_loc = [%call_loc]) () = call_loc, self#from_class_param
end
[%%expect{|
class c :
  ?call_loc:[%call_loc] ->
  unit ->
  object
    method from_class_param : textloc
    method m : ?call_loc:[%call_loc] -> unit -> textloc * textloc
  end
|}]

let c = (new c ())
let from_method_param, from_class_param = c#m()

[%%expect{|
val c : c = <obj>
val from_method_param : textloc =
  <location: File "", line 2, characters 42-47>
val from_class_param : textloc = <location: File "", line 1, characters 8-18>
|}]

class parent ?(call_loc = [%call_loc]) () = object
  method pos = call_loc
end

let o = object
  inherit parent ()
end
let position = o#pos

[%%expect{|
class parent :
  ?call_loc:[%call_loc] -> unit -> object method pos : textloc end
val o : parent = <obj>
val position : textloc = <location: File "", line 6, characters 10-19>
|}]

let o ?(call_loc = [%call_loc]) () = object
  inherit parent ~call_loc ()
end
let position = (o ())#pos

[%%expect{|
val o : ?call_loc:[%call_loc] -> unit -> parent = <fun>
val position : textloc = <location: File "", line 4, characters 15-21>
|}]

(* Applying an call_loc argument without a label. *)
let o ?(call_loc = [%call_loc]) () = object
  inherit parent call_loc ()
end
let position = (o ())#pos

[%%expect{|
Line 2, characters 10-16:
2 |   inherit parent call_loc ()
              ^^^^^^
Warning 6 [labels-omitted]: label "?call_loc" was omitted in the application of
  this function.

val o : ?call_loc:[%call_loc] -> unit -> parent = <fun>
val position : textloc = <location: File "", line 4, characters 15-21>
|}]


(* Same behavior as optional parameters. *)
class parent ?(i = 1) () = object
  method i = i
end

let o = object
  inherit parent ()
end
let position = o#i

[%%expect{|
class parent : ?i:int -> unit -> object method i : int end
val o : parent = <obj>
val position : int = 1
|}]

(* Partially applying a class *)
class c ?(a = [%call_loc]) ?(b = [%call_loc]) () =
  object
    method a = a
    method b = b
  end

[%%expect{|
class c :
  ?a:[%call_loc] ->
  ?b:[%call_loc] -> unit -> object method a : textloc method b : textloc end
|}]

let pos_a : textloc = Textloc.Compact.none;;
let partially_applied_class = new c ~a:pos_a

[%%expect{|
val pos_a : textloc = <location: File "-">
val partially_applied_class : ?b:[%call_loc] -> unit -> c = <fun>
|}]

let fully_applied_class = partially_applied_class ()

[%%expect{|
val fully_applied_class : c = <obj>
|}]

let a, b = fully_applied_class#a, fully_applied_class#b

[%%expect{|
val a : textloc = <location: File "-">
val b : textloc = <location: File "", line 1, characters 26-52>
|}]

class c :
  ?x:[%call_loc] -> y:textloc -> unit -> object
    method xy : textloc * textloc
  end = fun ?(x = [%call_loc]) ~y () -> object
    method xy = x, y
  end

[%%expect{|
class c :
  ?x:[%call_loc] ->
  y:textloc -> unit -> object method xy : textloc * textloc end
|}]

let x, y = (new c ~y:pos_a ())#xy

[%%expect{|
val x : textloc = <location: File "", line 1, characters 11-30>
val y : textloc = <location: File "-">
|}]
