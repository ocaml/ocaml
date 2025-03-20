(* TEST
   expect;
*)

let object_with_a_method_with_a_positional_parameter = object
  method m ?(call_pos = [%call_pos]) () = call_pos
end

[%%expect{|
val object_with_a_method_with_a_positional_parameter :
  < m : ?call_pos:[%call_pos] -> unit -> lexing_location > = <obj>
|}]

let position = object_with_a_method_with_a_positional_parameter#m ();;

[%%expect{|
val position : lexing_location = <location: "", line 1, bytes 15-68>
|}]

class class_with_a_method_with_a_positional_parameter = object
  method m ?(call_pos = [%call_pos]) () = call_pos
end

[%%expect{|
class class_with_a_method_with_a_positional_parameter :
  object method m : ?call_pos:[%call_pos] -> unit -> lexing_location end
|}]

let o = new class_with_a_method_with_a_positional_parameter;;

[%%expect{|
val o : class_with_a_method_with_a_positional_parameter = <obj>
|}]

let position = o#m ();;

[%%expect{|
val position : lexing_location = <location: "", line 1, bytes 15-21>
|}]

let position = (new class_with_a_method_with_a_positional_parameter)#m ();;

[%%expect{|
val position : lexing_location = <location: "", line 1, bytes 15-73>
|}]


class class_with_positional_parameter ?(call_pos = [%call_pos]) () = object
  method call_pos = call_pos
end

[%%expect{|
class class_with_positional_parameter :
  ?call_pos:[%call_pos] ->
  unit -> object method call_pos : lexing_location end
|}]

let o = new class_with_positional_parameter ()
let position = o#call_pos

[%%expect{|
val o : class_with_positional_parameter = <obj>
val position : lexing_location = <location: "", line 1, bytes 8-46>
|}]


(* Different kinds of shadowed parameters (both a class parameter is shadowed and a
   method parameter is shadowed) *)

class c ?(call_pos = [%call_pos]) () = object(self)
  method from_class_param = call_pos

  method m ?(call_pos = [%call_pos]) () = call_pos, self#from_class_param
end
[%%expect{|
class c :
  ?call_pos:[%call_pos] ->
  unit ->
  object
    method from_class_param : lexing_location
    method m :
      ?call_pos:[%call_pos] -> unit -> lexing_location * lexing_location
  end
|}]

let c = (new c ())
let from_method_param, from_class_param = c#m()

[%%expect{|
val c : c = <obj>
val from_method_param : lexing_location = <location: "", line 2, bytes 42-47>
val from_class_param : lexing_location = <location: "", line 1, bytes 8-18>
|}]

class parent ?(call_pos = [%call_pos]) () = object
  method pos = call_pos
end

let o = object
  inherit parent ()
end
let position = o#pos

[%%expect{|
class parent :
  ?call_pos:[%call_pos] -> unit -> object method pos : lexing_location end
val o : parent = <obj>
val position : lexing_location = <location: "", line 6, bytes 10-19>
|}]

let o ?(call_pos = [%call_pos]) () = object
  inherit parent ~call_pos ()
end
let position = (o ())#pos

[%%expect{|
val o : ?call_pos:[%call_pos] -> unit -> parent = <fun>
val position : lexing_location = <location: "", line 4, bytes 15-21>
|}]

(* Applying an call_pos argument without a label. *)
let o ?(call_pos = [%call_pos]) () = object
  inherit parent call_pos ()
end
let position = (o ())#pos

[%%expect{|
Line 2, characters 10-16:
2 |   inherit parent call_pos ()
              ^^^^^^
Warning 6 [labels-omitted]: label "?call_pos" was omitted in the application of
  this function.

val o : ?call_pos:[%call_pos] -> unit -> parent = <fun>
val position : lexing_location = <location: "", line 4, bytes 15-21>
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
class c ?(a = [%call_pos]) ?(b = [%call_pos]) () =
  object
    method a = a
    method b = b
  end

[%%expect{|
class c :
  ?a:[%call_pos] ->
  ?b:[%call_pos] ->
  unit -> object method a : lexing_location method b : lexing_location end
|}]

let pos_a : lexing_location = Textloc.dummy;;
let partially_applied_class = new c ~a:pos_a

[%%expect{|
val pos_a : lexing_location = <location: "", line 0, bytes -1--1>
val partially_applied_class : ?b:[%call_pos] -> unit -> c = <fun>
|}]

let fully_applied_class = partially_applied_class ()

[%%expect{|
val fully_applied_class : c = <obj>
|}]

let a, b = fully_applied_class#a, fully_applied_class#b

[%%expect{|
val a : lexing_location = <location: "", line 0, bytes -1--1>
val b : lexing_location = <location: "", line 1, bytes 26-52>
|}]

class c :
  ?x:[%call_pos] -> y:lexing_location -> unit -> object
    method xy : lexing_location * lexing_location
  end = fun ?(x = [%call_pos]) ~y () -> object
    method xy = x, y
  end

[%%expect{|
class c :
  ?x:[%call_pos] ->
  y:lexing_location ->
  unit -> object method xy : lexing_location * lexing_location end
|}]

let x, y = (new c ~y:pos_a ())#xy

[%%expect{|
val x : lexing_location = <location: "", line 1, bytes 11-30>
val y : lexing_location = <location: "", line 0, bytes -1--1>
|}]
