(* TEST
   expect;
*)

type t = [%call_pos]
[%%expect {|
Line 1, characters 11-19:
1 | type t = [%call_pos]
               ^^^^^^^^
Error: [%call_pos] can only exist as the type of an
       optional argument, or its default value
|}]

type t = unit -> unit -> [%call_pos]
[%%expect {|
Line 1, characters 27-35:
1 | type t = unit -> unit -> [%call_pos]
                               ^^^^^^^^
Error: [%call_pos] can only exist as the type of an
       optional argument, or its default value
|}]

let f ?(call_pos = [%call_pos]) () : [%call_pos] = call_pos

[%%expect{|
Line 1, characters 39-47:
1 | let f ?(call_pos = [%call_pos]) () : [%call_pos] = call_pos
                                           ^^^^^^^^
Error: [%call_pos] can only exist as the type of an
       optional argument, or its default value
|}]

let apply f = f ~call_pos:Lexing.dummy_pos () ;;
[%%expect {|
val apply : (call_pos:Lexing.position -> unit -> 'a) -> 'a = <fun>
|}]

let g = fun ?(call_pos = [%call_pos]) () -> ()
[%%expect{|
val g : ?call_pos:[%call_pos] -> unit -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
Line 1, characters 14-15:
1 | let _ = apply g ;;
                  ^
Error: The value "g" has type "?call_pos:[%call_pos] -> unit -> unit"
       but an expression was expected of type
         "call_pos:Lexing.position -> unit -> 'a"
       The label "?call_pos" was not expected to be of type "[%call_pos]"
|}]

let h ~(call_pos:[%call_pos]) () = ()
[%%expect{|
Line 1, characters 17-28:
1 | let h ~(call_pos:[%call_pos]) () = ()
                     ^^^^^^^^^^^
Error: A position argument must be optional.
|}]

let j (call_pos:[%call_pos]) () = ()
[%%expect{|
Line 1, characters 16-27:
1 | let j (call_pos:[%call_pos]) () = ()
                    ^^^^^^^^^^^
Error: A position argument must not be unlabelled.
|}]

let k : ?call_pos:[%call_pos] -> unit -> unit =
   fun ?call_pos () -> ()
[%%expect{|
Line 2, characters 3-25:
2 |    fun ?call_pos () -> ()
       ^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type "?call_pos:[%call_pos] -> unit -> unit"
       but its first argument is labeled "?call_pos"
       instead of "?(call_pos = [%call_pos])"
Hint: Consider marking the argument as a source position with "= [%call_pos]"
|}]

let n = fun ?(call_pos = [%call_pos]) () -> call_pos
[%%expect{|
val n : ?call_pos:[%call_pos] -> unit -> lexing_location = <fun>
|}]

let _ = n Lexing.dummy_pos ();;
[%%expect {|
Line 1, characters 27-29:
1 | let _ = n Lexing.dummy_pos ();;
                               ^^
Error: The function applied to this argument has type
         ?call_pos:[%call_pos] -> lexing_location
This argument cannot be applied without label
|}]

class this_class_has_an_unerasable_argument ?(pos = [%call_pos]) = object end

[%%expect{|
Line 1, characters 46-49:
1 | class this_class_has_an_unerasable_argument ?(pos = [%call_pos]) = object end
                                                  ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

class this_class_has_an_unerasable_argument : ?pos:[%call_pos] -> object  end
|}]

class c = object
  method this_method_has_an_unerasable_argument ?(pos = [%call_pos]) = pos
end
[%%expect{|
Line 2, characters 50-53:
2 |   method this_method_has_an_unerasable_argument ?(pos = [%call_pos]) = pos
                                                      ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

class c :
  object
    method this_method_has_an_unerasable_argument :
      ?pos:[%call_pos] -> lexing_location
  end
|}]

let this_object_has_an_unerasable_argument ?(pos = [%call_pos]) = object end

[%%expect{|
Line 1, characters 45-48:
1 | let this_object_has_an_unerasable_argument ?(pos = [%call_pos]) = object end
                                                 ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

val this_object_has_an_unerasable_argument : ?pos:[%call_pos] -> <  > = <fun>
|}]
