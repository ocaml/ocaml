(* TEST
   expect;
*)

type t = [%call_loc]
[%%expect {|
Line 1, characters 11-19:
1 | type t = [%call_loc]
               ^^^^^^^^
Error: [%call_loc] can only exist as the type of an
       optional argument, or its default value
|}]

type t = unit -> unit -> [%call_loc]
[%%expect {|
Line 1, characters 27-35:
1 | type t = unit -> unit -> [%call_loc]
                               ^^^^^^^^
Error: [%call_loc] can only exist as the type of an
       optional argument, or its default value
|}]

let f ?(call_loc = [%call_loc]) () : [%call_loc] = call_loc

[%%expect{|
Line 1, characters 39-47:
1 | let f ?(call_loc = [%call_loc]) () : [%call_loc] = call_loc
                                           ^^^^^^^^
Error: [%call_loc] can only exist as the type of an
       optional argument, or its default value
|}]

let apply f = f ~call_loc:Lexing.dummy_pos () ;;
[%%expect {|
val apply : (call_loc:Lexing.position -> unit -> 'a) -> 'a = <fun>
|}]

let g = fun ?(call_loc = [%call_loc]) () -> ()
[%%expect{|
val g : ?call_loc:[%call_loc] -> unit -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
Line 1, characters 14-15:
1 | let _ = apply g ;;
                  ^
Error: The value "g" has type "?call_loc:[%call_loc] -> unit -> unit"
       but an expression was expected of type
         "call_loc:Lexing.position -> unit -> 'a"
       The label "?call_loc" was not expected to be of type "[%call_loc]"
|}]

let h ~(call_loc:[%call_loc]) () = ()
[%%expect{|
Line 1, characters 17-28:
1 | let h ~(call_loc:[%call_loc]) () = ()
                     ^^^^^^^^^^^
Error: A position argument must be optional.
|}]

let j (call_loc:[%call_loc]) () = ()
[%%expect{|
Line 1, characters 16-27:
1 | let j (call_loc:[%call_loc]) () = ()
                    ^^^^^^^^^^^
Error: A position argument must not be unlabelled.
|}]

let k : ?call_loc:[%call_loc] -> unit -> unit =
   fun ?call_loc () -> ()
[%%expect{|
Line 2, characters 3-25:
2 |    fun ?call_loc () -> ()
       ^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type "?call_loc:[%call_loc] -> unit -> unit"
       but its first argument is labeled "?call_loc"
       instead of "?(call_loc = [%call_loc])"
Hint: Consider marking the argument as a source position with "= [%call_loc]"
|}]

let n = fun ?(call_loc = [%call_loc]) () -> call_loc
[%%expect{|
val n : ?call_loc:[%call_loc] -> unit -> textloc = <fun>
|}]

let _ = n Lexing.dummy_pos ();;
[%%expect {|
Line 1, characters 27-29:
1 | let _ = n Lexing.dummy_pos ();;
                               ^^
Error: The function applied to this argument has type
         ?call_loc:[%call_loc] -> textloc
This argument cannot be applied without label
|}]

class this_class_has_an_unerasable_argument ?(pos = [%call_loc]) = object end

[%%expect{|
Line 1, characters 46-49:
1 | class this_class_has_an_unerasable_argument ?(pos = [%call_loc]) = object end
                                                  ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

class this_class_has_an_unerasable_argument : ?pos:[%call_loc] -> object  end
|}]

class c = object
  method this_method_has_an_unerasable_argument ?(pos = [%call_loc]) = pos
end
[%%expect{|
Line 2, characters 50-53:
2 |   method this_method_has_an_unerasable_argument ?(pos = [%call_loc]) = pos
                                                      ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

class c :
  object
    method this_method_has_an_unerasable_argument :
      ?pos:[%call_loc] -> textloc
  end
|}]

let this_object_has_an_unerasable_argument ?(pos = [%call_loc]) = object end

[%%expect{|
Line 1, characters 45-48:
1 | let this_object_has_an_unerasable_argument ?(pos = [%call_loc]) = object end
                                                 ^^^
Warning 76 [unerasable-position-argument]: this position argument
  cannot be erased.

val this_object_has_an_unerasable_argument : ?pos:[%call_loc] -> <  > = <fun>
|}]
