(* TEST
   * expect
*)

class virtual child1 parent =
  object
    method private parent = parent
  end

and virtual child2 =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

[%%expect{|
class virtual child1 : 'a -> object method private parent : 'a end
and virtual child2 :
  object ('a)
    method private virtual parent : < previous : 'a option; .. >
  end
|}]

class virtual child1' parent =
  object
    method private parent = parent
  end

and virtual child2' =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

and foo = object(self)
  method previous = None
  method child =
    object
      inherit child1' self
      inherit child2'
    end
end;;

[%%expect{|
Line 16, characters 22-26:
16 |       inherit child1' self
                           ^^^^
Error: This expression has type < child : 'a; previous : 'b option; .. >
       but an expression was expected of type 'c
       Self type cannot escape its class
|}]

(* Whether we have [class foo1] or [let foo1] doesn't change a thing. *)
class foo1 = object(self)
  method previous = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
class foo1 : object method child : child1 method previous : child1 option end
|}]

class nested = object
  method obj = object(self)
    method previous = None
    method child () =
      object
        inherit child1 self
        inherit child2
      end
  end
end;;
[%%expect{|
class nested :
  object
    method obj : < child : unit -> child1; previous : child1 option >
  end
|}]

class just_to_see = object(self)
  method previous = None
  method child =
    let o =
      object
        inherit child1 self
        inherit child2
      end
    in
    o
end;;
[%%expect{|
class just_to_see :
  object method child : child1 method previous : child1 option end
|}]

class just_to_see2 = object
  method obj = object(self)
    method previous = None
    method child =
      let o =
        object
          inherit child1 self
          inherit child2
        end
      in
      o
  end
end;;
[%%expect{|
class just_to_see2 :
  object method obj : < child : child1; previous : child1 option > end
|}]

type gadt = Not_really_though : gadt

class just_to_see3 = object(self)
  method previous = None
  method child Not_really_though =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
type gadt = Not_really_though : gadt
class just_to_see3 :
  object method child : gadt -> child1 method previous : child1 option end
|}]

class leading_up_to = object(self : 'a)
  method previous : 'a option = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
Lines 4-7, characters 4-7:
4 | ....object
5 |       inherit child1 self
6 |       inherit child2
7 |     end
Error: This object has undeclared virtual methods.
       The following methods were not declared : previous child
|}]

class assertion_failure = object(self : 'a)
  method previous : 'a option = None
  method child =
    object
      inherit child1 self
      inherit child2

      method previous = None
      method child = assert false
    end
end;;
[%%expect{|
Lines 4-10, characters 4-7:
 4 | ....object
 5 |       inherit child1 self
 6 |       inherit child2
 7 |
 8 |       method previous = None
 9 |       method child = assert false
10 |     end
Error: Cannot close type of object literal:
       < child : '_weak1; previous : 'a option; _.. > as 'a
       it has been unified with the self type of a class that is not yet
       completely defined.
|}]

(* MPR#7894 and variations *)
class parameter_contains_self app = object(self)
  method invalidate : unit =
    app#redrawWidget self
end;;
[%%expect{|
class parameter_contains_self :
  < redrawWidget : 'a -> unit; .. > ->
  object ('a) method invalidate : unit end
|}]

class closes_via_inheritance param =
  let _ = new parameter_contains_self param in object
    inherit parameter_contains_self param
  end;;
[%%expect{|
Line 3, characters 36-41:
3 |     inherit parameter_contains_self param
                                        ^^^^^
Error: This expression has type
         < redrawWidget : parameter_contains_self -> unit; .. >
       but an expression was expected of type
         < redrawWidget : < invalidate : unit; .. > -> unit; .. >
       Type parameter_contains_self = < invalidate : unit >
       is not compatible with type < invalidate : unit; .. >
       Self type cannot be unified with a closed object type
|}]

class closes_via_application param =
  let _ = new parameter_contains_self param in
  parameter_contains_self param;;
[%%expect{|
Line 3, characters 26-31:
3 |   parameter_contains_self param;;
                              ^^^^^
Error: This expression has type
         < redrawWidget : parameter_contains_self -> unit; .. >
       but an expression was expected of type
         < redrawWidget : < invalidate : unit; .. > -> unit; .. >
       Type parameter_contains_self = < invalidate : unit >
       is not compatible with type < invalidate : unit; .. >
       Self type cannot be unified with a closed object type
|}]

let escapes_via_inheritance param =
  let module Local = struct
    class c = object
      inherit parameter_contains_self param
    end
  end in
  ();;
[%%expect{|
Line 4, characters 38-43:
4 |       inherit parameter_contains_self param
                                          ^^^^^
Error: This expression has type 'a but an expression was expected of type
         < redrawWidget : < invalidate : unit; .. > -> unit; .. >
       Self type cannot escape its class
|}]

let escapes_via_application param =
  let module Local = struct
    class c = parameter_contains_self param
  end in
  ();;
[%%expect{|
Line 3, characters 38-43:
3 |     class c = parameter_contains_self param
                                          ^^^^^
Error: This expression has type 'a but an expression was expected of type
         < redrawWidget : < invalidate : unit; .. > -> unit; .. >
       Self type cannot escape its class
|}]

let can_close_object_via_inheritance param =
    let _ = new parameter_contains_self param in object
    inherit parameter_contains_self param
  end;;
[%%expect{|
Line 3, characters 36-41:
3 |     inherit parameter_contains_self param
                                        ^^^^^
Error: This expression has type
         < redrawWidget : parameter_contains_self -> unit; .. >
       but an expression was expected of type
         < redrawWidget : < invalidate : unit; .. > -> unit; .. >
       Type parameter_contains_self = < invalidate : unit >
       is not compatible with type < invalidate : unit; .. >
       Self type cannot be unified with a closed object type
|}]

let can_escape_object_via_inheritance param = object
    inherit parameter_contains_self param
  end;;
[%%expect{|
val can_escape_object_via_inheritance :
  < redrawWidget : parameter_contains_self -> unit; .. > ->
  parameter_contains_self = <fun>
|}]

let can_close_object_explicitly = object (_ : < i : int >)
  method i = 5
end;;
[%%expect{|
val can_close_object_explicitly : < i : int > = <obj>
|}]

let cannot_close_object_explicitly_with_inheritance = object
  inherit object (_ : < i : int >)
    method i = 5
  end
end;;
[%%expect{|
Line 2, characters 17-34:
2 |   inherit object (_ : < i : int >)
                     ^^^^^^^^^^^^^^^^^
Error: This pattern cannot match self: it only matches values of type
       < i : int >
|}]

class closes_after_constraint =
  ((fun (x : 'a) -> object (_:'a) end) : 'a -> object('a) end) (object end);;
[%%expect{|
Line 2, characters 63-75:
2 |   ((fun (x : 'a) -> object (_:'a) end) : 'a -> object('a) end) (object end);;
                                                                   ^^^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         < .. >
       Self type cannot be unified with a closed object type
|}];;

class type ['a] ct = object ('a) end
class type closes_via_application = [ <m : int> ] ct;;
[%%expect{|
class type ['a] ct = object ('a) constraint 'a = < .. > end
Line 2, characters 38-47:
2 | class type closes_via_application = [ <m : int> ] ct;;
                                          ^^^^^^^^^
Error: The type parameter < m : int >
       does not meet its constraint: it should be < .. >
       Self type cannot be unified with a closed object type
|}];;
