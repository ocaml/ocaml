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
Line _, characters 22-26:
        inherit child1' self
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
class foo1 : object method child : child2 method previous : child2 option end
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
    method obj : < child : unit -> child2; previous : child2 option >
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
  object method child : child2 method previous : child2 option end
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
  object method obj : < child : child2; previous : child2 option > end
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
  object method child : gadt -> child2 method previous : child2 option end
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
Line _, characters 4-65:
  ....object
        inherit child1 self
        inherit child2
      end
Error: Cannot close type of object literal:
       < child : '_weak1; previous : 'a option; _.. > as 'a
       it has been unified with the self type of a class that is not yet
       completely defined.
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
Line _, characters 4-129:
  ....object
        inherit child1 self
        inherit child2

        method previous = None
        method child = assert false
      end
Error: Cannot close type of object literal:
       < child : '_weak2; previous : 'a option; _.. > as 'a
       it has been unified with the self type of a class that is not yet
       completely defined.
|}]
