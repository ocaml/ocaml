(* TEST
   * expect
*)
class virtual ['self] x = object(self : 'self)
  method q () = self#z ()
end
and virtual ['self] x' = object(self : 'self)
  method q () = self#z ()
end
and y = object(self : 'self)
  inherit ['self] x
  inherit ['self] x'
  method z () = ()
end

[%%expect{|
Line 2, characters 16-22:
2 |   method q () = self#z ()
                    ^^^^^^
Warning 17 [undeclared-virtual-method]: the virtual method z is not declared.

Line 5, characters 16-22:
5 |   method q () = self#z ()
                    ^^^^^^
Warning 17 [undeclared-virtual-method]: the virtual method z is not declared.

class virtual ['a] x :
  object ('a)
    constraint 'a = < q : unit -> unit; .. >
    method q : unit -> unit
    method private virtual z : unit -> unit
  end
and virtual ['a] x' :
  object ('a)
    constraint 'a = < q : unit -> unit; .. >
    method q : unit -> unit
    method private virtual z : unit -> unit
  end
and y : object method q : unit -> unit method z : unit -> unit end
|}]

(* Variations on the same theme *)

class [ 'self ] c = object(_ : 'self) end

class d = object(self : 'self)
  method virtual private m : int
  inherit (['self] c : object ('self) method virtual private m : int end)
  method n (o : 'self) : string = o#m
  method m = 0
end

let o = new d

let () = print_endline (o#n o)
[%%expect{|
class ['a] c : object ('a) constraint 'a = < .. > end
Line 5, characters 12-17:
5 |   inherit (['self] c : object ('self) method virtual private m : int end)
                ^^^^^
Error: The type parameter < .. > does not meet its constraint: it should be
         < .. >
       Self type cannot escape its class
|}]

class d = object(self : 'self)
  method virtual private m : int
  inherit ['self] c
  method n (o : 'self) : string = o#m
  method m = 0
end

let o = new d

let () = print_endline (o#n o)
[%%expect{|
Line 4, characters 34-37:
4 |   method n (o : 'self) : string = o#m
                                      ^^^
Error: This expression has type int but an expression was expected of type
         string
|}]

class d = object(self : 'self)
  method virtual private m : unit
  inherit ['self] c
  method m = ()
end
[%%expect{|
class d : object method m : unit end
|}]

let o = object(_ : 'self)
  method virtual private m : unit
  constraint 'self = < >
  method m = ()
end
[%%expect{|
Line 4, characters 2-15:
4 |   method m = ()
      ^^^^^^^^^^^^^
Error: This object is expected to have type : <  >
       This type does not have a method m.
|}]
