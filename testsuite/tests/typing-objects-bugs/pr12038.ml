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
Uncaught exception: File "typing/ctype.ml", line 3433, characters 29-35: Assertion failed

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
Uncaught exception: File "typing/ctype.ml", line 3433, characters 29-35: Assertion failed

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
Uncaught exception: File "typing/ctype.ml", line 3433, characters 29-35: Assertion failed

|}]

class d = object(self : 'self)
  method virtual private m : unit
  inherit ['self] c
  method m = ()
end
[%%expect{|
Uncaught exception: File "typing/ctype.ml", line 3433, characters 29-35: Assertion failed

|}]

let o = object(_ : 'self)
  method virtual private m : unit
  constraint 'self = < >
  method m = ()
end
[%%expect{|
Uncaught exception: File "typing/ctype.ml", line 3433, characters 29-35: Assertion failed

|}]
