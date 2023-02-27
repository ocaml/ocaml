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
