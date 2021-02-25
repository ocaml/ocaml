(* TEST
   * expect
*)

class virtual t = object method virtual x: float end

class x = object(self: <x:int; ..>)
        inherit t
end
[%%expect {|
class virtual t : object method virtual x : float end
Line 4, characters 8-17:
4 |         inherit t
            ^^^^^^^^^
Error: The method x has type int but is expected to have type float
       Type int is not compatible with type float
|}]

let x =
  let module M = struct module type t = sig end end in
  (module struct end: M.t)
[%%expect {|
Line 3, characters 2-26:
3 |   (module struct end: M.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type (module M.t)
       but an expression was expected of type 'a
       The module type M.t would escape its scope
|}]
