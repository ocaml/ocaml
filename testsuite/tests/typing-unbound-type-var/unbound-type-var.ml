(* TEST
   * expect
*)

class test a c =
object
  method b = c
end

[%%expect{|
Lines 1-4, characters 0-3:
1 | class test a c =
2 | object
3 |   method b = c
4 | end
Error: Some type variables are unbound in this type:
         class test : 'a -> 'b -> object method b : 'b end
       The method b has type 'a where 'a is unbound
|}]
