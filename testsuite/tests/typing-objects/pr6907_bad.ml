(* TEST
   * expect
*)

class type ['e] t = object('s)
  method update : 'e -> 's
end;;
[%%expect{|
class type ['e] t = object ('a) method update : 'e -> 'a end
|}];;

module type S = sig
  class base : 'e -> ['e] t
end;;
[%%expect{|
Line 2, characters 2-27:
2 |   class base : 'e -> ['e] t
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Some type variables are unbound in this type:
         class base : 'e -> ['e] t
       The method update has type 'e -> #base where 'e is unbound
|}];;
