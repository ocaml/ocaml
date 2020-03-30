(* TEST
   * expect
*)

class type c = object end;;
[%%expect{|
class type c = object  end
|}]

module type S = sig class c: c end;;
[%%expect{|
Line _, characters 29-30:
  module type S = sig class c: c end;;
                               ^
Error: The class type c is not yet completely defined
|}]
