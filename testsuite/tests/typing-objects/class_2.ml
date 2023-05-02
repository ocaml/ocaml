(* TEST
 expect;
*)

(* class expressions may also contain local recursive bindings *)
class test =
  let rec f = fun x -> g x
      and g = fun x -> f x in
object
  method f : 'a 'b. 'a -> 'b = f
  method g : 'a 'b. 'a -> 'b = g
end
[%%expect{|
class test : object method f : 'a -> 'b method g : 'a -> 'b end
|}]
