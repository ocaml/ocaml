(* TEST
   ocamldoc with html;
   ocamldoc with latex;
*)

(* The output of this test demonstrates that the documentation
 * is put in the right order, even though the param tags order
 * may not correspond to the actual param order.
 *)

(**
 @param y second arg
 @param x first arg
 *)
let f1 x ~y = x + y

(**
 There's no way to refer to the third arg in documentation.

 @param y second arg
 @param x first arg
 *)
let f2 x ~y = function
  | Some z -> String.length x + y + z
  | None -> String.length x + y

(**
 @param x first arg
 @param z third arg
 @param y second arg
 *)
let f3 = fun x -> fun y -> fun z -> x + y + z

(**
 This output is a bit weird: we should probably refer
 to the third argument as [z], its label, rather than
 [blah], its internally-bound pattern.

 @param x first arg
 @param blah third arg
 @param y second arg
 *)
let f4 ?x ?(y = 4) ?z:(blah = 3) () =
  Option.value x ~default:5 + y + blah

class m = object (self)

  (**
   @param self this param should be dropped.
   *)
  method no_args = ()

  (**
   @param x first arg
   @param z third arg
   @param y second arg
   *)
  method a_few_args x y z = x + y + z
end
