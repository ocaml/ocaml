(* TEST
 flags = "-w +A";
 expect;
*)


module M = struct let f () = 1 end;;
[%%expect {|
module M : sig val f : unit -> int end
|}]

let g f = f (); M.(f ());;
[%%expect {|
Line 1, characters 16-17:
1 | let g f = f (); M.(f ());;
                    ^
Warning 44 [open-shadow-identifier]: this open statement shadows the
  value identifier "f" (which is later used)

val g : (unit -> 'a) -> int = <fun>
|}]

(* regression test for #12494

   When checking 'let f ...' functions, the type-checker sometimes
   inserts a dummy 'unbound' variable 'f' in the environment to detect
   failed lookups on 'f' in the definition and hint at a missing 'rec'
   keyword. Ensure that those dummy bindings do not result in
   shadowing warnings.
*)
let f () = M.(f ());;
[%%expect {|
val f : unit -> int = <fun>
|}]

(* Advanced behaviors related to #12494: in classes.

   (See the discussion in #12498 for a weird related example by Florian
    Angeletti using modules that is arguably not completely fixed by
    #12498.)

 *)

(* this is expected *)
class c = object
  val x = 0
  val y = x + 1
end;;
[%%expect {|
Line 3, characters 10-11:
3 |   val y = x + 1
              ^
Error: The instance variable "x"
       cannot be accessed from the definition of another instance variable
|}]

module M = struct let x = 0 end
class c = object
  val x=0
  (* ... so there is no shadowing here *)
  val y = M.( x ) + 1
end;;
[%%expect {|
module M : sig val x : int end
class c : object val x : int val y : int end
|}]
