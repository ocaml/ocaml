(* TEST
 expect;
*)

(* PR#7324 *)

module rec T : sig type t = T.t end = T;;
[%%expect{|
Line 1, characters 0-39:
1 | module rec T : sig type t = T.t end = T;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "T.t" is cyclic:
         "T.t" = "T.t"
|}]

(* Cyclic module type definitions should throw an error *)
module rec X : (sig module type A = X.A end)
  = struct module type A end
[%%expect {|
Line 1, characters 36-37:
1 | module rec X : (sig module type A = X.A end)
                                        ^
Error: This module type is recursive. This use of the recursive module "X"
       within its own definition makes the module type of "X" depend on itself.
       Such recursive definitions of module types are not allowed.
|}]

(* Cyclic module type definitions should throw an error *)
module rec X : (sig module type A := X.A end)
  = struct end
[%%expect {|
Line 1, characters 37-38:
1 | module rec X : (sig module type A := X.A end)
                                         ^
Error: This module type is recursive. This use of the recursive module "X"
       within its own definition makes the module type of "X" depend on itself.
       Such recursive definitions of module types are not allowed.
|}]
