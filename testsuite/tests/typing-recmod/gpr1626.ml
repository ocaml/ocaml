(* TEST
 expect;
*)

module type S = sig module M : sig end module N = M end;;
[%%expect{|
module type S = sig module M : sig end module N = M end
|}];;

module rec M : S with module M := M = M;;
[%%expect{|
Line 1, characters 34-35:
1 | module rec M : S with module M := M = M;;
                                      ^
Error: This module type is recursive. This use of the recursive module "M"
       within its own definition makes the module type of "M" depend on itself.
       Such recursive definitions of module types are not allowed.
|}];;

module rec M : S = M and P : S with module M := M = P;;
[%%expect{|
Line 1, characters 48-49:
1 | module rec M : S = M and P : S with module M := M = P;;
                                                    ^
Error: This module type is recursive. This use of the recursive module "M"
       within the definition of the module "P"
       makes the module type of "P" depend on the module type of "M".
       Such recursive definitions of module types are not allowed.
|}];;
