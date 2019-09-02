(* TEST
   * expect
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
Error: Illegal recursive module reference
|}];;
