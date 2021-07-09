(* TEST
   flags = " -short-paths "
   * expect
*)

module M = struct type t = T end

type t = M.t

let x : M.t = S
[%%expect {|
module M : sig type t = T end
type t = M.t
Line 5, characters 14-15:
5 | let x : M.t = S
                  ^
Error: This variant expression is expected to have type t
       There is no constructor S within type t
|}]

module M = struct
  class c = object method foo = 3 end
end

type c = M.c

let () = (new M.c)#bar
[%%expect {|
module M : sig class c : object method foo : int end end
type c = M.c
Line 7, characters 9-18:
7 | let () = (new M.c)#bar
             ^^^^^^^^^
Error: This expression has type c
       It has no method bar
|}]
