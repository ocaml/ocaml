(* TEST
 expect;
*)

module M = struct
  module N = struct
    type t = A | B | C
  end
end
[%%expect {|
module M : sig module N : sig type t = A | B | C end end
|}]

let it = (M.N.A, M.N.B, M.N.C)
[%%expect {|
val it : M.N.t * M.N.t * M.N.t = (M.N.A, M.N.B, M.N.C)
|}]

open M
let it = (N.A, N.B, N.C)
[%%expect {|
val it : M.N.t * M.N.t * M.N.t = (N.A, N.B, N.C)
|}]

open N

let it = (A, B, C)
[%%expect {|
val it : M.N.t * M.N.t * M.N.t = (A, B, C)
|}]


module O = struct
  module P = struct
    type t = D | E | F
  end
end
[%%expect {|
module O : sig module P : sig type t = D | E | F end end
|}]

let it = (O.P.D, O.P.E, O.P.F)
[%%expect {|
val it : O.P.t * O.P.t * O.P.t = (O.P.D, O.P.E, O.P.F)
|}]

open O.P

let it = (D, E, F)
[%%expect {|
val it : O.P.t * O.P.t * O.P.t = (D, E, F)
|}]

(* Introduce Q with a Sub submodule *)
module Q = struct
  module Sub = struct type t = A end
end

(* open Q: Q.Sub.A can now be printed as Sub.A *)
open Q

(* shadow the Sub module:
   Sub.A is not a valid printing choice for Q.Sub.A anymore *)
module Sub = struct end
[%%expect {|
module Q : sig module Sub : sig type t = A end end
module Sub : sig end
|}]

(* Test the printing of Q.Sub.A *)
let it = Q.Sub.A
[%%expect {|
val it : Q.Sub.t = Q.Sub.A
|}]

(* A "hellish example" from Florian Angeletti. *)
module M = struct
  module N = struct
    module A = struct
      module B = struct
        type t = X
      end
    end
    module F(X:sig type t end) = struct type t = A of X.t end
  end
end
open M open N open A open B
[%%expect {|
module M :
  sig
    module N :
      sig
        module A : sig module B : sig type t = X end end
        module F : (X : sig type t end) -> sig type t = A of X.t end
      end
  end
|}]
let x = let module FB = F(B) in FB.A X
[%%expect {|
val x : M.N.F(M.N.A.B).t = M.N.F(M.N.A.B).A X
|}]

module X__A = struct type t = A type r = { f: int } end
module X__B = struct type t = B end

module X = struct
  module A = X__A
  module B = X__B
end

let x = X__A.A, X__B.B, { X__A.f = 0 }
[%%expect {|
module X__A : sig type t = A type r = { f : int; } end
module X__B : sig type t = B end
module X : sig module A = X__A module B = X__B end
val x : X.A.t * X.B.t * X.A.r = (X.A.A, X.B.B, {X.A.f = 0})
|}]

open X
let y = x

[%%expect{|
val y : X.A.t * X.B.t * X.A.r = (A.A, B.B, {A.f = 0})
|}]

open A
let z = x
[%%expect{|
val z : X.A.t * X.B.t * X.A.r = (A, B.B, {f = 0})
|}]
