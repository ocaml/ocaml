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
