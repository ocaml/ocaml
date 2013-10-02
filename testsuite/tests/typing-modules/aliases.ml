module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C'' : (module C) = C';; (* fails *)

module C'' : (module Char) = C;;
C''.chr 66;;

module C3 = struct include Char end;;
C3.chr 66;;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
C4.chr 66;;

module G(X:sig end) = struct module M = X end;; (* does not alias X *)
module M = G(struct end);;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;

module M'' : sig module N' : sig val x : int end end = M';;
M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
M3'.N'.x;;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M4.N'.x;;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' = N end);;
M2.N'.x;;

open M;;
N'.x;;

module M = struct
  module C = Char
  module C' = C
end;;
module M1
  : sig module C : sig val escaped : char -> string end module C' = C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' = C end);;
M2.C'.chr 66;;

StdLabels.List.map;;

module Q = Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;
