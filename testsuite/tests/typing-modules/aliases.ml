module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C'' : (module C) = C';; (* fails *)

module C'' : (module Char) = C;;
C''.chr 66;;

let f x = let module M = struct module L = List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;

module F(X:sig end) = Char;;
module C3 = F(struct end);;

module G(X:sig end) = X;; (* does not alias X *)
module M = G(struct end);;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;

module M'' : sig module N' : sig val x : int end end = M';; (* must fix *)
M''.N'.x;;
