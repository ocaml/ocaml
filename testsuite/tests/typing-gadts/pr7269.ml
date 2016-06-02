type s = [`A | `B] and sub = [`B];;
type +'a t = T : [< `Conj of 'a & sub | `Other of string] -> 'a t;; (* fail *)

(* would cause segmentation fault:
let f (T (`Other msg) : s t) = print_string msg;;
let _ = f (T (`Conj `B) :> s t)
*)
