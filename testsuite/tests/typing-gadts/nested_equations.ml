(* Tests for nested equations (bind abstract types from other modules) *)

type _ t = Int : int t;;

let to_int (type a) (w : a t) (x : a) : int = let Int = w in x;;

let w_bool : bool t = Obj.magic 0;;
let f_bool (x : bool) : int = let Int = w_bool in x;; (* fail *)

let w_buffer : Buffer.t t = Obj.magic 0;;
let f_buffer (x : Buffer.t) : int = let Int = w_buffer in x;; (* ok *)

let w_spec : Arg.spec t = Obj.magic 0;;
let f_spec (x : Arg.spec) : int = let Int = w_spec in x;; (* ok *)

module M : sig type u val w : u t val x : u end =
  struct type u = int let w = Int let x = 33 end;;
let m_x : int = let Int = M.w in M.x;;

module F (X : sig type u = int val x : u end) = struct let x : int = X.x end;;
let fm_x : int = let Int = M.w in let module FM = F(M) in FM.x;; (* ok *)

module M' = struct module M : sig type u val w : u t val x : u end = M end;;
module F' (X : sig module M : sig type u = int val x : u end end) =
  struct let x : int = X.M.x end;;
let fm'_x : int =
  let Int = M'.M.w in let module FM' = F'(M') in FM'.x;; (* ok *)

(* PR#7233 *)

type (_, _) eq = Refl : ('a, 'a) eq

module type S = sig
  type t
  val eql : (t, int) eq
end

module F (M : S) = struct
  let zero : M.t =
    let Refl = M.eql in 0
end;;
