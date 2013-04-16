(* Report from Jeremy Yallop *)
module F (S : sig type 'a s end) = struct
  include S
  type _ t = T : 'a -> 'a s t
end;; (* fail *)
module M = F (struct type 'a s = int end) ;;

(* Another version using OCaml 2.00 objects *)
module F(T:sig type 'a t end) = struct
  class ['a] c x =
    object constraint 'a = 'b T.t val x' : 'b = x method x = x' end
end;; (* fail *)

(* It is not OK to allow modules exported by other compilation units *)
type (_,_) eq = Eq : ('a,'a) eq;;
let eq = Obj.magic Eq;;
let eq : ('a Queue.t, 'b Queue.t) eq = eq;;
type _ t = T : 'a -> 'a Queue.t t;; (* fail *)
let castT (type a) (type b) (x : a t) (e: (a, b) eq) : b t =
  let Eq = e in (x : b t);;
let x = castT (T 3) eq;;

(* The following signature should not be accepted *)
module type S = sig
  type 'a s
  type _ t = T : 'a -> 'a s t
end;; (* fail *)
(* Otherwise we can write the following *)
module rec M : (S with type 'a s = unit) = M;;
(* For the above reason, we cannot allow the abstract declaration
   of s and the definition of t to be in the same module, as
   we could create the signature using [module type of ...] *)
