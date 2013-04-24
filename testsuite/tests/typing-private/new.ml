(* Concrete new types *)
module M1 : sig type 'a t val f : int t -> bool t end =
  struct type 'a t = new int let f x = x+1 end;;

module M2 =
  struct type 'a t = new int let f x = x+1  let r = ref 1 end;;

module M3 : sig type 'a t  val f : int t -> bool t end = M2;;

module M4 : sig type 'a t  val r : int t ref end = M2;; (* should fail *)

module M5 : sig type 'a t  val f : int t -> bool t  val r : int t ref end =
  struct type 'a t = new int  let f x = x+1  let r = ref (1 :> int t) end;;

module M6 : sig type 'a t = private int  val f : int t -> bool t end = M2;;

module M7 : sig type 'a t = new int end =  (* fail *)
  struct type 'a t = int end;;

module M8 : sig type 'a t  val f : int t ref -> int end =
  struct type 'a t = new int  let f r = !r end;; (* ok *)

module M9 : sig type 'a t  val f : int -> int t ref end =
  struct type 'a t = new int  let f x = ref x end;; (* should be ok *)

(* Abstract new types *)
module M : sig type t = new type u = new end =
  struct type t = new int type u = new int end;;
type (_,_) comp = Eq : ('a,'a) comp | Diff : ('a,'b) comp;;
fun (x : (M.t,M.u) comp) -> match x with Diff -> false;; (* ok *)

(* new datatypes *)
module M = struct type t = new T   type u = t = T end;; (* fail *)

module M : sig
  type t = new   type t' = new
  type u = new T type u' = new T
  type v = T     type v' = T     type v2 = V
  type z         type z'
end = struct
  type t = new T type t' = new T
  type u = new T type u' = new T
  type v = T     type v' = T     type v2 = V
  type z = T     type z' = T
end;;
fun (x : (M.t,M.t') comp) -> match x with Diff -> false;; (* ok *)
fun (x : (M.t,M.u) comp)  -> match x with Diff -> false;; (* ok *)
fun (x : (M.t,M.v) comp)  -> match x with Diff -> false;; (* ok *)
fun (x : (M.t,M.z) comp)  -> match x with Diff -> false;; (* warn *)
fun (x : (M.u,M.u') comp) -> match x with Diff -> false;; (* ok *)
fun (x : (M.u,M.v) comp)  -> match x with Diff -> false;; (* ok *)
fun (x : (M.u,M.z) comp)  -> match x with Diff -> false;; (* warn *)
fun (x : (M.v,M.v') comp) -> match x with Diff -> false;; (* warn *)
fun (x : (M.v,M.v2) comp) -> match x with Diff -> false;; (* ok *)
fun (x : (M.v,M.z) comp)  -> match x with Diff -> false;; (* warn *)
fun (x : (M.z,M.z') comp) -> match x with Diff -> false;; (* warn *)

(* Now works *)
module M : sig type t = new type _ is_t = I : ('a,t) comp -> 'a is_t end =
  struct type t = new int  type _ is_t = I : ('a,t) comp -> 'a is_t end;;

module N = M;;

let e = M.I Eq;;
let N.I e' = e;;
match e' with Diff -> false;; (* warn *)

(* functors *)
module M = struct type t = new int end;;
module F(X : sig type t = new end) = struct end;;
module FM = F(M);; (* must fail *)
module FM' = F(struct type t = new int end);; (* ok *)

