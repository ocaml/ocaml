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
