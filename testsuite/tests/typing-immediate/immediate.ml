module type S = sig type t [@@immediate] end;;
module F (M : S) : S = M;;

(* VALID DECLARATIONS *)

module A = struct
  (* Abstract types can be immediate *)
  type t [@@immediate]

  (* [@@immediate] tag here is unnecessary but valid since t has it *)
  type s = t [@@immediate]

  (* Again, valid alias even without tag *)
  type r = s

  (* Mutually recursive declarations work as well *)
  type p = q [@@immediate]
  and q = int
end;;

(* Valid using with constraints *)
module type X = sig type t end;;
module Y = struct type t = int end;;
module Z = ((Y : X with type t = int) : sig type t [@@immediate] end);;

(* Valid using an explicit signature *)
module M_valid : S = struct type t = int end;;
module FM_valid = F (struct type t = int end);;

(* Practical usage over modules *)
module Foo : sig type t val x : t ref end = struct
  type t = int
  let x = ref 0
end;;

module Bar : sig type t [@@immediate] val x : t ref end = struct
  type t = int
  let x = ref 0
end;;

let test f =
  let start = Sys.time() in f ();
  (Sys.time() -. start);;

let test_foo () =
  for i = 0 to 100_000_000 do
    Foo.x := !Foo.x
  done;;

let test_bar () =
  for i = 0 to 100_000_000 do
    Bar.x := !Bar.x
  done;;

(* Uncomment these to test. Should see substantial speedup!
let () = Printf.printf "No @@immediate: %fs\n" (test test_foo)
let () = Printf.printf "With @@immediate: %fs\n" (test test_bar) *)


(* INVALID DECLARATIONS *)

(* Cannot directly declare a non-immediate type as immediate *)
module B = struct
  type t = string [@@immediate]
end;;

(* Not guaranteed that t is immediate, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@immediate]
end;;

(* Can't ascribe to an immediate type signature with a non-immediate type *)
module D : sig type t [@@immediate] end = struct
  type t = string
end;;

(* Same as above but with explicit signature *)
module M_invalid : S = struct type t = string end;;
module FM_invalid = F (struct type t = string end);;

(* Can't use a non-immediate type even if mutually recursive *)
module E = struct
  type t = s [@@immediate]
  and s = string
end;;
