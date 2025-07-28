(* TEST
 expect;
*)

(* This is a regression test.  The include functor version of the below program
   used to typecheck - we check here it gets the same error as the
   non-include-functor version. *)

module type T = sig
  type t
end

module Ref (A : T) : sig
  val r : A.t option ref
end = struct
  let r = ref None
end;;
[%%expect{|
module type T = sig type t end
module Ref : (A : T) -> sig val r : A.t option ref end
|}]


(* Legacy version *)
let r (type a) =
  let module R = struct
    module T = struct
      type t = a
    end

    include Ref(T)
  end
  in
  R.r
;;

let magic (type a b) (a : a) : b =
  r := Some a;
  match !r with
  | Some r -> r
  | None -> assert false
;;
[%%expect{|
val r : '_a option ref = {contents = None}
Line 14, characters 12-13:
14 |   r := Some a;
                 ^
Error: The value "a" has type "a" but an expression was expected of type "'a"
       The type constructor "a" would escape its scope
|}]

(* Include functor version *)
let r (type a) =
  let module R = struct
    type t = a

    include functor Ref
  end
  in
  R.r
;;

let magic (type a b) (a : a) : b =
  r := Some a;
  match !r with
  | Some r -> r
  | None -> assert false
;;
[%%expect{|
val r : '_a option ref = {contents = None}
Line 12, characters 12-13:
12 |   r := Some a;
                 ^
Error: The value "a" has type "a" but an expression was expected of type "'a"
       The type constructor "a" would escape its scope
|}]
