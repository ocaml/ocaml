(* TEST
   * expect
*)

(* Typing for recursive modules checks scope escape *)
module type S = sig
  type t
end;;

let rec (module M : S) =
  (module struct
    type t = M.t
  end : S
    with type t = M.t)
in
();;
[%%expect{|
module type S = sig type t end
Lines 6-9, characters 2-22:
6 | ..(module struct
7 |     type t = M.t
8 |   end : S
9 |     with type t = M.t)
Error: This expression has type (module S with type t = M.t)
       but an expression was expected of type (module S)
       The type constructor M.t would escape its scope
|}];;

let rec k =
  let (module K : S with type t = A.t) = k in
  (module struct
    type t = K.t
  end : S
    with type t = K.t)
and (module A : S) =
  (module struct
    type t = unit

    let x = ()
  end)
in
();;
[%%expect{|
Line 2, characters 41-42:
2 |   let (module K : S with type t = A.t) = k in
                                             ^
Error: This expression has type 'a but an expression was expected of type
         (module S with type t = A.t)
       The type constructor A.t would escape its scope
|}];;

(* The locally abstract type lets us check the module's type
   without scope escape. *)
let f (type a) () =
  let rec (module M : S with type t = a) =
    (module struct
      type t = M.t
    end : S with type t = M.t)
  in
  ()
;;
[%%expect{|
val f : unit -> unit = <fun>
|}];;

let f (type a) () =
  let rec (module M : S with type t = a) =
    (module struct
      type t = M.t
    end : S with type t = a)
  in
  ();;
[%%expect{|
val f : unit -> unit = <fun>
|}];;

(* Reject scope escape via unification *)

module type S = sig
  type t
  val x : t
end;;

let f () =
  let (module M : S) =
    (module struct
      type t = unit

      let x = ()
    end)
  in
  let unify x = if true then M.x else x in
  unify ()
;;
[%%expect{|
module type S = sig type t val x : t end
Line 15, characters 8-10:
15 |   unify ()
             ^^
Error: This expression has type unit but an expression was expected of type
         M.t
|}, Principal{|
module type S = sig type t val x : t end
Lines 8-12, characters 4-8:
 8 | ....(module struct
 9 |       type t = unit
10 |
11 |       let x = ()
12 |     end)
Warning 18 [not-principal]: this module packing is not principal.

Line 15, characters 8-10:
15 |   unify ()
             ^^
Error: This expression has type unit but an expression was expected of type
         M.t
|}];;
