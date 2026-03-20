(* TEST
 expect;
*)

(* Typing for recursive modules checks scope escape *)
module type S = sig
  type t
end;;

let rec (m : (module S)) =
  let (module M) = m in
  (module struct
    type t = M.t
  end : S
    with type t = M.t)
in
();;
[%%expect{|
module type S = sig type t end
Lines 7-10, characters 2-22:
 7 | ..(module struct
 8 |     type t = M.t
 9 |   end : S
10 |     with type t = M.t)
Error: This expression has type "(module S with type t = M.t)"
       but an expression was expected of type "(module S)"
       The type constructor "M.t" would escape its scope
|}, Principal{|
module type S = sig type t end
Lines 7-10, characters 2-22:
 7 | ..(module struct
 8 |     type t = M.t
 9 |   end : S
10 |     with type t = M.t)
Error: This expression has type "(module S with type t = M.t)"
       but an expression was expected of type "(module S)"
|}];;

let rec k =
  let (module A) = a in
  let (module K : S with type t = A.t) = k in
  (module struct
    type t = K.t
  end : S
    with type t = K.t)
and (a : (module S)) =
  (module struct
    type t = unit

    let x = ()
  end)
in
();;
[%%expect{|
Line 3, characters 41-42:
3 |   let (module K : S with type t = A.t) = k in
                                             ^
Error: The value "k" has type "'a" but an expression was expected of type
         "(module S with type t = A.t)"
       The type constructor "A.t" would escape its scope
|}];;

(* The locally abstract type lets us check the module's type
   without scope escape. *)
let f (type a) () =
  let rec (m : (module S with type t = a)) =
    let (module M) = m in
    (module struct
      type t = M.t
    end : S with type t = M.t)
  in
  ignore m;
  ()
;;
[%%expect{|
val f : unit -> unit = <fun>
|}];;

let f (type a) () =
  let rec (m : (module S with type t = a)) =
    let (module M) = m in
    (module struct
      type t = M.t
    end : S with type t = a)
  in
  ignore m;
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
Error: The constructor "()" has type "unit"
       but an expression was expected of type "M.t"
|}];;
