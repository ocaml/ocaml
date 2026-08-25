(* TEST
 flags = "-drawlambda -dcanonical-ids";
 expect;
*)

(* Checking that the optimisation for avoiding capture
   of simple aliases in non-syntactic recursive function definitions
   works properly *)

(* Original example from issue number 14876 *)
type expr = Lit of int | Many of expr list

let rec depth : expr -> int =
    let __0 = depth in
    function
    | Lit _ -> 0
    | Many l ->
      List.fold_left (fun acc b -> Int.max acc (__0 b)) 0 l
[%%expect {|
0
type expr = Lit of int | Many of expr list
(let (letrec_function_context/0 = (caml_alloc_dummy 0))
  (letrec
    (depth/0
       (function param/0 : int
         (switch* param/0
          case tag 0: (let (*match*/0 =a (field_imm 0 param/0)) 0)
          case tag 1:
           (let (l/0 =a (field_imm 0 param/0))
             (apply (field_imm 29 (global Stdlib__List!))
               (function acc/0[int] b/0 : int
                 (apply (field_imm 14 (global Stdlib__Int!)) acc/0
                   (apply depth/0 b/0)))
               0 l/0)))))
    (seq
      (caml_update_dummy letrec_function_context/0
        (let (__0/0 = depth/0) (makeblock 0)))
      (apply (field_mut 1 (global Toploop!)) "depth" depth/0))))
val depth : expr -> int = <fun>
|}];;

(* A few more complicated cases, with capture of renamed variables.
   Only [b] should end up as part of [foo]'s function context. *)

let rec foo =
  let x = foo in
  let y = x in
  let z = bar in
  let a = z in
  let b () = y (); a () in
  fun () -> x (); y (); z (); a (); b ()
and bar () = foo ()
[%%expect {|
(let (letrec_function_context/1 = (caml_alloc_dummy 1))
  (letrec
    (foo/0
       (function param/1[int]
         (seq (apply foo/0 0) (apply foo/0 0) (apply bar/0 0) (apply bar/0 0)
           (apply (field_imm 0 letrec_function_context/1) 0)))
      bar/0 (function param/2[int] (apply foo/0 0)))
    (seq
      (caml_update_dummy letrec_function_context/1
        (let
          (x/0 = foo/0
           y/0 = x/0
           z/0 = bar/0
           a/0 = z/0
           b/1 = (function param/3[int] (seq (apply y/0 0) (apply a/0 0))))
          (makeblock 0 b/1)))
      (apply (field_mut 1 (global Toploop!)) "foo" foo/0)
      (apply (field_mut 1 (global Toploop!)) "bar" bar/0))))
val foo : unit -> 'a = <fun>
val bar : unit -> 'a = <fun>
|}];;

(* Examples with module projections (global and local) *)

let rec bar =
  let __0 = Hashtbl.hash in
  fun () -> __0
[%%expect {|
(let (letrec_function_context/2 = (caml_alloc_dummy 0))
  (letrec
    (bar/1 (function param/4[int] (field_imm 29 (global Stdlib__Hashtbl!))))
    (seq
      (caml_update_dummy letrec_function_context/2
        (let (__0/1 = (field_imm 29 (global Stdlib__Hashtbl!)))
          (makeblock 0)))
      (apply (field_mut 1 (global Toploop!)) "bar" bar/1))))
val bar : unit -> 'a -> int = <fun>
|}]

module M = struct let x = 0 end

let rec baz =
  let x = M.x in
  fun () -> x
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "M/466"
  (let (x/1 =[int] 0) (makeblock 0 x/1)))
module M : sig val x : int end
(let
  (M/0 = (apply (field_mut 0 (global Toploop!)) "M/466")
   letrec_function_context/3 = (caml_alloc_dummy 0))
  (letrec (baz/0 (function param/5[int] : int (field_imm 0 M/0)))
    (seq
      (caml_update_dummy letrec_function_context/3
        (let (x/2 =[int] (field_imm 0 M/0)) (makeblock 0)))
      (apply (field_mut 1 (global Toploop!)) "baz" baz/0))))
val baz : unit -> int = <fun>
|}];;


(* Some case not currently handled, for reference. *)

let rec foobar =
  let x = (1, 2) in
  let z = x in
  fun () -> z
[%%expect {|
(let (letrec_function_context/4 = (caml_alloc_dummy 1))
  (letrec
    (foobar/0 (function param/6[int] (field_imm 0 letrec_function_context/4)))
    (seq
      (caml_update_dummy letrec_function_context/4
        (let (x/3 = [0: 1 2] z/1 = x/3) (makeblock 0 x/3)))
      (apply (field_mut 1 (global Toploop!)) "foobar" foobar/0))))
val foobar : unit -> int * int = <fun>
|}];;
