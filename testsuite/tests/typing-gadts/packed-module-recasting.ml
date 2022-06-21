(* TEST
   * expect
*)

type (_, _) eq = Refl : ('a, 'a) eq;;
[%%expect {|
type (_, _) eq = Refl : ('a, 'a) eq
|}]

module type S = sig
  type t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
end;;

let cast_type_under_equality (type t) (module M : S' with type t = t) :
    (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t end
module type S' = sig type _ t_aux type t val eq : (t, unit t_aux) eq end
val cast_type_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t

  val x : t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
  val x : t
end;;

let cast_value_under_equality (type t) (module M : S' with type t = t) :
    (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t val x : t end
module type S' =
  sig type _ t_aux type t val eq : (t, unit t_aux) eq val x : t end
val cast_value_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
  type u = t

  val x : u
end;;

module type S' = sig
  type _ t_aux
  type t
  type u = unit t_aux

  val eq : (t, unit t_aux) eq
  val x : u
end;;

let cast_value_under_manifest_equality (type t) (module M : S' with type t = t)
    : (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t type u = t val x : u end
module type S' =
  sig
    type _ t_aux
    type t
    type u = unit t_aux
    val eq : (t, unit t_aux) eq
    val x : u
  end
val cast_value_under_manifest_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
  type u = A of t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq

  type u = A of unit t_aux
end;;

let cast_constructor_under_equality (type t) (module M : S' with type t = t) :
    (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t type u = A of t end
module type S' =
  sig
    type _ t_aux
    type t
    val eq : (t, unit t_aux) eq
    type u = A of unit t_aux
  end
val cast_constructor_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
  type u = ..
  type u += A of t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq

  type u = ..
  type u += A of unit t_aux
end;;

let cast_extension_constructor_under_equality (type t)
    (module M : S' with type t = t) : (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t type u = .. type u += A of t end
module type S' =
  sig
    type _ t_aux
    type t
    val eq : (t, unit t_aux) eq
    type u = ..
    type u += A of unit t_aux
  end
val cast_extension_constructor_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
  type u = { x : t }
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq

  type u = { x : unit t_aux }
end;;

let cast_record_under_equality (type t) (module M : S' with type t = t) :
    (module S with type t = t) =
  let Refl = M.eq in
  (module M);;
[%%expect {|
module type S = sig type t type u = { x : t; } end
module type S' =
  sig
    type _ t_aux
    type t
    val eq : (t, unit t_aux) eq
    type u = { x : unit t_aux; }
  end
val cast_record_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
end;;

let cast_indirect_under_equality (type t)
    (module M : S' with type t = t) : (module S with type t = t)
    =
  let module N : S' with type 'a t_aux = 'a M.t_aux = M in
  let Refl = M.eq in
  let Refl = N.eq in
  if true then (module M) else (module N)
[%%expect {|
module type S = sig type t end
module type S' = sig type _ t_aux type t val eq : (t, unit t_aux) eq end
val cast_indirect_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
end;;

module type F = functor (M : S) -> S with type t = M.t;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
end;;

let cast_functor_argument_under_equality (type t)
    (module M : S' with type t = t) (module F : F) : (module S with type t = t)
    =
  let Refl = M.eq in
  (module F (M))
[%%expect {|
module type S = sig type t end
module type F = functor (M : S) -> sig type t = M.t end
module type S' = sig type _ t_aux type t val eq : (t, unit t_aux) eq end
val cast_functor_argument_under_equality :
  (module S' with type t = 't) -> (module F) -> (module S with type t = 't) =
  <fun>
|}]

module type S = sig
  type t
end;;

module type F = functor (M : S) -> sig
  module type S = sig
    type t = M.t
  end
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
end;;

let cast_functor_argument_signature_under_equality (type t)
    (module M : S' with type t = t) (module F : F) : (module S with type t = t)
    =
  let Refl = M.eq in
  (module M : F(M).S)
[%%expect {|
module type S = sig type t end
module type F =
  functor (M : S) -> sig module type S = sig type t = M.t end end
module type S' = sig type _ t_aux type t val eq : (t, unit t_aux) eq end
val cast_functor_argument_signature_under_equality :
  (module S' with type t = 't) -> (module F) -> (module S with type t = 't) =
  <fun>
|}]

let cast_double_functor_argument_signature_under_equality (type t)
    (module M : S' with type t = t) (module F : F) : (module S with type t = t)
    =
  let Refl = M.eq in
  let module N : F(M).S = M in
  let module O : F(N).S = N in
  (module (O : F(M).S) : F(M).S)
[%%expect {|
val cast_double_functor_argument_signature_under_equality :
  (module S' with type t = 't) -> (module F) -> (module S with type t = 't) =
  <fun>
|}]

module type S = sig
  type t

  module type S_inner = sig
    type nonrec t = t
  end
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq

  module type S_inner = sig
    type nonrec t = t
  end
end;;

let cast_module_type_under_equality (type t) (module M : S' with type t = t) :
    (module S with type t = t) =
  let Refl = M.eq in
  (module M)
[%%expect {|
module type S =
  sig type t module type S_inner = sig type nonrec t = t end end
module type S' =
  sig
    type _ t_aux
    type t
    val eq : (t, unit t_aux) eq
    module type S_inner = sig type nonrec t = t end
  end
val cast_module_type_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

module type S = sig
  type t
end;;

module type S' = sig
  type _ t_aux
  type t

  val eq : (t, unit t_aux) eq
end;;

let cast_via_module_type_under_equality (type t) (module M : S' with type t = t)
    : (module S with type t = t) =
  let Refl = M.eq in
  let module N = struct
    module M : S' with type t = t = M
  end in
  (module N.M)
[%%expect {|
module type S = sig type t end
module type S' = sig type _ t_aux type t val eq : (t, unit t_aux) eq end
val cast_via_module_type_under_equality :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

let cast_via_module_type_under_equality2 (type t) (module M : S' with type t = t)
    : (module S with type t = t) =
  let Refl = M.eq in
  let module N = struct
    module M : S' with type t = M.t = M
  end in
  (module N.M)
[%%expect {|
val cast_via_module_type_under_equality2 :
  (module S' with type t = 't) -> (module S with type t = 't) = <fun>
|}]

(* Test from issue #10768 *)
type _ t

module type S = sig type t end

type pack = Pack : 'a t *  (module S with type t = 'a) -> pack

let dispatch : pack list ref = ref []

exception Not_registered

type (_,_) equal =
  | Refl : 'a -> ('a,'a) equal

type equality =
  {
    equal: 'a 'b. ('a t * 'b t) -> ('a t,'b t) equal option
  }

let equal : equality ref = ref {equal = fun _ -> None}

let get (type a) : a t -> (module S with type t = a)  = fun index ->
  let dispatch = !dispatch in
  let rec unpack list =
    match list with
    | [] -> raise Not_registered
    | Pack (index', (module P))::list' ->
      match !equal.equal (index, index') with
      | Some (Refl _) ->  (module P : ( S with type t = a))
        | None -> unpack list'
  in
  unpack dispatch
[%%expect {|
type _ t
module type S = sig type t end
type pack = Pack : 'a t * (module S with type t = 'a) -> pack
val dispatch : pack list ref = {contents = []}
exception Not_registered
type (_, _) equal = Refl : 'a -> ('a, 'a) equal
type equality = { equal : 'a 'b. 'a t * 'b t -> ('a t, 'b t) equal option; }
val equal : equality ref = {contents = {equal = <fun>}}
val get : 'a t -> (module S with type t = 'a) = <fun>
|}]


(* Ambivalance via module expression *)
(* Both should fail *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
  let module M = struct let g = g end in
  let Refl = w1 in let Refl = w2 in M.g 3;;
[%%expect {|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}, Principal{|
Line 3, characters 36-41:
3 |   let Refl = w1 in let Refl = w2 in M.g 3;;
                                        ^^^^^
Error: This expression has type b = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let module M = struct let g = g end in
   let Refl = w2 in let Refl = w1 in M.g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> int = <fun>
|}, Principal{|
Line 3, characters 37-42:
3 |    let Refl = w2 in let Refl = w1 in M.g 3;;
                                         ^^^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

(* Ambivalance via packed module *)
(* Both should fail *)
module type S = sig
  type a
  val g : a
end;;
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq)
    (module M : S with type a = a) =
  let Refl = w1 in let Refl = w2 in M.g 3
[%%expect {|
module type S = sig type a val g : a end
val f :
  ('a, 'b -> 'b) eq ->
  ('a, int -> int) eq -> (module S with type a = 'a) -> 'b = <fun>
|}, Principal{|
module type S = sig type a val g : a end
Line 7, characters 36-41:
7 |   let Refl = w1 in let Refl = w2 in M.g 3
                                        ^^^^^
Error: This expression has type b = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq)
    (module M : S with type a = a) =
  let Refl = w2 in let Refl = w1 in M.g 3
[%%expect{|
val f :
  ('a, 'b -> 'b) eq ->
  ('a, int -> int) eq -> (module S with type a = 'a) -> int = <fun>
|}, Principal{|
Line 3, characters 36-41:
3 |   let Refl = w2 in let Refl = w1 in M.g 3
                                        ^^^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

(* Ambivalance in module expression *)
(* Both should fail *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
  let Refl = w1 in let Refl = w2 in
  let module M = struct let res = g 3 end in
  M.res;;
[%%expect {|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}, Principal{|
Line 4, characters 2-7:
4 |   M.res;;
      ^^^^^
Error: This expression has type b = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w2 in let Refl = w1 in
   let module M = struct let res = g 3 end in
   M.res;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> int = <fun>
|}, Principal{|
Line 4, characters 3-8:
4 |    M.res;;
       ^^^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
