(* TEST
   * expect
*)

module type Type = sig
  type t
end;;

[%%expect{|
module type Type = sig type t end
|}]

(* Polymorphic functions. *)

let poly (f : {T : Type} -> T.t -> T.t -> T.t) =
  (f {Int} 1 2, f {Bool} true false);;

[%%expect{|
val poly : ({T : Type} -> T.t -> T.t -> T.t) -> Int.t * Bool.t = <fun>
|}]

let choose_fst {T : Type} (x : T.t) (y : T.t) = x;;

let poly_choose_fst = poly choose_fst;;

[%%expect{|
val choose_fst : {T : Type} -> T.t -> T.t -> T.t = <fun>
val poly_choose_fst : Int.t * Bool.t = (1, Bool.true)
|}]

let choose_snd_annot {T : Type} : T.t -> T.t -> T.t = fun x y -> y;;

let poly_choose_snd = poly choose_snd_annot;;

[%%expect{|
val choose_snd_annot : {T : Type} -> T.t -> T.t -> T.t = <fun>
val poly_choose_snd : Int.t * Bool.t = (2, Bool.false)
|}]

let choose_fst_anon : {T : Type} -> T.t -> T.t -> T.t =
  fun {T : Type} (x : T.t) (y : T.t) -> x;;

let poly_choose_fst_anon = poly choose_fst_anon;;

[%%expect{|
val choose_fst_anon : {T : Type} -> T.t -> T.t -> T.t = <fun>
val poly_choose_fst_anon : Int.t * Bool.t = (1, Bool.true)
|}]

let poly_unification_fail1 (f : {T : Type} -> T.t -> T.t -> T.t) =
  f {Int} 1 true;;

[%%expect{|
Line 2, characters 12-16:
2 |   f {Int} 1 true;;
                ^^^^
Error: This expression has type bool but an expression was expected of type
         Int.t = int
|}]

let poly_unification_fail2 (f : {T : Type} -> T.t -> T.t -> T.t) =
  f {Bool} 2 false;;

[%%expect{|
Line 2, characters 11-12:
2 |   f {Bool} 2 false;;
               ^
Error: This expression has type int but an expression was expected of type
         Bool.t = bool
|}]

let poly2 (f : {A : Type} -> {B : Type} -> A.t -> B.t -> (A.t * B.t)) =
  (f {Int} {Bool} 1 true, f {Bool} {Int} false 2);;

[%%expect{|
val poly2 :
  ({A : Type} -> {B : Type} -> A.t -> B.t -> A.t * B.t) ->
  (Int.t * Bool.t) * (Bool.t * Int.t) = <fun>
|}]

(* Higher arity types *)

module type Foldable = sig
  type _ t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

[%%expect{|
module type Foldable =
  sig type _ t val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a end
|}]

let fold_left {F : Foldable} f init x = F.fold_left f init x;;

[%%expect{|
val fold_left : {F : Foldable} -> ('a -> 'b -> 'a) -> 'a -> 'b F.t -> 'a =
  <fun>
|}]

let fold_left_inline {F : Foldable} = F.fold_left;;

[%%expect{|
val fold_left_inline :
  {F : Foldable} -> ('a -> 'b -> 'a) -> 'a -> 'b F.t -> 'a = <fun>
|}]

type 'a array_or_list = Array of 'a Array.t | List of 'a List.t;;

[%%expect{|
type 'a array_or_list = Array of 'a Array.t | List of 'a List.t
|}]

let fold_left_array_or_list f init x =
  match x with
  | Array x -> fold_left {Array} f init x
  | List x -> fold_left {List} f init x;;

[%%expect{|
val fold_left_array_or_list :
  ('a -> 'b -> 'a) -> 'a -> 'b array_or_list -> 'a = <fun>
|}]

let nested_functor_types1 {F : Foldable} {T : Type} f init (x : T.t F.t) =
  fold_left {F} f init x;;

[%%expect{|
val nested_functor_types1 :
  {F : Foldable} -> {T : Type} -> ('a -> T.t -> 'a) -> 'a -> T.t F.t -> 'a =
  <fun>
|}]

let nested_functor_types2 {T : Type} {F : Foldable} f init (x : T.t F.t) =
  fold_left {F} f init x;;

[%%expect{|
val nested_functor_types2 :
  {T : Type} -> {F : Foldable} -> ('a -> T.t -> 'a) -> 'a -> T.t F.t -> 'a =
  <fun>
|}]

(* Free monad: Types in functors and recursion *)

module type Functor = sig
  type _ t

  val map : ('a -> 'b) -> 'a t -> 'b t
end;;

[%%expect{|
module type Functor = sig type _ t val map : ('a -> 'b) -> 'a t -> 'b t end
|}]

module Free_monad (F : Functor) = struct
  type 'a t = Pure of 'a | Free of 'a t F.t
end;;

[%%expect{|
module Free_monad :
  functor (F : Functor) -> sig type 'a t = Pure of 'a | Free of 'a t F.t end
|}]

let rec map :
       {F : Functor}
    -> ('a -> 'b)
    -> 'a Free_monad(F).t
    -> 'b Free_monad(F).t =
  fun {F : Functor} f (x : _ Free_monad(F).t) ->
    ( ( match x with
        | Pure x -> Pure (f x)
        | Free x -> Free (F.map (map {F} f) x) )
    : _ Free_monad(F).t);;

[%%expect{|
val map :
  {F : Functor} -> ('a -> 'b) -> 'a Free_monad(F).t -> 'b Free_monad(F).t =
  <fun>
|}]

let rec bind :
       {F : Functor}
    -> ('a -> 'b Free_monad(F).t)
    -> 'a Free_monad(F).t
    -> 'b Free_monad(F).t =
  fun {F : Functor} f (x : _ Free_monad(F).t) ->
    ( ( match x with
        | Pure x -> f x
        | Free x -> Free (F.map (bind {F} f) x) )
    : _ Free_monad(F).t);;

[%%expect{|
val bind :
  {F : Functor} ->
  ('a -> 'b Free_monad(F).t) -> 'a Free_monad(F).t -> 'b Free_monad(F).t =
  <fun>
|}]

let rec reduce :
       {F : Functor}
    -> ('a -> 'b)
    -> ('b F.t -> 'b)
    -> 'a Free_monad(F).t
    -> 'b =
  fun {F : Functor} f g (x : _ Free_monad(F).t) ->
    match x with
    | Pure x -> f x
    | Free x -> g (F.map (reduce {F} f g) x);;

[%%expect{|
val reduce :
  {F : Functor} -> ('a -> 'b) -> ('b F.t -> 'b) -> 'a Free_monad(F).t -> 'b =
  <fun>
|}]

let option_free =
  (Free (Some (Pure 15)) : _ Free_monad(Option).t)
  |> map {Option} ((+) 1)
  |> bind {Option} (fun x -> Free (Some (Pure (x + 25))));;

[%%expect{|
val option_free : int Free_monad(Option).t =
  Free_monad(Option).Free
   (Option.Some
     (Free_monad(Option).Free (Option.Some (Free_monad(Option).Pure 41))))
|}]

let option_free_reduced =
  reduce {Option} Option.some Option.join option_free;;

[%%expect{|
val option_free_reduced : int option = Some 41
|}]

let list_free =
  (Free [Pure 1; Free [Pure 2; Pure 3]; Pure 4; Pure 5] : _ Free_monad(List).t)
  |> map {List} (( * ) 2)
  |> bind {List} (fun x -> Free [Pure x; Pure (x+5)]);;

[%%expect{|
val list_free : int Free_monad(List).t =
  Free_monad(List).Free
   (List.(::)
     (Free_monad(List).Free
       (List.(::) (Free_monad(List).Pure 2, [Free_monad(List).Pure 7])),
     [Free_monad(List).Free
       (List.(::)
         (Free_monad(List).Free
           (List.(::) (Free_monad(List).Pure 4, [Free_monad(List).Pure 9])),
         [Free_monad(List).Free
           (List.(::) (Free_monad(List).Pure 6, [Free_monad(List).Pure 11]))]));
      Free_monad(List).Free
       (List.(::) (Free_monad(List).Pure 8, [Free_monad(List).Pure 13]));
      Free_monad(List).Free
       (List.(::) (Free_monad(List).Pure 10, [Free_monad(List).Pure 15]))]))
|}]

let list_free_reduced =
  reduce {List} (fun x -> [x]) List.concat list_free;;

[%%expect{|
val list_free_reduced : int list = [2; 7; 4; 9; 6; 11; 8; 13; 10; 15]
|}]

(* Signature narrowing *)

module type Monad = sig
  type _ t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val return : 'a -> 'a t
end;;

[%%expect{|
module type Monad =
  sig
    type _ t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    val return : 'a -> 'a t
  end
|}]

let narrow_sig {M : Monad} (f : {F : Functor} -> 'a F.t -> 'b F.t) x =
  f {M} x;;

[%%expect{|
val narrow_sig :
  {M : Monad} -> ({F : Functor} -> 'a F.t -> 'b F.t) -> 'a M.t -> 'b M.t =
  <fun>
|}]

let no_widen_sig {F : Functor} (f : {M : Monad} -> 'a M.t -> 'b M.t) x =
  f {F} x;;

[%%expect{|
Line 2, characters 2-7:
2 |   f {F} x;;
      ^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a F.t val map : ('a -> 'b) -> 'a t -> 'b t end
       is not included in
         Monad
       The value `return' is required but not provided
       The value `bind' is required but not provided
|}]

(* Functor type bindings *)

let with_type_bindings {T : Type with type t = int} (x : T.t) (y : int) = x + y;;

[%%expect{|
val with_type_bindings : {T : Type with type t = int} -> T.t -> int -> int =
  <fun>
|}]

let apply_with_type_bindings = with_type_bindings {Int} 3 5;;

[%%expect{|
val apply_with_type_bindings : int = 8
|}]

let fail_with_type_bindings = with_type_bindings {Bool};;

[%%expect{|
Line 1, characters 30-55:
1 | let fail_with_type_bindings = with_type_bindings {Bool};;
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         Bool.t = bool
|}]

(* Scope escapes *)

let option_ref = ref None;;

let ref_escape {T : Type} (x : T.t) = option_ref := Some x;;

[%%expect{|
val option_ref : '_weak1 option ref = {contents = None}
Line 3, characters 57-58:
3 | let ref_escape {T : Type} (x : T.t) = option_ref := Some x;;
                                                             ^
Error: This expression has type T.t but an expression was expected of type
         'weak1
       The type constructor T.t would escape its scope
|}]

let var_escape (x : 'a) {T : Type} (y : T.t) = x = y;;

[%%expect{|
Line 1, characters 51-52:
1 | let var_escape (x : 'a) {T : Type} (y : T.t) = x = y;;
                                                       ^
Error: This expression has type T.t but an expression was expected of type 'a
       The type constructor T.t would escape its scope
|}]

(* Nested functors *)

let nested_map1 {M1 : Monad} {M2 : Monad} f (x : _ M1.t M2.t) =
  M2.map (M1.map f) x;;

[%%expect{|
val nested_map1 :
  {M1 : Monad} -> {M2 : Monad} -> ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t =
  <fun>
|}]

let nested_map2 {M2 : Monad} {M1 : Monad} f (x : _ M1.t M2.t) =
  M2.map (M1.map f) x;;

[%%expect{|
val nested_map2 :
  {M2 : Monad} -> {M1 : Monad} -> ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t =
  <fun>
|}]

module type Sig = sig
  module type S
end;;

module Functor_sig = struct module type S = Functor end;;

let nested_sig {S : Sig} {M : S.S} x = x;;

[%%expect{|
module type Sig = sig module type S end
module Functor_sig : sig module type S = Functor end
val nested_sig : {S : Sig} -> {M : S.S} -> 'a -> 'a = <fun>
|}]

let apply_nested_sig x = nested_sig {Functor_sig} {Option} x;;

[%%expect{|
val apply_nested_sig : 'a -> 'a = <fun>
|}]

module type Type_with_sig = sig
  type _ t

  module type S = Functor with type 'a t = 'a t
end;;

module Option_with_sig = struct
  type 'a t = 'a Option.t

  module type S = Functor with type 'a t = 'a t
end;;

let nested_type_with_sig {T : Type_with_sig} {M : T.S} (x : _ T.t) f = M.map f x;;

[%%expect{|
module type Type_with_sig =
  sig
    type _ t
    module type S =
      sig type 'a t = 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
  end
module Option_with_sig :
  sig
    type 'a t = 'a Option.t
    module type S =
      sig type 'a t = 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
  end
val nested_type_with_sig :
  {T : Type_with_sig} -> {M : T.S} -> 'a T.t -> ('a -> 'b) -> 'b M.t = <fun>
|}]

let apply_nested_type_with_sig =
  nested_type_with_sig {Option_with_sig} {Option} None ignore;;

[%%expect{|
val apply_nested_type_with_sig : unit Option.t = Option.None
|}]

module List_functor : Functor with type 'a t = 'a List.t = List;;

let fail_apply_nested_type_with_sig =
  nested_type_with_sig {Option_with_sig} {List_functor} [1; 2; 3] ((+) 1);;

[%%expect{|
module List_functor :
  sig type 'a t = 'a List.t val map : ('a -> 'b) -> 'a t -> 'b t end
Line 4, characters 2-55:
4 |   nested_type_with_sig {Option_with_sig} {List_functor} [1; 2; 3] ((+) 1);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a List.t val map : ('a -> 'b) -> 'a t -> 'b t end
       is not included in
         Option_with_sig.S
       Type declarations do not match:
         type 'a t = 'a List.t
       is not included in
         type 'a t = 'a Option_with_sig.t
|}]

(* Functor record types *)

module Record (T : Type) = struct
  type t = {a: T.t}
end;;

let mk_record {T : Type} (a : T.t) : Record(T).t = {a};;

[%%expect{|
module Record : functor (T : Type) -> sig type t = { a : T.t; } end
val mk_record : {T : Type} -> T.t -> Record(T).t = <fun>
|}]

let record = mk_record {Int} 15;;

[%%expect{|
val record : Record(Int).t = {Record(Int).a = 15}
|}]

let get_record {T : Type} (x : Record(T).t) = x.a;;

[%%expect{|
val get_record : {T : Type} -> Record(T).t -> T.t = <fun>
|}]

module Poly_record (F : Functor) = struct
  type t = {f: 'a 'b. ('a -> 'b F.t) -> 'a F.t -> 'b F.t}
end

let apply_poly_record {F : Functor} ({f} : Poly_record(F).t) = f;;

[%%expect{|
module Poly_record :
  functor (F : Functor) ->
    sig type t = { f : 'a 'b. ('a -> 'b F.t) -> 'a F.t -> 'b F.t; } end
val apply_poly_record :
  {F : Functor} -> Poly_record(F).t -> ('a -> 'b F.t) -> 'a F.t -> 'b F.t =
  <fun>
|}]

let mk_poly_record {F : Functor}
  (f : {A : Type} -> {B : Type} -> (A.t -> B.t F.t) -> A.t F.t -> B.t F.t)
  : Poly_record(F).t =
  let f' (type a b) x y =
    let module A = struct type t = a end in
    let module B = struct type t = b end in
    f {A} {B} x y
  in
  {f= f'};;

[%%expect{|
val mk_poly_record :
  {F : Functor} ->
  ({A : Type} -> {B : Type} -> (A.t -> B.t F.t) -> A.t F.t -> B.t F.t) ->
  Poly_record(F).t = <fun>
|}]

(* Variant type expansion *)

module type Variant = sig
  type 'a t = ([>] as 'a)
end;;

let apply_variant {V : Variant} (f : [`A | `B | `C] -> unit) (x : _ V.t) = f x;;

[%%expect{|
module type Variant = sig type 'a t = 'a constraint 'a = [>  ] end
val apply_variant :
  {V : Variant} -> ([ `A | `B | `C ] -> unit) -> [ `A | `B | `C ] V.t -> unit =
  <fun>
|}]

module type Variant2 = sig
  type 'a t = ([< `A | `B | `C | `D > `A `B `C] as 'a)
end;;

let apply_variant2 {V : Variant2} (f : [`A | `B | `C] -> unit) (x : _ V.t) = f x;;

[%%expect{|
module type Variant2 =
  sig type 'a t = 'a constraint 'a = [< `A | `B | `C | `D > `A `B `C ] end
val apply_variant2 :
  {V : Variant2} ->
  ([ `A | `B | `C ] -> unit) -> [ `A | `B | `C ] V.t -> unit = <fun>
|}]

(* Interaction with packed modules *)

let fail_unpack_apply1 (module F : Functor) f x = bind {F} f x;;

[%%expect{|
Line 1, characters 59-60:
1 | let fail_unpack_apply1 (module F : Functor) f x = bind {F} f x;;
                                                               ^
Error: This expression has type 'a but an expression was expected of type
         'b -> 'c Free_monad(F).t
       The type constructor Free_monad(F).t would escape its scope
|}]

let fail_unpack_apply2 {F : Functor} =
  let (module F' : Functor) = (module F : Functor) in
  bind {F'};;

[%%expect{|
Line 3, characters 2-11:
3 |   bind {F'};;
      ^^^^^^^^^
Error: This expression has type
         ('a -> 'b Free_monad(F').t) ->
         'a Free_monad(F').t -> 'b Free_monad(F').t
       but an expression was expected of type 'c
       The type constructor Free_monad(F').t would escape its scope
|}]

let unpack_exposes_type_alias (type t) (module M : Type with type t = t)
    (f : {T : Type} -> T.t -> unit) =
  f {M};;

[%%expect{|
val unpack_exposes_type_alias :
  (module Type with type t = 't) -> ({T : Type} -> T.t -> unit) -> 't -> unit =
  <fun>
|}]

(* Co-recursive definitions *)
let rec f {T : Type} = g () and g () = f {Int};;

[%%expect{|
Line 1, characters 10-27:
1 | let rec f {T : Type} = g () and g () = f {Int};;
              ^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}]
