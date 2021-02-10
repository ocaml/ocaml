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
Line 2, characters 5-6:
2 |   f {F} x;;
         ^
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
Line 1, characters 50-54:
1 | let fail_with_type_bindings = with_type_bindings {Bool};;
                                                      ^^^^
Error: This expression has type (module Type with type t = Bool.t)
       but an expression was expected of type (module Type with type t = int)
       Type Bool.t = bool is not compatible with type int
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
Line 4, characters 42-54:
4 |   nested_type_with_sig {Option_with_sig} {List_functor} [1; 2; 3] ((+) 1);;
                                              ^^^^^^^^^^^^
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
Error: This expression has type 'a but an expression was expected of type
         ('b -> 'c Free_monad(F').t) ->
         'b Free_monad(F').t -> 'c Free_monad(F').t
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
val f : {T : Type} -> 'a = <fun>
val g : unit -> 'a = <fun>
|}]

(* Apply to functor modules *)

module type Createable = sig
  type 'a t

  val create : 'a -> 'a t
end;;

[%%expect{|
module type Createable = sig type 'a t val create : 'a -> 'a t end
|}]

let create {M : Createable} x = M.create x;;

[%%expect{|
val create : {M : Createable} -> 'a -> 'a M.t = <fun>
|}]

module Int_create = struct
  type _ t = int
  let create _ = 19
end;;

module type Int_create = module type of Int_create;;

module Option_create = struct
  type 'a t = 'a option
  let create x = Some x
end;;

module List_create = struct
  type 'a t = 'a list
  let create x = [x]
end;;

module Pair_create (T1 : Createable) (T2 : Createable) = struct
  type 'a t = 'a T1.t * 'a T2.t

  let create x = (T1.create x, T2.create x)
end;;

module List_of_create (T : Createable) = struct
  type 'a t = 'a T.t list
  let create x = [T.create x]
end;;

[%%expect{|
module Int_create : sig type _ t = int val create : 'a -> int end
module type Int_create = sig type _ t = int val create : 'a -> int end
module Option_create :
  sig type 'a t = 'a option val create : 'a -> 'a option end
module List_create : sig type 'a t = 'a list val create : 'a -> 'a list end
module Pair_create :
  functor (T1 : Createable) (T2 : Createable) ->
    sig
      type 'a t = 'a T1.t * 'a T2.t
      val create : 'a -> 'a T1.t * 'a T2.t
    end
module List_of_create :
  functor (T : Createable) ->
    sig type 'a t = 'a T.t list val create : 'a -> 'a T.t list end
|}]

let create_apply_functors =
  create {Pair_create(Pair_create(Int_create)(Option_create))(List_of_create(Option_create))} 15;;

[%%expect{|
val create_apply_functors :
  int
  Pair_create(Pair_create(Int_create)(Option_create))(List_of_create(Option_create)).t =
  ((19, Some 15), [Some 15])
|}]

(* Apply to different module kinds. *)

module Opaque_create : Createable = struct
  type 'a t = 'a option
  let create x = Some x
end;;

module Opaque_of_create (T : Createable) : Createable = struct
  include T
end;;

module Opaque_of_left_create (T1 : Createable) (T2 : Createable) =
  Opaque_of_create(T1);;

[%%expect{|
module Opaque_create : Createable
module Opaque_of_create : functor (T : Createable) -> Createable
module Opaque_of_left_create :
  functor (T1 : Createable) (T2 : Createable) ->
    sig type 'a t = 'a Opaque_of_create(T1).t val create : 'a -> 'a t end
|}]

let create_apply_opaque = create {Opaque_create} 15;;

[%%expect{|
val create_apply_opaque : int Opaque_create.t = <abstr>
|}]

let create_apply_opaque_functor =
  create {Pair_create(Opaque_create)(Opaque_create)} 15;;

[%%expect{|
val create_apply_opaque_functor :
  int Pair_create(Opaque_create)(Opaque_create).t = (<abstr>, <abstr>)
|}]

let create_apply_functor_opaque =
  create {Opaque_of_create(Int_create)} 15;;

[%%expect{|
val create_apply_functor_opaque : int Opaque_of_create(Int_create).t =
  <abstr>
|}]

(* We would like the next several examples to typecheck, but without proper
   support in the environment for functor aliases we would have confusing and
   inconsistent behaviour.
   [create_opaque_apply_left] and [create_opaque_apply_left_inline] below are
   designed to exercise this inconsistency.
*)

let create_apply_inline =
  create
    {struct
      type 'a t = 'a option
      let create x = Some x
    end}
    15;;

[%%expect{|
Lines 3-6, characters 5-7:
3 | .....struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end.
Error: This module does not have a canonical path.
|}]

let create_apply_functor_inline =
  create
    {List_of_create(struct
      type 'a t = 'a option
      let create x = Some x
    end)}
    15;;

[%%expect{|
Lines 3-6, characters 20-7:
3 | ....................struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end..
Error: This module does not have a canonical path.
|}]

let create_apply_opaque_functor_inline_fail =
  create
    {Opaque_of_create(struct
      type 'a t = 'a option
      let create x = Some x
    end)}
    15;;

[%%expect{|
Lines 3-6, characters 22-7:
3 | ......................struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end..
Error: This module does not have a canonical path.
|}]

let create_apply_unpacked =
  create {(val (module Int_create : Int_create))} ();;

[%%expect{|
Line 2, characters 10-48:
2 |   create {(val (module Int_create : Int_create))} ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

let create_apply_unpacked_opaque =
  create {(val (module Opaque_create : Createable))} ();;

[%%expect{|
Line 2, characters 10-51:
2 |   create {(val (module Opaque_create : Createable))} ();;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

let create_opaque {T : Createable} (x : 'a) : 'a Opaque_of_create(T).t =
  let module M = Opaque_of_create(T) in
  M.create x;;

[%%expect{|
val create_opaque : {T : Createable} -> 'a -> 'a Opaque_of_create(T).t =
  <fun>
|}]

let create_opaque_apply_functors =
  create_opaque
    {Pair_create(Pair_create(Int_create)(Option_create))(List_of_create(Option_create))}
    15;;

[%%expect{|
val create_opaque_apply_functors :
  int
  Opaque_of_create(Pair_create(Pair_create(Int_create)(Option_create))(List_of_create(Option_create))).t =
  <abstr>
|}]

let create_opaque_inline_fail =
  create_opaque
    {struct
      type 'a t = 'a option
      let create x = Some x
    end}
    15;;

[%%expect{|
Lines 3-6, characters 5-7:
3 | .....struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end.
Error: This module does not have a canonical path.
|}]

let create_opaque_apply_left =
  create_opaque {Opaque_of_left_create(Opaque_create)(Int_create)} 15;;

[%%expect{|
val create_opaque_apply_left :
  int Opaque_of_create(Opaque_of_left_create(Opaque_create)(Int_create)).t =
  <abstr>
|}]

(* We would like this to have type
   [int Opaque_of_create(Opaque_of_create(Opaque_create)).t],
   but currently functor application erases aliases, and so the identity
   [Opaque_of_left_create(Opaque_create)(_) = Opaque_create(Opaque_create)]
   is lost.
*)

let create_opaque_apply_left_inline =
  create_opaque
    {Opaque_of_left_create(Opaque_create)
      (struct
        type 'a t = 'a option
        let create x = Some x
      end)}
    15;;

[%%expect{|
Lines 4-7, characters 7-9:
4 | .......struct
5 |         type 'a t = 'a option
6 |         let create x = Some x
7 |       end..
Error: This module does not have a canonical path.
|}]

let create_opaque_apply_unpacked =
  create_opaque {(val (module Int_create : Int_create))} ();;

[%%expect{|
Line 2, characters 17-55:
2 |   create_opaque {(val (module Int_create : Int_create))} ();;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

let create_opaque_apply_unpacked_opaque =
  create_opaque {(val (module Opaque_create : Createable))} ();;

[%%expect{|
Line 2, characters 17-58:
2 |   create_opaque {(val (module Opaque_create : Createable))} ();;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

let create_unused {M : Createable} () = ();;

[%%expect{|
val create_unused : {M : Createable} -> unit -> unit = <fun>
|}]

let create_unused_apply_opaque = create_unused {Opaque_create} ();;

[%%expect{|
val create_unused_apply_opaque : unit = ()
|}]

let create_unused_apply_opaque_functor =
  create_unused {Pair_create(Opaque_create)(Opaque_create)} ();;

[%%expect{|
val create_unused_apply_opaque_functor : unit = ()
|}]

let create_unused_apply_functor_opaque =
  create_unused {Opaque_of_create(Int_create)} ();;

[%%expect{|
val create_unused_apply_functor_opaque : unit = ()
|}]

let create_unused_apply_inline =
  create_unused
    {struct
      type 'a t = 'a option
      let create x = Some x
    end}
    ();;

[%%expect{|
Lines 3-6, characters 5-7:
3 | .....struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end.
Error: This module does not have a canonical path.
|}]

let create_unused_apply_functor_inline =
  create_unused
    {List_of_create(struct
      type 'a t = 'a option
      let create x = Some x
    end)}
    ();;

[%%expect{|
Lines 3-6, characters 20-7:
3 | ....................struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end..
Error: This module does not have a canonical path.
|}]

let create_unused_apply_opaque_functor_inline =
  create_unused
    {Opaque_of_create(struct
      type 'a t = 'a option
      let create x = Some x
    end)}
    ();;

[%%expect{|
Lines 3-6, characters 22-7:
3 | ......................struct
4 |       type 'a t = 'a option
5 |       let create x = Some x
6 |     end..
Error: This module does not have a canonical path.
|}]

let create_unused_apply_unpacked =
  create_unused {(val (module Int_create : Int_create))} ();;

[%%expect{|
Line 2, characters 17-55:
2 |   create_unused {(val (module Int_create : Int_create))} ();;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

let create_unused_apply_unpacked_opaque =
  create_unused {(val (module Opaque_create : Createable))} ();;

[%%expect{|
Line 2, characters 17-58:
2 |   create_unused {(val (module Opaque_create : Createable))} ();;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module does not have a canonical path.
|}]

(* Inferring functor type. *)

let infer_arg_type : {T : Type} -> T.t -> T.t = fun {M} x -> x;;

[%%expect{|
val infer_arg_type : {T : Type} -> T.t -> T.t = <fun>
|}]

let infer_arg_type_mismatch_fail : {T1 : Type} -> {T2 : Type} -> T1.t -> T2.t =
  fun {T1} {T2} x -> x;;

[%%expect{|
Line 2, characters 21-22:
2 |   fun {T1} {T2} x -> x;;
                         ^
Error: This expression has type T1.t but an expression was expected of type
         T2.t
|}]

let infer_arg_type_ambiguous_fail : _ = fun {T1} x -> x;;

[%%expect{|
Line 1, characters 40-55:
1 | let infer_arg_type_ambiguous_fail : _ = fun {T1} x -> x;;
                                            ^^^^^^^^^^^^^^^
Error: The signature for this functor couldn't be inferred.
|}]

let infer_arg_type_ambiguous_nested_fail : {T1 : Type} -> _ =
  fun {T1} {T2} x -> x;;

[%%expect{|
Line 2, characters 11-22:
2 |   fun {T1} {T2} x -> x;;
               ^^^^^^^^^^^
Error: The signature for this functor couldn't be inferred.
|}]

let incompatible_type_sig : int * bool = fun {T : Type} x -> x;;

[%%expect{|
Line 1, characters 41-62:
1 | let incompatible_type_sig : int * bool = fun {T : Type} x -> x;;
                                             ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is functor, but the expected type is int * bool
|}]

let incompatible_type_nosig : int * bool = fun {T} x -> x;;

[%%expect{|
Line 1, characters 43-57:
1 | let incompatible_type_nosig : int * bool = fun {T} x -> x;;
                                               ^^^^^^^^^^^^^^
Error: This expression is functor, but the expected type is int * bool
|}]

let infer_arg_type_apply (f : ({T : Type} -> T.t -> T.t) -> int -> int) =
  f (fun {T} x -> x) 0;;

[%%expect{|
val infer_arg_type_apply : (({T : Type} -> T.t -> T.t) -> int -> int) -> int =
  <fun>
|}]

let infer_arg_type_with_constraint
  : {T : Type with type t = bool} -> T.t -> int =
  fun {T} x -> if x then 1 else 0;;

[%%expect{|
val infer_arg_type_with_constraint :
  {T : Type with type t = bool} -> T.t -> int = <fun>
|}]

let infer_arg_type_with_constraint_fail
  : {T : Type with type t = int} -> T.t -> int =
  fun {T} x -> if x then 1 else 0;;

[%%expect{|
Line 3, characters 18-19:
3 |   fun {T} x -> if x then 1 else 0;;
                      ^
Error: This expression has type T.t = int
       but an expression was expected of type bool
       because it is in the condition of an if-statement
|}]

let escape_check_with_inferred_arg : {T : Type} -> 'arg -> 'ret =
  fun {T} (x : T.t) -> x

[%%expect{|
Line 2, characters 10-19:
2 |   fun {T} (x : T.t) -> x
              ^^^^^^^^^
Error: This pattern matches values of type T.t
       but a pattern was expected which matches values of type 'arg
       The type constructor T.t would escape its scope
|}]

let mismatched_annotation : {T : Type} -> T.t -> T.t =
  fun {F : Foldable} (x : _ F.t) -> x;;

[%%expect{|
Line 2, characters 2-37:
2 |   fun {F : Foldable} (x : _ F.t) -> x;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type {T : Foldable} -> 'a
       but an expression was expected of type {T : Type} -> 'b
|}]

(* Expansions and normalisations in the module type *)

module type Normalise = sig
  type t
  type _ t_alias = t
  val create : unit -> t
end

module Normalise_test = struct
  type t = int
  type _ t_alias = t
  let create () = 15
end

module Type_pair (T : sig type t end) (M : sig type t end) = struct
  type t = T.t * M.t

  module Opaque : sig
    type 'a t
    val create : 'a -> 'a t
  end = struct
    type 'a t = unit
    let create _ = ()
  end

  module type S = sig
    type t = M.t * T.t
  end
end;;

[%%expect{|
module type Normalise =
  sig type t type _ t_alias = t val create : unit -> t end
module Normalise_test :
  sig type t = int type _ t_alias = t val create : unit -> int end
module Type_pair :
  functor (T : sig type t end) (M : sig type t end) ->
    sig
      type t = T.t * M.t
      module Opaque : sig type 'a t val create : 'a -> 'a t end
      module type S = sig type t = M.t * T.t end
    end
|}]

let expand_alias =
  let module Int2 = Int in
  let f {M : Normalise} : Int2.t M.t_alias = M.create () in
  f;;

[%%expect{|
val expand_alias : {M : Normalise} -> M.t = <fun>
|}]

let test_expand_alias = expand_alias {Normalise_test};;

[%%expect{|
val test_expand_alias : Normalise_test.t = 15
|}]

let normalise_functor_type =
  let module Int2 = Int in
  let f {M : Normalise} : Type_pair(Int2)(M).t = (1, M.create ()) in
  f;;

[%%expect{|
val normalise_functor_type : {M : Normalise} -> int * M.t = <fun>
|}]

let test_normalise_functor_type = normalise_functor_type {Normalise_test};;

[%%expect{|
val test_normalise_functor_type : int * Normalise_test.t = (1, 15)
|}]

let normalise_functor_type_opaque =
  let module Int2 = Int in
  let f {M : Normalise} : M.t Type_pair(Int2)(M).Opaque.t =
    let module N = Type_pair(Int2)(M) in
    N.Opaque.create (M.create ())
  in
  f;;

[%%expect{|
val normalise_functor_type_opaque :
  {M : Normalise} -> M.t Type_pair(Int)(M).Opaque.t = <fun>
|}]

let test_normalise_functor_type_opaque =
  normalise_functor_type_opaque {Normalise_test};;

[%%expect{|
val test_normalise_functor_type_opaque :
  Normalise_test.t Type_pair(Int)(Normalise_test).Opaque.t = <abstr>
|}]

let normalise_functor_mty =
  let module Int2 = Int in
  let f {M : Normalise} : (module Type_pair(Int2)(M).S) =
    (module struct type t = M.t * Int2.t end)
  in
  f;;

[%%expect{|
val normalise_functor_mty : {M : Normalise} -> (module Type_pair(Int)(M).S) =
  <fun>
|}]

let test_normalise_functor_mty = normalise_functor_mty {Normalise_test};;

[%%expect{|
val test_normalise_functor_mty : (module Type_pair(Int)(Normalise_test).S) =
  <module>
|}]
