(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module type Typ = sig type t end

module type Add = sig type t val add : t -> t -> t end


module type Add2 = sig
  type a
  type t
  val add : t -> t -> t
end

module type Add3 = sig
  type t
  type a
  val add : t -> t -> t
end

module type Add4 = sig
  type t
  val add : t -> t -> t
  type a
end

module type TypPrivInt = sig
  type t = private int
end

let id (module T : Typ) (x : T.t) = x

let id2 : (module T : Typ) -> T.t -> T.t =
  fun (module A : Typ) (x : A.t) -> x

let id_infer_sig  : (module T : Typ) -> T.t -> T.t =
  fun (module A) (x : A.t) -> x

let f x (module M : Typ) (y : M.t) = (x, y)

(* Here we cannot extract the type of m *)
let invalid_arg3 =
  let m = (module Int : Typ) in
  f 3 m 4

(* Here we cannot extract the type of m. This could be accepted because m does
   not hide any abstract types. *)
let invalid_arg4 =
  let m = (module Int : Typ with type t = int) in
  f 3 m 4

let foo f a =
  let _ = (f ~a : (module M : Typ) -> M.t) in
  f ~a (fun x -> x)

let c : string = 10

let labelled () (module M : Typ) ~(y:M.t) = y

let apply_labelled = labelled () ~y:3 (module Int)

let apply_labelled_fail = labelled () ~y:3

let foo2 f a =
  let m = (module Int : Typ) in
  let _ = (f ~a : (module M : Typ) -> M.t) in
  f ~a m

let apply_labelled_success = labelled' ~y:3

let foo4 f a =
  let _ = (f ~a : b:(module M : Typ) -> M.t) in
  f ~a ~c:(module Int)

let x_from_struct = id (module struct type t = int end) 3

let x_from_generative_functor = id (module F ())

module type Map = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

let map (module M : Map) f x = M.map f x

module MapCombine (M1 : Map) (M2 : Map) = struct
  type 'a t = 'a M1.t M2.t
  let map f = map (module M2) (map (module M1) f)
end

let s_list_array = map (module MapCombine(List)(Array))
    string_of_int [|[3; 2]; [2]; []|]

let s_list_arrayb =
    map
      (module MapCombine(struct
          type 'a t = A of 'a
          let map f (A x) = (A (f x))
        end)(Array))
      string_of_int [|[]|]

let fail = map (module F()) string_of_int [3]


(* But we can add type fields with ground coercion *)
let coerce5 (f : (module A : Add) -> A.t -> A.t) =
  (f :> (module A : Add2) -> A.t -> A.t)

(* changing type order in signature *)
let try_coerce6 (f : (module A : Add2) -> A.t -> A.t) =
  (f : (module A : Add3) -> A.t -> A.t)

let try_coerce7 (f : (module A : Add2) -> A.t -> A.t) =
  (f : (module A : Add4) -> A.t -> A.t)

let try_coerce8 (f : (module A : Add2) -> A.t -> A.t) =
  (f :> (module A : Add) -> A.t -> A.t)

let restrict_signature_with_priv_to_remove_dep =
  fun (x : (module T : Typ) -> unit -> T.t) ->
    (x :> (module TypPrivInt) -> unit -> int)

let restrict_signature_with_priv_to_add_dep =
  fun (x : (module Typ) -> int -> int) ->
  (x :> (module T : TypPrivInt) -> T.t -> int)

let last_error : string = 10
let r = last_error ^ "foo"
