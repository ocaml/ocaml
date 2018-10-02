(* TEST
*)

module Float = struct
  type _ t =
    | IO : int option t
    | F : float t

  let bar : type a. a t -> float -> int -> a =
    fun t f i ->
      match t with
      | IO -> Some i
      | F -> f
  [@@inline always]

  let foo (t : float t) f i =
    let r = ref 0. in
    r := bar t f i
end

(* These boxed integer cases were also fixed by GPR#2083, although
   current compiler code would not actually cause a failure even
   before that fix.  The tests here are given in case register
   typing is tightened up in future (e.g. GPR#1192). *)

module Int32 = struct
  type _ t =
    | IO : int option t
    | F : int32 t

  let bar : type a. a t -> int32 -> int -> a =
    fun t f i ->
      match t with
      | IO -> Some i
      | F -> f
  [@@inline always]

  let foo (t : int32 t) f i =
    let r = ref 0l in
    r := bar t f i
end

module Int64 = struct
  type _ t =
    | IO : int option t
    | F : int64 t

  let bar : type a. a t -> int64 -> int -> a =
    fun t f i ->
      match t with
      | IO -> Some i
      | F -> f
  [@@inline always]

  let foo (t : int64 t) f i =
    let r = ref 0L in
    r := bar t f i
end

module Nativeint = struct
  type _ t =
    | IO : int option t
    | F : nativeint t

  let bar : type a. a t -> nativeint -> int -> a =
    fun t f i ->
      match t with
      | IO -> Some i
      | F -> f
  [@@inline always]

  let foo (t : nativeint t) f i =
    let r = ref 0n in
    r := bar t f i
end
