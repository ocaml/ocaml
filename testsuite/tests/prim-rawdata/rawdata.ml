(* TEST
   modules = "external.c"
*)

module ExtInt : sig

  type t

  val create : int -> t
  val read : t -> int
  val write : t -> int -> unit

end = struct

  type t = int

  external create : int -> int = "external_int_ref"

  let read t =
    let d = Obj.int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    Obj.raw_data_load_int d

  let write t i =
    let d = Obj.int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    Obj.raw_data_set_int d i

end

module ExtFloat : sig

  type t

  val create : float -> t
  val read : t -> float
  val write : t -> float -> unit

end = struct

  type t = int

  external create : float -> int = "external_float_ref"

  let read t =
    let d = Obj.int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    Obj.raw_data_load_float d

  let write t f =
    let d = Obj.int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    Obj.raw_data_set_float d f

end

let main () =
  let ir = ExtInt.create 17 in
  let fr = ExtFloat.create 42.0 in
  Printf.printf "Int: %i Float: %f\n%!"
    (ExtInt.read ir) (ExtFloat.read fr);
  ExtInt.write ir 9;
  ExtFloat.write fr 9.9;
  Printf.printf "Int: %i Float: %f\n%!"
    (ExtInt.read ir) (ExtFloat.read fr)

let () = main ()
