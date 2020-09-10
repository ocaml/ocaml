(* TEST
   modules = "external.c"
*)

external external_int_ref : int -> int = "external_int_ref"
external external_float_ref : float -> int = "external_float_ref"
external int_as_raw_data : int -> Obj.raw_data = "%int_as_rawdata"
external raw_data_load_int : Obj.raw_data -> int = "%rawdata_load_int"
external raw_data_load_float : Obj.raw_data -> float = "%rawdata_load_float"
external raw_data_set_int : Obj.raw_data -> int -> unit = "%rawdata_set_int"
external raw_data_set_float : Obj.raw_data -> float -> unit  = "%rawdata_set_float"

module ExtInt : sig

  type t

  val create : int -> t
  val read : t -> int
  val write : t -> int -> unit

end = struct

  type t = int

  let create = external_int_ref

  let read t =
    let d = int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    raw_data_load_int d

  let write t i =
    let d = int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    raw_data_set_int d i

end

module ExtFloat : sig

  type t

  val create : float -> t
  val read : t -> float
  val write : t -> float -> unit

end = struct

  type t = int

  let create = external_float_ref

  let read t =
    let d = int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    raw_data_load_float d

  let write t f =
    let d = int_as_raw_data t in
    let d = Nativeint.sub d 1n in
    raw_data_set_float d f

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
