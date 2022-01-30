(* TEST
   modules = "globrootsprim.c"
*)

module type GLOBREF = sig
  type t
  val register: string -> t
  val get: t -> string
  val set: t -> string -> unit
  val remove: t -> unit
end

module Classic : GLOBREF = struct
  type t
  external register: string -> t = "gb_classic_register"
  external get: t -> string = "gb_get"
  external set: t -> string -> unit = "gb_classic_set"
  external remove: t -> unit = "gb_classic_remove"
end

module Generational : GLOBREF = struct
  type t
  external register: string -> t = "gb_generational_register"
  external get: t -> string = "gb_get"
  external set: t -> string -> unit = "gb_generational_set"
  external remove: t -> unit = "gb_generational_remove"
end

module Test(G: GLOBREF) () = struct

  let size = 1024

  let random_state =
    Domain.DLS.new_key
      ~split_from_parent:Random.State.split
      Random.State.make_self_init

  let vals = Array.init size Int.to_string

  let a = Array.init size (fun i -> G.register (Int.to_string i))

  let check () =
    for i = 0 to size - 1 do
      if G.get a.(i) <> vals.(i) then begin
        print_string "Error on "; print_int i; print_string ": ";
        print_string (String.escaped (G.get a.(i))); print_newline()
      end
    done

  let change () =
    match Random.State.int (Domain.DLS.get random_state) 37 with
    | 0 ->
        Gc.full_major()
    | 1|2|3|4 ->
        Gc.minor()
    | 5|6|7|8|9|10|11|12 ->             (* update with young value *)
        let i = Random.State.int (Domain.DLS.get random_state) size in
        G.set a.(i) (Int.to_string i)
    | 13|14|15|16|17|18|19|20 ->        (* update with old value *)
        let i = Random.State.int (Domain.DLS.get random_state) size in
        G.set a.(i) vals.(i)
    | 21|22|23|24|25|26|27|28 ->        (* re-register young value *)
        let i = Random.State.int (Domain.DLS.get random_state) size in
        G.remove a.(i);
        a.(i) <- G.register (Int.to_string i)
    | (*29|30|31|32|33|34|35|36*) _ ->  (* re-register old value *)
        let i = Random.State.int (Domain.DLS.get random_state) size in
        G.remove a.(i);
        a.(i) <- G.register vals.(i)

  let test n =
    for i = 1 to n do
      change(); check();
    done
end

module TestClassic = Test(Classic) ()
module TestGenerational = Test(Generational) ()

external young2old : unit -> unit = "gb_young2old"

external static2young : int * int -> (unit -> unit) -> int = "gb_static2young"
