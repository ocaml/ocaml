(* TEST
*)

(* PR#9494 *)

(* Additional test cases from Vincent Laviron: *)

(* Looping version *)
module rec M1 : sig
  val f : unit -> unit
  val g : unit -> unit
end = struct
  let f = M1.g
  let g () = M1.f ()
end

(* Alias chain *)
module rec M2 : sig
  val f : unit -> unit
  val g : unit -> unit
end = struct
  let f = M2.g
  let g = M2.f
end

(* Original test case from the issue: *)

module rec Id : sig
  type t = {id : int}
  val compare : t -> t -> int
end = Id (* error here: undefined compare function *)

module IdSet = Set.Make(Id)

let _ = try
  let basic_set = IdSet.singleton {id = 0} in
  IdSet.mem {id = 1} basic_set (* diverge here *)
with e -> print_endline @@ Printexc.to_string e; false
