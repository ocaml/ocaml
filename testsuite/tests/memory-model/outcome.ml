(*********************************************)
(* Outcomes, _i.e._ final values of test run *)
(*********************************************)

module type T = sig
  type t
  val compare : t -> t -> int
  val pp : t -> string
end

module Int = struct
  type t = int
  let compare = Int.compare
  let pp = Printf.sprintf "%d"
end

module type Allow = sig val allowed : bool end

module
  Make(T0:T)(T1:T)
    (N:
       sig
         val name : string
         val tag0 : string
         val tag1 : string
         val ok : T0.t -> T1.t -> bool
       end)
    (A:Allow) =
  struct

    type t = { r0 : T0.t ; r1 : T1.t; }

    let compare k1 k2 = match T0.compare k1.r0 k2.r0 with
      | 0 -> T1.compare k1.r1 k2.r1
      | r -> r

    let ok k = N.ok k.r0 k.r1

    let allowed = A.allowed

    let name = N.name

    let make r0 r1 = { r0; r1; }

    let pp chan k =
      Printf.fprintf chan "%s=%s; %s=%s;"
        N.tag0 (T0.pp k.r0) N.tag1 (T1.pp k.r1)

  end

module OK = struct let allowed = true end
module NO = struct let allowed = false  end
