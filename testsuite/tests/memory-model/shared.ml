(*****************)
(* Shared memory *)
(*****************)

module type T = sig
  type t
  val empty : 'a -> t
  val reinit :  t -> unit
end

module type S = sig
  type t
  val make : int -> t
  val reinit : t -> unit
  type in_t
  val env2in : t -> int -> in_t
end

module One(T0:T) = struct

  type t = T0.t array

  let make sz =  Array.init sz T0.empty

  let reinit env = Array.iter T0.reinit env

  type in_t = T0.t

  let env2in env i = env.(i)

end

module Make(T0:T)(T1:T) = struct

  type t =
    {
      x :T0.t array ;
      y :T1.t array ;
      sz : int ;
    }

  let make sz =
    {
      x = Array.init sz T0.empty ;
      y = Array.init sz T1.empty ;
      sz;
    }

  let reinit env =
    let x = env.x and y = env.y in
    for i = 0 to env.sz-1 do
      T0.reinit x.(i) ; T1.reinit y.(i)
    done

  type in_t = T0.t * T1.t
  let env2in env i = env.x.(i),env.y.(i)

end
