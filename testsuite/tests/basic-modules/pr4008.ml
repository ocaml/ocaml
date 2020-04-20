module rec M : sig
  val f : int list -> int list
end = struct
  let f = List.map succ
end
let v = M.f []
