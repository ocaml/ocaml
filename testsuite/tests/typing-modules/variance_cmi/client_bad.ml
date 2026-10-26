module G (X : sig type 'a t end) : sig
  type +-'a s
  val inj : 'a Q.q X.t -> 'a s
  val prj : 'a s -> 'a Q.q X.t
end = struct
  type 'a s = 'a Q.q X.t
  let inj x = x
  let prj x = x
end
