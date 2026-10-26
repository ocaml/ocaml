module G (X : sig type 'a t end) : sig
  type +-'a s
  val inj : 'a Phantom.t X.t -> 'a s
  val prj : 'a s -> 'a Phantom.t X.t
end = struct
  type 'a s = 'a Phantom.t X.t
  let inj x = x
  let prj x = x
end
