module M : sig
  val f : [< `Foo of int & string] -> unit
end
