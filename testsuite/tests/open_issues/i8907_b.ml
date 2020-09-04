module M = struct
  type t = int
  let f (x : [< `Foo of t & int & string]) = ()
end
