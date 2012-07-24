module Js = struct
  type +'a t
  type +'a gen_prop
  module Unsafe = struct
    let get (_o : 'a t) (_meth : string) = assert false
    let set (_o : 'a t) (_meth : string) (_v : 'b) = ()
  end
end

let foo1 o =
  if JS.(o.bar) then JS.(o.foo1.foo2) else JS.(o.foo2)

let foo2 o =
  JS.(o.x <- o.x + 1)
