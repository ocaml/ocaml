module Js = struct
  type +'a t
  type +'a gen_prop
  type +'a meth
  module Unsafe = struct
    type any
    let get (_o : 'a t) (_meth : string) = assert false
    let set (_o : 'a t) (_meth : string) (_v : 'b) = ()
    let meth_call (_ : 'a) (_ : string) (_ : any array) : 'b = assert false
    let inject _ : any = assert false
  end
end

let foo1 o =
  if JVS.(o.bar) then JVS.(o.foo1.foo2) else JVS.(o.foo2)

let foo2 o =
  JVS.(o.x <- o.x + 1)


let foo3 o a =
  JVS.(o#x) + JVS.(o#y 1 a)
