module Core = struct
  module Int = struct
    module T = struct
      type t = int
      let compare = compare
      let (+) x y = x + y
    end
    include T
    module Map = Map.Make(T)
  end

  module Std = struct
    module Int = Int
  end
end

open Core.Std


let x = Int.Map.empty
let y = x + x
