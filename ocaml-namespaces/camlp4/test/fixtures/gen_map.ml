type t =
  A of int * t * t
| B of int list
| C of option t

module Map = struct
  module T = Camlp4Filters.GenerateMap.Generated
end
