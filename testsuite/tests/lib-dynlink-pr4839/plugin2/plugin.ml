let rec fact = function
  | 0 -> 1
  | n -> n * fact (n - 1)

let _ =
  Packed.Api.zero := 0;
  Packed.Api.fact := Some fact
