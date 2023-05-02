(* TEST *)

let rec empty_int_array : int array =
  let _ = empty_int_array in [||]

let rec empty_float_array : float array =
  let _ = empty_float_array in [||]

module type Empty = sig end
let rec empty_mod : (module Empty) =
  let _ = empty_mod in (module struct end)
