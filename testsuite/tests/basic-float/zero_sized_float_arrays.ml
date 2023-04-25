(* TEST *)

let non_float_array : int array = [| |]

let float_array : float array = [| |]

let non_float_array_from_runtime : int array =
  Array.make 0 0

let float_array_from_runtime : float array =
  Array.make 0 0.0

let () =
  assert (Stdlib.compare non_float_array non_float_array_from_runtime = 0);
  assert (Stdlib.compare non_float_array non_float_array_from_runtime = 0);
  assert (Stdlib.compare float_array float_array_from_runtime = 0);
  assert (Stdlib.compare float_array float_array_from_runtime = 0)
