(* Tk_GetBitmap emulation *)
(* type *)
type bitmap = [
  | `File of string                 (* path of file *)
  | `Predefined of string           (* bitmap  name *)
]
(* /type *)

