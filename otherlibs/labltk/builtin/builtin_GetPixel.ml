(* Tk_GetPixels emulation *)
(* type *)
type units = [
  | `Pix of int
  | `Cm of float
  | `In of float
  | `Mm of float
  | `Pt of float
]
(* /type *)

