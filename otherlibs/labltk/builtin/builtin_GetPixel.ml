(* Tk_GetPixels emulation *)

##ifdef CAMLTK

(* type *)
type units =
  | Pixels of int       (* specified as floating-point, but inconvenient *)
  | Centimeters of float
  | Inches of float
  | Millimeters of float
  | PrinterPoint of float
;;
(* /type *)

##else

(* type *)
type units = [
  | `Pix of int
  | `Cm of float
  | `In of float
  | `Mm of float
  | `Pt of float
]
;;
(* /type *)

##endif
