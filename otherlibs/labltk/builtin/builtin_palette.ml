##ifdef CAMLTK

(* type *)
type paletteType =
  | GrayShades of int
  | RGBShades of int * int * int
;;
(* /type *)

##else

(* type *)
type paletteType = [
  | `Gray of int
  | `Rgb of int * int * int
]
;;
(* /type *)

##endif
