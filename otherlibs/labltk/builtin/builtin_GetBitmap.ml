(* Tk_GetBitmap emulation *)

##ifdef CAMLTK

(* type *)
type bitmap =
  | BitmapFile of string                 (* path of file *)
  | Predefined of string                 (* bitmap  name *)
;;
(* /type *)

##else

(* type *)
type bitmap = [
  | `File of string                 (* path of file *)
  | `Predefined of string           (* bitmap  name *)
]
;;
(* /type *)

##endif
