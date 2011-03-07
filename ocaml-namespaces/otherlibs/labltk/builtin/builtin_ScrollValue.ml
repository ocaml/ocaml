##ifdef CAMLTK

(* type *)
type scrollValue =
  | ScrollPage of int           (* tk option: scroll <int> page *)
  | ScrollUnit of int           (* tk option: scroll <int> unit *)
  | MoveTo of float             (* tk option: moveto <float> *)
;;
(* /type *)

##else

(* type *)
type scrollValue = [
  | `Page of int          (* tk option: scroll <int> page *)
  | `Unit of int          (* tk option: scroll <int> unit *)
  | `Moveto of float      (* tk option: moveto <float> *)
]
;;
(* /type *)

##endif
