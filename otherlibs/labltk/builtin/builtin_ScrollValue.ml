(* type *)
type scrollValue = [
  | `Page of int          (* tk option: scroll <int> page *)
  | `Unit of int          (* tk option: scroll <int> unit *)
  | `Moveto of float      (* tk option: moveto <float> *)
]
(* /type *)

