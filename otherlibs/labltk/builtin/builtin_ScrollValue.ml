(* type *)
type scrollValue = [
    `Page(int)		(* tk option: scroll <int> page *)
  | `Unit(int)		(* tk option: scroll <int> unit *)
  | `Moveto(float)	(* tk option: moveto <float> *)
]
(* /type *)

