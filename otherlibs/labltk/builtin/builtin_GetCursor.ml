(* Color *)
(* type *)
type color = [
    `Color string
  | `Black			(* tk keyword: black *)
  | `White			(* tk keyword: white *)
  | `Red			(* tk keyword: red *)
  | `Green			(* tk keyword: green *)
  | `Blue			(* tk keyword: blue *)
  | `Yellow                     (* tk keyword: yellow *)
]
(* /type *)

(* Tk_GetCursor emulation *)
(* type *)
type cursor = [
    `Xcursor string
  | `Xcursorfg string * color
  | `Xcursorfgbg string * color * color
  | `Cursorfilefg string * color
  | `Cursormaskfile string * string * color * color
]
(* /type *)

