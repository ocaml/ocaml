(* Color *)

##ifdef CAMLTK

(* type *)
type color =
  | NamedColor of string
  | Black                       (* tk keyword: black *)
  | White                       (* tk keyword: white *)
  | Red                 (* tk keyword: red *)
  | Green                       (* tk keyword: green *)
  | Blue                        (* tk keyword: blue *)
  | Yellow                     (* tk keyword: yellow *)
;;
(* /type *)

##else

(* type *)
type color = [
  | `Color of string
  | `Black                      (* tk keyword: black *)
  | `White                      (* tk keyword: white *)
  | `Red                        (* tk keyword: red *)
  | `Green                      (* tk keyword: green *)
  | `Blue                       (* tk keyword: blue *)
  | `Yellow                     (* tk keyword: yellow *)
]
;;
(* /type *)

##endif

##ifdef CAMLTK

(* type *)
type cursor =
  | XCursor of string 
  | XCursorFg of string * color
  | XCursortFgBg of string * color * color
  | CursorFileFg of string * color 
  | CursorMaskFile of string * string * color * color
;;
(* /type *)

##else

(* Tk_GetCursor emulation *)
(* type *)
type cursor = [
  | `Xcursor of string
  | `Xcursorfg of string * color
  | `Xcursorfgbg of string * color * color
  | `Cursorfilefg of string * color
  | `Cursormaskfile of string * string * color * color
]
;;
(* /type *)

##endif

