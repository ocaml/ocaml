(* Not a string as such, more like a symbol *)

(* type *)
type textMark = string
(* /type *)

(* type *)
type textTag = string
(* /type *)

(* type *)
type textModifier = [
  | `Char of int                (* tk keyword: +/- Xchars *)
  | `Line of int                (* tk keyword: +/- Xlines *)
  | `Linestart                  (* tk keyword: linestart *)
  | `Lineend                    (* tk keyword: lineend *)
  | `Wordstart                  (* tk keyword: wordstart *)
  | `Wordend                    (* tk keyword: wordend *)
]
(* /type *)

(* type *)
type textIndex = text_index * textModifier list
(* /type *)
