(* Various indexes
    canvas
    entry
    listbox
*)

##ifdef CAMLTK

(* A large type for all indices in all widgets *)
(* a bit overkill though *)

(* type *)
type index =
  | Number of int         (* no keyword  *)
  | ActiveElement         (* tk keyword: active *)
  | End                   (* tk keyword: end *)
  | Last                  (* tk keyword: last *)
  | NoIndex               (* tk keyword: none *)
  | Insert                (* tk keyword: insert *)
  | SelFirst              (* tk keyword: sel.first *)
  | SelLast               (* tk keyword: sel.last *)
  | At of int             (* tk keyword: @n *)
  | AtXY of int * int     (* tk keyword: @x,y *)
  | AnchorPoint           (* tk keyword: anchor *)
  | Pattern of string     (* no keyword *)
  | LineChar of int * int (* tk keyword: l.c *)
  | Mark of string        (* no keyword *)
  | TagFirst of string    (* tk keyword: tag.first *)
  | TagLast of string     (* tk keyword: tag.last *)
  | Embedded of widget    (* no keyword *)
;;
(* /type *)

##else

type canvas_index = [
  | `Num of int
  | `End
  | `Insert
  | `Selfirst
  | `Sellast
  | `Atxy of int * int
]
;;

type entry_index = [
  | `Num of int
  | `End
  | `Insert
  | `Selfirst
  | `Sellast
  | `At of int
  | `Anchor
]
;;

type listbox_index = [
  | `Num of int
  | `Active
  | `Anchor
  | `End
  | `Atxy of int * int
]
;;

type menu_index = [
  | `Num of int
  | `Active
  | `End
  | `Last
  | `None
  | `At of int
  | `Pattern of string
]
;;

type text_index = [
  | `Linechar of int * int
  | `Atxy of int * int
  | `End
  | `Mark of string
  | `Tagfirst of string
  | `Taglast of string
  | `Window of any widget
  | `Image of string
]
;;

type linechar_index = int * int;;
type num_index = int;;

##endif
