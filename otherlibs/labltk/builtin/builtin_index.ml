(* Various indexes
    canvas
    entry
    listbox
*)

type canvas_index = [
  | `Num of int
  | `End
  | `Insert
  | `Selfirst
  | `Sellast
  | `Atxy of int * int
]

type entry_index = [
  | `Num of int
  | `End
  | `Insert
  | `Selfirst
  | `Sellast
  | `At of int
  | `Anchor
]

type listbox_index = [
  | `Num of int
  | `Active
  | `Anchor
  | `End
  | `Atxy of int * int
]

type menu_index = [
  | `Num of int
  | `Active
  | `End
  | `Last
  | `None
  | `At of int
  | `Pattern of string
]

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

type linechar_index = int * int
type num_index = int
