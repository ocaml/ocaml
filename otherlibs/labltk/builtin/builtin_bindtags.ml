##ifdef CAMLTK

(* type *)
type bindings =
  | TagBindings of string       (* tk option: <string> *)
  | WidgetBindings of widget    (* tk option: <widget> *)
;;
(* /type *)

##else

(* type *)
type bindings = [
  | `Tag of string                (* tk option: <string> *)
  | `Widget of any widget         (* tk option: <widget> *)
]
;;
(* /type *)

##endif

