##ifdef CAMLTK

let cCAMLtoTKbindings = function
  | WidgetBindings v1 -> cCAMLtoTKwidget widget_any_table v1
  | TagBindings v1 -> TkToken v1
;;

(* this doesn't really belong here *)
let cTKtoCAMLbindings s =
  if String.length s > 0 && s.[0] = '.' then
    WidgetBindings (cTKtoCAMLwidget s)
  else TagBindings s
;;

##else

let cCAMLtoTKbindings = function
| `Widget v1 -> cCAMLtoTKwidget v1
| `Tag v1 -> TkToken v1
;;

(* this doesn't really belong here *)
let cTKtoCAMLbindings s =
  if String.length s > 0 && s.[0] = '.' then
    `Widget (cTKtoCAMLwidget s)
  else `Tag s
;;

##endif
