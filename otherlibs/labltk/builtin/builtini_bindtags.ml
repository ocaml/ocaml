let cCAMLtoTKbindings = function
  `Widget v1 -> cCAMLtoTKwidget v1
| `Tag v1 -> TkToken v1

(* this doesn't really belong here *)
let cTKtoCAMLbindings s =
  if String.length s > 0 & s.[0] = '.' then
    `Widget (cTKtoCAMLwidget s)
  else `Tag s
