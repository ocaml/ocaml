let cCAMLtoTKindex (* Don't put explicit typing *) = function
 | `Num x -> TkToken (string_of_int x)
 | `Active -> TkToken "active"
 | `End -> TkToken "end"
 | `Last -> TkToken "last"
 | `None -> TkToken "none"
 | `Insert -> TkToken "insert"
 | `Selfirst -> TkToken "sel.first"
 | `Sellast -> TkToken "sel.last"
 | `At n -> TkToken ("@" ^ string_of_int n)
 | `Atxy (x,y) -> TkToken ("@" ^ string_of_int x ^ "," ^ string_of_int y)
 | `Anchor -> TkToken "anchor"
 | `Pattern s -> TkToken s
 | `Linechar (l,c) -> TkToken (string_of_int l ^ "." ^ string_of_int c)
 | `Mark s -> TkToken s
 | `Tagfirst t -> TkToken (t ^ ".first")
 | `Taglast t -> TkToken (t ^ ".last")
 | `Window (w : any widget) -> cCAMLtoTKwidget w
 | `Image s -> TkToken s

let cCAMLtoTKcanvas_index = (cCAMLtoTKindex : canvas_index -> tkArgs)
let cCAMLtoTKentry_index = (cCAMLtoTKindex : entry_index -> tkArgs)
let cCAMLtoTKlistbox_index = (cCAMLtoTKindex : listbox_index -> tkArgs)
let cCAMLtoTKmenu_index = (cCAMLtoTKindex : menu_index -> tkArgs)
let cCAMLtoTKtext_index = (cCAMLtoTKindex : text_index -> tkArgs)

(* Assume returned values are only numerical and l.c *)

let cTKtoCAMLtext_index s = 
  try
   let p = String.index s '.' in
    `Linechar (int_of_string (String.sub s pos:0 len:p), 
             int_of_string (String.sub s pos:(p + 1) 
                                         len:(String.length s - p - 1)))
  with
    Not_found ->
      raise (Invalid_argument ("TKtoCAMLtext_index: " ^ s))


let cTKtoCAMLlistbox_index s =
  try `Num (int_of_string s)
  with _ -> raise (Invalid_argument ("TKtoCAMLlistbox_index: " ^ s))
