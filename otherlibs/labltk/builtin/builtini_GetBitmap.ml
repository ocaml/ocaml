##ifdef CAMLTK

let cCAMLtoTKbitmap = function
  BitmapFile s -> TkToken ("@" ^ s)
| Predefined s -> TkToken s
;;

let cTKtoCAMLbitmap s = 
 if s = "" then Predefined ""
 else if String.get s 0 = '@'
 then BitmapFile (String.sub s 1 (String.length s - 1))
 else Predefined s
;;

##else

let cCAMLtoTKbitmap : bitmap -> tkArgs = function
  | `File s -> TkToken ("@" ^ s)
  | `Predefined s -> TkToken s
;;

let cTKtoCAMLbitmap s = 
 if String.get s 0 = '@'
 then `File (String.sub s ~pos:1 ~len:(String.length s - 1))
 else `Predefined s
;;

##endif
