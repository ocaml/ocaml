let cCAMLtoTKbitmap : bitmap -> tkArgs = function
  `File s -> TkToken ("@" ^ s)
| `Predefined s -> TkToken s

let cTKtoCAMLbitmap s = 
 if String.get s 0 = '@'
 then `File (String.sub s pos:1 len:(String.length s - 1))
 else `Predefined s


