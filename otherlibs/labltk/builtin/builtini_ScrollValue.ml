##ifdef CAMLTK

let cCAMLtoTKscrollValue = function
   ScrollPage v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"pages"]
 | ScrollUnit v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"units"]
 | MoveTo v1 ->
    TkTokenList [TkToken"moveto"; TkToken (Printf.sprintf "%g" v1)]
;;

(* str l -> scrllv -> str l *)
let cTKtoCAMLscrollValue = function
   "scroll"::n::"pages"::l -> 
     ScrollPage (int_of_string n), l
 | "scroll"::n::"units"::l ->
     ScrollUnit (int_of_string n), l
 | "moveto"::f::l ->
     MoveTo (float_of_string f), l
 | _ -> raise (Invalid_argument "TKtoCAMLscrollValue")
;;

##else

let cCAMLtoTKscrollValue : scrollValue -> tkArgs = function
 | `Page v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"pages"]
 | `Unit v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"units"]
 | `Moveto v1 ->
    TkTokenList [TkToken"moveto"; TkToken (Printf.sprintf "%g" v1)]
;;

(* str l -> scrllv -> str l *)
let cTKtoCAMLscrollValue = function
 | "scroll" :: n :: "pages" :: l -> 
     `Page (int_of_string n), l
 | "scroll" :: n :: "units" :: l ->
     `Unit (int_of_string n), l
 | "moveto" :: f :: l ->
     `Moveto (float_of_string f), l
 | _ -> raise (Invalid_argument "TKtoCAMLscrollValue")
;;

##endif
