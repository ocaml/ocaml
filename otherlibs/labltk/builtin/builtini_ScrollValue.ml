let cCAMLtoTKscrollValue : scrollValue -> tkArgs = function
   `Page v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"pages"]
 | `Unit v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"units"]
 | `Moveto v1 ->
    TkTokenList [TkToken"moveto"; TkToken (string_of_float v1)]

(* str l -> scrllv -> str l *)
let cTKtoCAMLscrollValue = function
   "scroll"::n::"pages"::l -> 
     `Page (int_of_string n), l
 | "scroll"::n::"units"::l ->
     `Unit (int_of_string n), l
 | "moveto"::f::l ->
     `Moveto (float_of_string f), l
 | _ -> raise (Invalid_argument "TKtoCAMLscrollValue")
