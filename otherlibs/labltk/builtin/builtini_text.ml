let cCAMLtoTKtextMark  x =  TkToken x
let cTKtoCAMLtextMark x = x

let cCAMLtoTKtextTag  x =  TkToken x
let cTKtoCAMLtextTag x = x

(* TextModifiers are never returned by Tk *)
let ppTextModifier = function
   `Char n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "chars"
      else if n = 0 then ""
      else (string_of_int n) ^ "chars"
 | `Line n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "lines"
      else if n = 0 then ""
      else (string_of_int n) ^ "lines"
 | `Linestart -> " linestart"
 | `Lineend -> " lineend"
 | `Wordstart -> " wordstart"
 | `Wordend -> " wordend"

(*
let ppTextIndex = function
   `None -> ""
 | `Index (base, ml) -> 
     let (TkToken ppbase) = cCAMLtoTKtext_index base in 
       catenate_sep "" (ppbase :: List.map fun:ppTextModifier ml)
*)

let ppTextIndex = function
  (base, ml) -> 
     let (TkToken ppbase) = cCAMLtoTKtext_index base in 
       catenate_sep "" (ppbase :: List.map fun:ppTextModifier ml)

let cCAMLtoTKtextIndex : textIndex -> tkArgs = function i -> 
  TkToken (ppTextIndex i)

