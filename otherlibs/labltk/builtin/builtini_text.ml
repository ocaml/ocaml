let cCAMLtoTKtextMark  x =  TkToken x
let cTKtoCAMLtextMark x = x

let cCAMLtoTKtextTag  x =  TkToken x
let cTKtoCAMLtextTag x = x

(* TextModifiers are never returned by Tk *)
let cCAMLtoTKtextIndex (i : textIndex) =
  let ppTextModifier = function
    | `Char n -> 
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
  in
  let ppTextIndex (base, ml : textIndex) =
    match cCAMLtoTKtext_index base with
      TkToken ppbase ->
        String.concat sep:"" (ppbase :: List.map fun:ppTextModifier ml)
    | _ -> assert false
  in
  TkToken (ppTextIndex i)

