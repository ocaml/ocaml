let cCAMLtoTKtextMark  x =  TkToken x;;
let cTKtoCAMLtextMark x = x;;

let cCAMLtoTKtextTag  x =  TkToken x;;
let cTKtoCAMLtextTag x = x;;

##ifdef CAMLTK

(* TextModifiers are never returned by Tk *)
let ppTextModifier = function
   CharOffset n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "chars"
      else if n = 0 then ""
      else (string_of_int n) ^ "chars"
 | LineOffset n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "lines"
      else if n = 0 then ""
      else (string_of_int n) ^ "lines"
 | LineStart -> " linestart"
 | LineEnd -> " lineend"
 | WordStart -> " wordstart"
 | WordEnd -> " wordend"
;;

let ppTextIndex = function
 | TextIndexNone -> ""
 | TextIndex (base, ml) -> 
     match cCAMLtoTKindex index_text_table base with
     | TkToken ppbase -> List.fold_left (^) ppbase (List.map ppTextModifier ml)
     | _ -> assert false
;;

let cCAMLtoTKtextIndex i = 
  TkToken (ppTextIndex i)
;;

##else

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
        String.concat ~sep:"" (ppbase :: List.map ~f:ppTextModifier ml)
    | _ -> assert false
  in
  TkToken (ppTextIndex i)
;;

##endif
