##ifdef CAMLTK

let cTKtoCAMLimage s =
  let res = tkEval [|TkToken "image"; TkToken "type"; TkToken s|] in
  match res with
  | "bitmap" -> ImageBitmap (BitmapImage s)
  | "photo" -> ImagePhoto (PhotoImage s)
  | _ -> raise (TkError ("unknown image type \"" ^ res ^ "\""))
;;

let names () = 
  let res = tkEval [|TkToken "image"; TkToken "names"|] in
  let names = splitlist res in
  List.map cTKtoCAMLimage names
;;
  
##else

let cTKtoCAMLimage s =
  let res = tkEval [|TkToken "image"; TkToken "type"; TkToken s|] in
  match res with
  | "bitmap" -> `Bitmap s
  | "photo" -> `Photo s
  | _ -> raise (TkError ("unknown image type \"" ^ res ^ "\""))
;;

let names () = 
  let res = tkEval [|TkToken "image"; TkToken "names"|] in
  let names = splitlist res in
  List.map cTKtoCAMLimage names
;;
  
##endif
