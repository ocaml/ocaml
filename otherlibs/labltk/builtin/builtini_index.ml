##ifdef CAMLTK

(* sp to avoid being picked up by doc scripts *)
 type index_constrs =
          CNumber
        | CActiveElement
        | CEnd
        | CLast
        | CNoIndex
        | CInsert
        | CSelFirst
        | CSelLast
        | CAt
        | CAtXY
        | CAnchorPoint
        | CPattern
        | CLineChar
        | CMark
        | CTagFirst
        | CTagLast
        | CEmbedded
;;

let index_any_table = 
 [CNumber; CActiveElement; CEnd; CLast; CNoIndex; CInsert; CSelFirst;
  CSelLast; CAt; CAtXY; CAnchorPoint; CPattern; CLineChar;
  CMark; CTagFirst; CTagLast; CEmbedded]
;;

let index_canvas_table =
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAtXY]
;;
let index_entry_table = 
  [CNumber; CAnchorPoint; CEnd; CInsert; CSelFirst; CSelLast; CAt]
;;
let index_listbox_table = 
  [CNumber; CActiveElement; CAnchorPoint; CEnd; CAtXY]
;;
let index_menu_table =
  [CNumber; CActiveElement; CEnd; CLast; CNoIndex; CAt; CPattern]
;;
let index_text_table =
  [CLineChar; CAtXY; CEnd; CMark; CTagFirst; CTagLast; CEmbedded]
;;

let cCAMLtoTKindex table = function
   Number x -> chk_sub "Number" table CNumber; TkToken (string_of_int x)
 | ActiveElement -> chk_sub "ActiveElement" table CActiveElement; TkToken "active"
 | End -> chk_sub "End" table CEnd; TkToken "end"
 | Last -> chk_sub "Last" table CLast; TkToken "last"
 | NoIndex -> chk_sub "NoIndex" table CNoIndex; TkToken "none"
 | Insert -> chk_sub "Insert" table CInsert; TkToken "insert"
 | SelFirst -> chk_sub "SelFirst" table CSelFirst; TkToken "sel.first"
 | SelLast -> chk_sub "SelLast" table CSelLast; TkToken "sel.last"
 | At n -> chk_sub "At" table CAt; TkToken ("@"^string_of_int n)
 | AtXY (x,y) -> chk_sub "AtXY" table CAtXY; 
             TkToken ("@"^string_of_int x^","^string_of_int y)
 | AnchorPoint -> chk_sub "AnchorPoint" table CAnchorPoint; TkToken "anchor"
 | Pattern s -> chk_sub "Pattern" table CPattern; TkToken s
 | LineChar (l,c) -> chk_sub "LineChar" table CLineChar;
          TkToken (string_of_int l^"."^string_of_int c)
 | Mark s -> chk_sub "Mark" table CMark; TkToken s
 | TagFirst t -> chk_sub "TagFirst" table CTagFirst; 
           TkToken (t^".first")
 | TagLast t -> chk_sub "TagLast" table CTagLast;
           TkToken (t^".last")
 | Embedded w -> chk_sub "Embedded" table CEmbedded;
           cCAMLtoTKwidget widget_any_table w
;;

let char_index c s =
  let rec find i =
    if i >= String.length s 
    then raise Not_found
    else if String.get s i = c then i 
    else find (i+1) in
  find 0
;;

(* Assume returned values are only numerical and l.c *)
(* .menu index returns none if arg is none, but blast it *)
let cTKtoCAMLindex s =
  try
   let p = char_index '.' s in
    LineChar(int_of_string (String.sub s 0 p), 
             int_of_string (String.sub s (p+1) (String.length s - p - 1)))
  with
    Not_found ->
      try Number (int_of_string s)
      with _ -> raise (Invalid_argument ("TKtoCAMLindex: "^s))
;;

##else

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
;;

let cCAMLtoTKcanvas_index = (cCAMLtoTKindex : canvas_index -> tkArgs);;
let cCAMLtoTKentry_index = (cCAMLtoTKindex : entry_index -> tkArgs);;
let cCAMLtoTKlistbox_index = (cCAMLtoTKindex : listbox_index -> tkArgs);;
let cCAMLtoTKmenu_index = (cCAMLtoTKindex : menu_index -> tkArgs);;
let cCAMLtoTKtext_index = (cCAMLtoTKindex : text_index -> tkArgs);;

(* Assume returned values are only numerical and l.c *)

let cTKtoCAMLtext_index s = 
  try
   let p = String.index s '.' in
    `Linechar (int_of_string (String.sub s ~pos:0 ~len:p), 
             int_of_string (String.sub s ~pos:(p + 1) 
                                         ~len:(String.length s - p - 1)))
  with
    Not_found ->
      raise (Invalid_argument ("TKtoCAMLtext_index: " ^ s))
;;

let cTKtoCAMLlistbox_index s =
  try `Num (int_of_string s)
  with _ -> raise (Invalid_argument ("TKtoCAMLlistbox_index: " ^ s))
;;

##endif
