##ifdef CAMLTK

let cCAMLtoTKcolor = function
        NamedColor x -> TkToken x
        | Black -> TkToken "black"
        | White -> TkToken "white"
        | Red -> TkToken "red"
        | Green -> TkToken "green"
        | Blue -> TkToken "blue"
        | Yellow -> TkToken "yellow"
;;

let cTKtoCAMLcolor = function  s -> NamedColor s
;;

let cCAMLtoTKcursor = function
   XCursor s -> TkToken s
 | XCursorFg (s,fg) -> 
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg])
 | XCursortFgBg (s,fg,bg) ->
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
 | CursorFileFg (s,fg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); cCAMLtoTKcolor fg])
 | CursorMaskFile (s,m,fg,bg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); TkToken m; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
;;

##else

let cCAMLtoTKcolor : color -> tkArgs = function
  | `Color x -> TkToken x
  | `Black -> TkToken "black"
  | `White -> TkToken "white"
  | `Red -> TkToken "red"
  | `Green -> TkToken "green"
  | `Blue -> TkToken "blue"
  | `Yellow -> TkToken "yellow"
;;

let cTKtoCAMLcolor = function  s -> `Color s
;;

let cCAMLtoTKcursor : cursor -> tkArgs = function
 | `Xcursor s -> TkToken s
 | `Xcursorfg (s,fg) -> 
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg])
 | `Xcursorfgbg (s,fg,bg) ->
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
 | `Cursorfilefg (s,fg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); cCAMLtoTKcolor fg])
 | `Cursormaskfile (s,m,fg,bg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); TkToken m; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
;;

##endif
