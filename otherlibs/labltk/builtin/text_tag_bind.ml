let tag_bind :tag :events ?(:extend = false) ?(:breakable = false)
    ?(:fields = []) ?:action widget =
  ignore begin
    tkEval
      [| cCAMLtoTKwidget widget;
         TkToken "tag";
         TkToken "bind";
         cCAMLtoTKtextTag tag;
         cCAMLtoTKeventSequence events;
         begin match action with None -> TkToken ""
         | Some f ->
             let cbId =
               register_callback widget callback: (wrapeventInfo f fields) in
             let cb = if extend then "+camlcb " else "camlcb " in
             let cb = cb ^ cbId ^ writeeventField fields in
             let cb =
               if breakable then 
                 cb ^ " ; if { $BreakBindingsSequence == 1 } then { break ;}"
                 ^ " ; set BreakBindingsSequence 0"
               else cb in
             TkToken cb
         end
       |]
  end
