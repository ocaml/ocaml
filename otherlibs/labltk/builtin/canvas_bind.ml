let bind ~canvas:widget ~events
    ?(extend = false) ?(breakable = false) ?(fields = [])
    ?action tag =
  tkCommand
    [| cCAMLtoTKwidget widget;
       TkToken "bind";
       cCAMLtoTKtagOrId tag;
       cCAMLtoTKeventSequence events;
       begin match action with None -> TkToken ""
       | Some f ->
           let cbId =
             register_callback widget ~callback: (wrapeventInfo f fields) in
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
