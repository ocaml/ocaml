##ifdef CAMLTK

let tag_bind widget tag eventsequence action =
  check_class widget widget_text_table;
  tkCommand [| 
    cCAMLtoTKwidget widget_text_table widget;
    TkToken "tag";
    TkToken "bind";
    cCAMLtoTKtextTag tag;
    cCAMLtoTKeventSequence eventsequence;
    begin match action with
    | BindRemove -> TkToken ""
    | BindSet (what, f) ->
        let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
    | BindSetBreakable (what, f) ->
        let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what) ^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; \
                   set BreakBindingsSequence 0")
    | BindExtend (what, f) ->
        let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
    end
  |]
;;

##else

let tag_bind ~tag ~events ?(extend = false) ?(breakable = false)
    ?(fields = []) ?action widget =
  tkCommand [| 
    cCAMLtoTKwidget widget;
    TkToken "tag";
    TkToken "bind";
    cCAMLtoTKtextTag tag;
    cCAMLtoTKeventSequence events;
    begin match action with
    | None -> TkToken ""
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
;;

##endif
