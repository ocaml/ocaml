let bind widget :tag events:eventsequence :action =
  tkEval [| cCAMLtoTKwidget widget;
      	    TkToken "bind";
      	    cCAMLtoTKtagOrId tag;
       	    cCAMLtoTKeventSequence eventsequence;
  begin match action with
     `Remove -> TkToken ""
  |  `Set (what, f) ->
      let cbId = register_callback widget callback:(wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  `Setbreakable (what, f) ->
      let cbId = register_callback widget callback:(wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  `Extend (what, f) ->
      let cbId = register_callback widget callback:(wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))

  end |];
  ()
