let bind_class :events ?(:extend = false) ?(:breakable = false) ?(:fields = [])
    ?:action ?(on:widget) name =
  let widget = match widget with None -> Widget.dummy | Some w -> coe w in
  tkCommand
    [| TkToken "bind";
       TkToken name;
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

let bind :events ?:extend ?:breakable ?:fields ?:action widget =
  bind_class :events ?:extend ?:breakable ?:fields ?:action on:widget
    (Widget.name widget)

let bind_tag = bind_class

(*
FUNCTION
  val break : unit -> unit
/FUNCTION
*)
let break = function () ->
  tkCommand [| TkToken "set" ; TkToken "BreakBindingsSequence" ; TkToken "1" |]
