##ifdef CAMLTK

(* type *)
type bindAction =
 | BindSet of eventField list *  (eventInfo -> unit)
 | BindSetBreakable of eventField list *  (eventInfo -> unit)
 | BindRemove
 | BindExtend of eventField list *  (eventInfo -> unit)
(* /type *)

(*
FUNCTION
 val bind: 
    widget -> (modifier list * xEvent) list -> bindAction -> unit
/FUNCTION
*)
let bind widget eventsequence action =
  tkCommand [| TkToken "bind";
               TkToken (Widget.name widget);
               cCAMLtoTKeventSequence eventsequence;
               begin match action with
                 BindRemove -> TkToken ""
               | BindSet (what, f) ->
                   let cbId = register_callback widget (wrapeventInfo f what) 
                   in
                   TkToken ("camlcb " ^ cbId ^ (writeeventField what))
               | BindSetBreakable (what, f) ->
                   let cbId = register_callback widget (wrapeventInfo f what) 
                   in
                   TkToken ("camlcb " ^ cbId ^ (writeeventField what) ^
                            " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0")
               |  BindExtend (what, f) ->
                   let cbId = register_callback widget (wrapeventInfo f what) 
                   in
                   TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
               end |]
;;

(* FUNCTION
(* unsafe *)
 val bind_class :
    string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION class arg is not constrained *)

let bind_class clas eventsequence action =
  tkCommand [| TkToken "bind";
               TkToken clas;
               cCAMLtoTKeventSequence eventsequence;
               begin match action with
                 BindRemove -> TkToken ""
               | BindSet (what, f) ->
                   let cbId = register_callback Widget.dummy 
                       (wrapeventInfo f what) in
                   TkToken ("camlcb " ^ cbId ^ (writeeventField what))
               | BindSetBreakable (what, f) ->
                   let cbId = register_callback Widget.dummy 
                       (wrapeventInfo f what) in
                   TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                            " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0" )
               | BindExtend (what, f) ->
                   let cbId = register_callback Widget.dummy 
                       (wrapeventInfo f what) in
                   TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
               end |]
;;

(* FUNCTION
(* unsafe *)
  val bind_tag : 
     string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION *)

let bind_tag = bind_class
;;

(*
FUNCTION
  val break : unit -> unit
/FUNCTION
*)
let break = function () ->
  Textvariable.set (Textvariable.coerce "BreakBindingsSequence") "1"
;;

(* Legacy functions *)
let tag_bind = bind_tag;;
let class_bind = bind_class;;
 
##else

let bind_class ~events ?(extend = false) ?(breakable = false) ?(fields = [])
    ?action ?on:widget name =
  let widget = match widget with None -> Widget.dummy | Some w -> coe w in
  tkCommand
    [| TkToken "bind";
       TkToken name;
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
;;

let bind ~events ?extend ?breakable ?fields ?action widget =
  bind_class ~events ?extend ?breakable ?fields ?action ~on:widget
    (Widget.name widget)
;;

let bind_tag = bind_class
;;

(*
FUNCTION
  val break : unit -> unit
/FUNCTION
*)
let break = function () ->
  tkCommand [| TkToken "set" ; TkToken "BreakBindingsSequence" ; TkToken "1" |]
;;

##endif
