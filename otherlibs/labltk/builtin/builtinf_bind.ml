(*
FUNCTION
 val bind: 
    any widget -> (modifier list * xEvent) list -> bindAction -> unit
/FUNCTION
*)
let bind widget events:eventsequence action:(action : bindAction) =
  tkEval [| TkToken "bind";
      	    TkToken (Widget.name widget);
	    cCAMLtoTKeventSequence eventsequence;
  begin match action with
     `Remove -> TkToken ""
  |  `Set (what, f) ->
      let cbId = register_callback widget callback: (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  `Setbreakable (what, f) ->
      let cbId = register_callback widget callback: (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  `Extend (what, f) ->
      let cbId = register_callback widget callback: (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
      
  end
  |];
  ()

(* 
FUNCTION
(* unsafe *)
 val class_bind : 
    string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION
 class arg is not constrained
*)
let class_bind clas events:eventsequence action:(action : bindAction) =
  tkEval [| TkToken "bind";
      	    TkToken clas;
	    cCAMLtoTKeventSequence eventsequence;
  begin match action with
     `Remove -> TkToken ""
  |  `Set (what, f) ->
      let cbId = register_callback Widget.dummy 
      	                           callback: (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  `Setbreakable (what, f) ->
      let cbId = register_callback Widget.dummy 
      	                           callback: (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  `Extend (what, f) ->
      let cbId = register_callback Widget.dummy 
                                   callback: (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
      
  end
 |];
  ()

(* 
FUNCTION
(* unsafe *)
 val tag_bind : 
    string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION
 tag name arg is not constrained 
*)

let tag_bind = class_bind


(*
FUNCTION
  val break : unit -> unit
/FUNCTION
*)
let break = function () ->
  tkEval [| TkToken "set" ; TkToken "BreakBindingsSequence" ; TkToken "1" |];
  ()
