(* $Id$ *)

(* toplevel shell *)

class shell :
  textw:Widget.text Widget.widget -> prog:string ->
  args:string array -> env:string array ->
  object
    method alive : bool
    method kill : unit
    method interrupt : unit
    method insert : string -> unit
    method send : string -> unit
    method history : [`next|`previous] -> unit
  end

val kill_all : unit -> unit
val get_all : unit -> (string * shell) list

val f : prog:string -> title:string -> unit
