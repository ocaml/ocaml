(* $Id$ *)

open Widget

val add_update_hook : (unit -> unit) -> unit
val exec_update_hooks : unit -> unit
    (* things to do when Config.load_path changes *)

val f : dir:string -> toplevel widget
    (* edit the load path *)  
