(* $Id$ *)

val f :
  title:string ->
  action:(string list -> unit) ->
  ?dir:string ->
  ?filter:string ->
  ?file:string ->
  ?multi:bool -> ?sync:bool -> ?usepath:bool -> unit -> unit

(* action 
      []  means canceled
      if multi select is false, then the list is null or a singleton *)

(* multi
      If true then more than one file are selectable *)

(* sync
      If true then synchronous mode *)

(* usepath
      Enables/disables load path search. Defaults to true *)
