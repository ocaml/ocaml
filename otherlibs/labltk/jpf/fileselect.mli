open Support

(* fileselect.mli *)

val f :
  title:string ->
  action:(string list -> unit) ->
  filter:string -> file:string -> multi:bool -> sync:bool -> unit

(* action 
      []  means canceled
      if multi select is false, then the list is null or a singleton *)

(* multi select 
      if true then more than one file are selectable *)

(* sync it 
      if true then in synchronous mode *)
