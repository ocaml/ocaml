(* $Id$ *)

val formatted :
  title:string ->
  ?on:Widget.frame Widget.widget ->
  ?width:int ->
  ?maxheight:int ->
  ?minheight:int ->
  unit -> Widget.any Widget.widget * Widget.text Widget.widget * (unit -> unit)

val ask :
    title:string -> ?master:Widget.toplevel Widget.widget ->
    string -> [`cancel|`no|`yes]
