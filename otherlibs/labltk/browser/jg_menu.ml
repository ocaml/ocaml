(* $Id$ *)

open Tk

class c :parent ?underline:n{=0} text = object (self)
  val pair =
    let button =
      Menubutton.create :parent :text underline:n  () in
    let menu =  Menu.create parent:button () in
    Menubutton.configure button :menu;
    button, menu
  method button = fst pair
  method menu = snd pair
  method virtual add_command :
      ?underline:int ->
      ?accelerator:string ->     ?activebackground:color ->
      ?activeforeground:color -> ?background:color ->
      ?bitmap:bitmap ->          ?command:(unit -> unit) ->
      ?font:string ->            ?foreground:color ->
      ?image:image ->            ?state:state ->
      string -> unit
  method add_command ?underline:n{=0} ?:accelerator ?:activebackground
      ?:activeforeground ?:background ?:bitmap ?:command ?:font ?:foreground
      ?:image ?:state label =
    Menu.add_command (self#menu) :label underline:n ?:accelerator
      ?:activebackground ?:activeforeground ?:background ?:bitmap
      ?:command ?:font ?:foreground ?:image ?:state
end
