(* $Id$ *)

open Tk

let titled ?:iconname title  =
  let iconname = match iconname with None -> title | Some s -> s in 
  let tl = Toplevel.create parent:Widget.default_toplevel () in
  Wm.title_set tl :title;
  Wm.iconname_set tl name:iconname;
  tl
