(* $Id$ *)

open Tk

let titled ?:iconname title  =
  let iconname = match iconname with None -> title | Some s -> s in 
  let tl = Toplevel.create Widget.default_toplevel in
  Wm.title_set tl :title;
  Wm.iconname_set tl name:iconname;
  Wm.group_set tl leader: Widget.default_toplevel;
  tl
