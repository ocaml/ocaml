(* $Id$ *)

open Tk
open Jg_tk

let get_all tw = Text.get tw start:tstart end:(tposend 1)

let tag_and_see tw :tag :start end:e =
  Text.tag_remove tw start:(tpos 0) end:tend :tag;
  Text.tag_add tw :start end:e :tag;
  try
    Text.see tw index:(`Tagfirst tag, []);
    Text.mark_set tw mark:"insert" index:(`Tagfirst tag, [])
  with Protocol.TkError _ -> ()

let output tw :buffer :pos :len =
  Text.insert tw index:tend text:(String.sub buffer :pos :len)

let add_scrollbar tw =
  let sb = Scrollbar.create parent:(Winfo.parent tw) command:(Text.yview tw) ()
  in Text.configure tw yscrollcommand:(Scrollbar.set sb); sb

let create_with_scrollbar :parent =
  let frame = Frame.create :parent () in
  let tw = Text.create parent:frame () in
  frame, tw, add_scrollbar tw

let goto_tag tw :tag =
  let index = (`Tagfirst tag, []) in
  try Text.see tw :index;
      Text.mark_set tw :index mark:"insert"
  with Protocol.TkError _ -> ()

let search_string tw =
  let tl = Jg_toplevel.titled "Search" in
  Wm.transient_set tl master:Widget.default_toplevel;
  let fi = Frame.create parent:tl ()
  and fd = Frame.create parent:tl ()
  and fm = Frame.create parent:tl ()
  and buttons = Frame.create parent:tl ()
  and direction = Textvariable.create on:tl ()
  and mode = Textvariable.create on:tl ()
  and count = Textvariable.create on:tl ()
  in
  let label = Label.create parent:fi text:"Pattern:" ()
  and text = Entry.create parent:fi width:20 ()
  and back = Radiobutton.create parent:fd variable:direction
      	       text:"Backwards" value:"backward" ()
  and forw = Radiobutton.create parent:fd variable:direction
               text:"Forwards" value:"forward" ()
  and exact = Radiobutton.create parent:fm variable:mode
                text:"Exact" value:"exact" ()
  and nocase = Radiobutton.create parent:fm variable:mode
                 text:"No case" value:"nocase" ()
  and regexp =  Radiobutton.create parent:fm variable:mode
                 text:"Regexp" value:"regexp" ()
  in
  let search = Button.create parent:buttons text:"Search" () command:
    begin fun () ->
    try
      let pattern = Entry.get text in
      let dir, ofs = match Textvariable.get direction with
          "forward" -> `Forwards, 1
        | "backward" -> `Backwards, -1
      and mode = match Textvariable.get mode with "exact" -> [`Exact]
                 | "nocase" -> [`Nocase] | "regexp" -> [`Regexp] | _ -> []
      in
      let ndx =
        Text.search tw :pattern switches:([dir;`Count count] @ mode)
	  start:(`Mark "insert", [`Char ofs])
      in
      tag_and_see tw tag:"sel" start:(ndx,[])
      	end:(ndx,[`Char(int_of_string (Textvariable.get count))])
    with Invalid_argument _ -> ()
    end
  and ok = Jg_button.create_destroyer tl parent:buttons text:"Cancel" in

  Focus.set text;
  Jg_bind.return_invoke text button:search;
  Jg_bind.escape_destroy tl;
  Textvariable.set direction to:"forward";
  Textvariable.set mode to:"nocase";
  pack [label] side:`Left;
  pack [text] side:`Right fill:`X expand:true;
  pack [back; forw] side:`Left;
  pack [exact; nocase; regexp] side:`Left;
  pack [search; ok] side:`Left fill:`X expand:true;
  pack [fi; fd; fm; buttons] side:`Top fill:`X
