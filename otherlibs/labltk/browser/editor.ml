(* $Id$ *)

open Tk
open Parsetree
open Location
open Jg_tk
open Mytypes

let lex_on_load = ref true
and type_on_load = ref false

let compiler_preferences () =
  let tl = Jg_toplevel.titled "Compiler" in
  Wm.transient_set tl master:Widget.default_toplevel;
  let mk_chkbutton :text :ref =
    let variable = Textvariable.create on:tl () in
    if !ref then Textvariable.set variable to:"1";
    Checkbutton.create parent:tl :text :variable (),
    (fun () -> ref := Textvariable.get variable = "1")
  in
  let chkbuttons, setflags = List.split
      (List.map fun:(fun (text, ref) -> mk_chkbutton :text :ref)
      	["No pervasives", Clflags.nopervasives;
	 "No warnings", Typecheck.nowarnings;
	 "Classic", Clflags.classic;
	 "Lex on load", lex_on_load;
	 "Type on load", type_on_load])
  in
  let buttons = Frame.create parent:tl () in
  let ok = Button.create parent:buttons text:"Ok" padx:(`Pix 20) () command:
      begin fun () ->
      	List.iter fun:(fun f -> f ()) setflags;
	destroy tl
      end
  and cancel = Jg_button.create_destroyer tl parent:buttons text:"Cancel"
  in
  pack chkbuttons side:`Top anchor:`W;
  pack [ok;cancel] side:`Left fill:`X expand:true;
  pack [buttons] side:`Bottom fill:`X

let rec exclude elt:txt = function
    [] -> []
  | x :: l -> if txt.number = x.number then l else x :: exclude elt:txt l

let goto_line tw =
  let tl = Jg_toplevel.titled "Go to" in
  Wm.transient_set tl master:Widget.default_toplevel;
  Jg_bind.escape_destroy tl;
  let ef = Frame.create parent:tl () in
  let fl = Frame.create parent:ef ()
  and fi = Frame.create parent:ef () in
  let ll = Label.create parent:fl text:"Line number:" ()
  and il = Entry.create parent:fi width:10 ()
  and lc = Label.create parent:fl text:"Col number:" ()
  and ic = Entry.create parent:fi width:10 ()
  and get_int ew =
      try int_of_string (Entry.get ew)
      with Failure "int_of_string" -> 0
  in
  let buttons = Frame.create parent:tl () in
  let ok = Button.create parent:buttons text:"Ok" () command:
    begin fun () ->
      let l = get_int il
      and c = get_int ic in
      Text.mark_set tw mark:"insert" index:(`Linechar (l,0), [`Char c]);
      Text.see tw index:(`Mark "insert", []);
      destroy tl
    end
  and cancel = Jg_button.create_destroyer tl parent:buttons text:"Cancel" in

  Focus.set il;
  List.iter [il; ic] fun:
  begin fun w ->
    Jg_bind.enter_focus w;
    Jg_bind.return_invoke w button:ok
  end;
  pack [ll; lc] side:`Top anchor:`W;
  pack [il; ic] side:`Top fill:`X expand:true;
  pack [fl; fi] side:`Left fill:`X expand:true;
  pack [ok; cancel] side:`Left fill:`X expand:true;
  pack [ef; buttons] side:`Top fill:`X expand:true

let select_shell txt =
  let shells = Shell.get_all () in
  let shells = Sort.list shells order:(fun (x,_) (y,_) -> x <= y) in
  let tl = Jg_toplevel.titled "Select Shell" in
  Jg_bind.escape_destroy tl;
  Wm.transient_set tl master:(Winfo.toplevel txt.tw);
  let label = Label.create parent:tl text:"Send to:" ()
  and box = Listbox.create parent:tl ()
  and frame = Frame.create parent:tl () in
  Jg_bind.enter_focus box;
  let cancel = Jg_button.create_destroyer tl parent:frame text:"Cancel"
  and ok = Button.create parent:frame text:"Ok" () command:
      begin fun () ->
	try
	  let name = Listbox.get box index:`Active in
	  txt.shell <- Some (name, List.assoc key:name shells);
	  destroy tl
	with Not_found -> txt.shell <- None; destroy tl
      end
  in
  Listbox.insert box index:`End texts:(List.map fun:fst shells);
  Listbox.configure box height:(List.length shells);
  bind box events:[[],`KeyPressDetail"Return"]
    action:(`Setbreakable([], fun _ -> Button.invoke ok; break ()));
  bind box events:[[`Double],`ButtonPressDetail 1]
    action:(`Setbreakable([`MouseX;`MouseY], fun ev ->
      Listbox.activate box index:(`Atxy (ev.ev_MouseX, ev.ev_MouseY));
      Button.invoke ok; break ()));
  pack [label] side:`Top anchor:`W;
  pack [box] side:`Top fill:`Both;
  pack [frame] side:`Bottom fill:`X expand:true;
  pack [ok;cancel] side:`Left fill:`X expand:true

let send_region txt =
  if txt.shell = None then begin
    match Shell.get_all () with [] -> ()
    | [sh] -> txt.shell <- Some sh
    | l ->  select_shell txt
  end;
  match txt.shell with None -> ()
  | Some (_,sh) ->
      try
	let i1,i2 = Text.tag_nextrange txt.tw tag:"sel" start:tstart in
	sh#send (Text.get txt.tw start:(i1,[]) end:(i2,[]));
	sh#send";;\n"
      with _ -> ()

let search_pos_window txt :x :y =
  if txt.structure = [] & txt.psignature = [] then () else
  let `Linechar (l, c) = Text.index txt.tw index:(`Atxy(x,y), []) in
  let text = Jg_text.get_all txt.tw in
  let pos = Searchpos.lines_to_chars l in:text + c in
  try if txt.structure <> [] then
    try Searchpos.search_pos_structure txt.structure :pos
    with Searchpos.Found_str (kind, env) ->
      Searchpos.view_type kind :env
  else
    try Searchpos.search_pos_signature
 	txt.psignature :pos env:!Searchid.start_env;
      ()
    with Searchpos.Found_sig (kind, lid, env) ->
      Searchpos.view_decl lid :kind :env
  with Not_found -> ()

let search_pos_menu txt :x :y =
  if txt.structure = [] & txt.psignature = [] then () else
  let `Linechar (l, c) = Text.index txt.tw index:(`Atxy(x,y), []) in
  let text = Jg_text.get_all txt.tw in
  let pos = Searchpos.lines_to_chars l in:text + c in
  try if txt.structure <> [] then
    try Searchpos.search_pos_structure txt.structure :pos
    with Searchpos.Found_str (kind, env) ->
      let menu = Searchpos.view_type_menu kind :env parent:txt.tw in
      let x = x + Winfo.rootx txt.tw and y = y + Winfo.rooty txt.tw - 10 in
      Menu.popup menu :x :y
  else
    try Searchpos.search_pos_signature
 	txt.psignature :pos env:!Searchid.start_env;
      ()
    with Searchpos.Found_sig (kind, lid, env) ->
      let menu = Searchpos.view_decl_menu lid :kind :env parent:txt.tw in
      let x = x + Winfo.rootx txt.tw and y = y + Winfo.rooty txt.tw - 10 in
      Menu.popup menu :x :y
  with Not_found -> ()

let string_width s =
  let width = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\t' then width := (!width / 8 + 1) * 8
    else incr width
  done;
  !width

let indent_line =
  let ins = `Mark"insert" and reg = Str.regexp "[ \t]*" in
  fun tw ->
    let `Linechar(l,c) = Text.index tw index:(ins,[])
    and line = Text.get tw start:(ins,[`Linestart]) end:(ins,[]) in
    Str.string_match reg line pos:0;
    if Str.match_end () < c then
      Text.insert tw index:(ins,[]) text:"\t"
    else let indent =
      if l <= 1 then 2 else
      let previous =
	Text.get tw start:(ins,[`Line(-1);`Linestart])
	  end:(ins,[`Line(-1);`Lineend]) in
      Str.string_match reg previous pos:0;
      let previous = Str.matched_string previous in
      let width = string_width line
      and width_previous = string_width previous in
      if  width_previous <= width then 2 else width_previous - width
    in
    Text.insert tw index:(ins,[]) text:(String.make len:indent ' ')

(* The editor class *)

class editor :top :menus = object (self)
  val file_menu = new Jg_menu.c "File" parent:menus
  val edit_menu = new Jg_menu.c "Edit" parent:menus
  val compiler_menu = new Jg_menu.c "Compiler" parent:menus
  val module_menu = new Jg_menu.c "Modules" parent:menus
  val window_menu = new Jg_menu.c "Windows" parent:menus
  val label =
    Checkbutton.create parent:menus state:`Disabled
      onvalue:"modified" offvalue:"unchanged" ()
  val mutable current_dir = Unix.getcwd ()
  val mutable error_messages = []
  val mutable windows = []
  val mutable current_tw = Text.create parent:top ()
  val vwindow = Textvariable.create on:top ()
  val mutable window_counter = 0

  method reset_window_menu =
    Menu.delete window_menu#menu first:(`Num 0) last:`End;
    List.iter
      (Sort.list windows order:
	 (fun w1 w2 -> Filename.basename w1.name < Filename.basename w2.name))
      fun:
      begin fun txt ->
	Menu.add_radiobutton window_menu#menu
	  label:(Filename.basename txt.name)
	  variable:vwindow value:txt.number
	  command:(fun () -> self#set_edit txt)
      end

  method set_edit txt  =
    if windows <> [] then
      Pack.forget [(List.hd windows).frame];
    windows <- txt :: exclude elt:txt windows;
    self#reset_window_menu;
    current_tw <- txt.tw;
    Checkbutton.configure label text:(Filename.basename txt.name)
      variable:txt.modified;
    Textvariable.set vwindow to:txt.number;
    Text.yview txt.tw scroll:(`Page 0);
    pack [txt.frame] fill:`Both expand:true side:`Bottom

  method new_window name =
    let tl, tw, sb = Jg_text.create_with_scrollbar parent:top in
    Text.configure tw background:`White;
    Jg_bind.enter_focus tw;
    window_counter <- window_counter + 1;
    let txt =
      { name = name; tw = tw; frame = tl;
	number = string_of_int window_counter;
	modified = Textvariable.create on:tw ();
	shell = None;
	structure = []; signature = []; psignature = [] }
    in
    let control c = Char.chr (Char.code c - 96) in
    bind tw events:[[`Alt], `KeyPress] action:(`Set ([], fun _ -> ()));
    bind tw events:[[], `KeyPress]
      action:(`Set ([`Char], fun ev ->
	if ev.ev_Char <> "" &
	  (ev.ev_Char.[0] >= ' ' or
	   List.mem elt:ev.ev_Char.[0]
	     (List.map fun:control ['d'; 'h'; 'i'; 'k'; 'o'; 't'; 'w'; 'y']))
	then Textvariable.set txt.modified to:"modified"));
    bind tw events:[[],`KeyPressDetail"Tab"]
      action:(`Setbreakable ([], fun _ ->
	indent_line tw;
	Textvariable.set txt.modified to:"modified";
	break ()));
    bind tw events:[[`Control],`KeyPressDetail"k"]
      action:(`Set ([], fun _ ->
	let text =
	  Text.get tw start:(`Mark"insert",[]) end:(`Mark"insert",[`Lineend])
	in Str.string_match (Str.regexp "[ \t]*") text pos:0;
	if Str.match_end () <> String.length text then begin
	  Clipboard.clear ();
	  Clipboard.append data:text ()
	end));
    bind tw events:[[], `KeyRelease]
      action:(`Set ([`Char], fun ev ->
	if ev.ev_Char <> "" then
	  Lexical.tag tw start:(`Mark"insert", [`Linestart])
	    end:(`Mark"insert", [`Lineend])));
    bind tw events:[[], `Motion] action:(`Set ([], fun _ -> Focus.set tw));
    bind tw events:[[], `ButtonPressDetail 2]
      action:(`Set ([], fun _ ->
	Textvariable.set txt.modified to:"modified";
	Lexical.tag txt.tw start:(`Mark"insert", [`Linestart])
	  end:(`Mark"insert", [`Lineend])));
    bind tw events:[[`Double], `ButtonPressDetail 1]
      action:(`Set ([`MouseX;`MouseY], fun ev ->
	search_pos_window txt x:ev.ev_MouseX y:ev.ev_MouseY));
    bind tw events:[[], `ButtonPressDetail 3]
      action:(`Set ([`MouseX;`MouseY], fun ev ->
	search_pos_menu txt x:ev.ev_MouseX y:ev.ev_MouseY));

    pack [sb] fill:`Y side:`Right;
    pack [tw] fill:`Both expand:true side:`Left;
    self#set_edit txt;
    Checkbutton.deselect label;
    Lexical.init_tags txt.tw

  method clear_errors () =
    Text.tag_remove current_tw tag:"error" start:tstart end:tend;
    List.iter error_messages
      fun:(fun tl -> try destroy tl with Protocol.TkError _ -> ());
    error_messages <- []

  method typecheck () =
    self#clear_errors ();
    error_messages <- Typecheck.f (List.hd windows)

  method lex () =
    Text.tag_remove current_tw tag:"error" start:tstart end:tend;
    Lexical.tag current_tw

  method save_text ?name:l txt =
    let l = match l with None -> [txt.name] | Some l -> l in
    if l = [] then () else
    let name = List.hd l in
    if txt.name <> name then current_dir <- Filename.dirname name;
    try
      if Sys.file_exists name then
	if txt.name = name then
	  Sys.rename old:name new:(name ^ "~")
	else begin match
	  Jg_message.ask master:top title:"Save"
	    ("File `" ^ name ^ "' exists. Overwrite it?")
	with `yes -> () | `no | `cancel -> raise Exit
	end;
      let file = open_out name in
      let text = Text.get txt.tw start:tstart end:(tposend 1) in
      output_string text to:file;
      close_out file;
      Checkbutton.configure label text:(Filename.basename name);
      Checkbutton.deselect label;
      txt.name <- name
    with
      Sys_error _ | Exit -> ()

  method load_text l =
    if l = [] then () else
    let name = List.hd l in
    try
      let index =
	try
	  self#set_edit (List.find windows pred:(fun x -> x.name = name));
	  let txt = List.hd windows in
	  if Textvariable.get txt.modified = "modified" then
	    begin match Jg_message.ask master:top title:"Open"
		("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
	    with `yes -> self#save_text txt
	    | `no -> ()
	    | `cancel -> raise Exit
	    end;
	  Checkbutton.deselect label;
	  (Text.index current_tw index:(`Mark"insert", []), [])
	with Not_found -> self#new_window name; tstart
      in
      current_dir <- Filename.dirname name;
      let file = open_in name
      and tw = current_tw
      and len = ref 0
      and buffer = String.create len:4096 in
      Text.delete tw start:tstart end:tend;
      while
	len := input file :buffer pos:0 len:4096;
	!len > 0
      do
	Jg_text.output tw :buffer pos:0 len:!len
      done;
      close_in file;
      Text.mark_set tw mark:"insert" :index;
      Text.see tw :index;
      if Filename.check_suffix name suff:".ml" or
	Filename.check_suffix name suff:".mli"
      then begin
	if !lex_on_load then self#lex ();
	if !type_on_load then self#typecheck ()
      end
    with
      Sys_error _ | Exit -> ()

  method close_window txt =
    try
      if Textvariable.get txt.modified = "modified" then
	begin match Jg_message.ask master:top title:"Close"
	    ("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
	with `yes -> self#save_text txt
	| `no -> ()
	| `cancel -> raise Exit
	end;
      windows <- exclude elt:txt windows;
      if windows = [] then
	self#new_window (current_dir ^ "/untitled")
      else self#set_edit (List.hd windows);
      destroy txt.frame
    with Exit -> ()

  method open_file () =
    Fileselect.f title:"Open File" action:self#load_text
      dir:current_dir filter:("*.{ml,mli}") sync:true ()

  method save_file () = self#save_text (List.hd windows)

  method close_file () = self#close_window (List.hd windows)

  method quit () =
    try List.iter windows
	fun:(fun txt ->
	  if Textvariable.get txt.modified = "modified" then
	    match Jg_message.ask master:top title:"Quit"
		("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
	    with `yes -> self#save_text txt
	    | `no -> ()
	    | `cancel -> raise Exit);
      bind top events:[[],`Destroy] action:`Remove;
      destroy top; break ()
    with Exit -> break ()

  method reopen :file :pos =
    if not (Winfo.ismapped top) then Wm.deiconify top;
    match file with None -> ()
    | Some file ->
	self#load_text [file]; 
	Text.mark_set current_tw mark:"insert" index:(tpos pos);
	Text.yview_index current_tw
	  index:(`Linechar(1,0),[`Char pos; `Line (-2)])

  initializer
    (* Create a first window *)
    self#new_window (current_dir ^ "/untitled");

    (* Bindings for the main window *)
    List.iter
      [ [`Control], "s", (fun () -> Jg_text.search_string current_tw);
	[`Control], "g", (fun () -> goto_line current_tw);
	[`Alt], "x", (fun () -> send_region (List.hd windows));
	[`Alt], "l", self#lex;
	[`Alt], "t", self#typecheck ]
      fun:begin fun (modi,key,act) ->
	bind top events:[modi, `KeyPressDetail key]
	  action:(`Setbreakable ([], fun _ -> act (); break ()))
      end;

    bind top events:[[],`Destroy]
      action:(`Setbreakable
		([`Widget], fun ev ->
		  if Widget.name ev.ev_Widget = Widget.name top
		  then self#quit ()));

    (* File menu *)
    file_menu#add_command "Open File..." command:self#open_file;
    file_menu#add_command "Reopen"
      command:(fun () -> self#load_text [(List.hd windows).name]);
    file_menu#add_command "Save File" command:self#save_file;
    file_menu#add_command "Save As..." underline:5
      command:begin fun () ->
	let txt = List.hd windows in
	Fileselect.f title:"Save as File"
	  action:(fun name -> self#save_text txt :name)
	  dir:(Filename.dirname txt.name)
	  filter:"*.{ml,mli}"
	  file:(Filename.basename txt.name)
	  sync:true usepath:false ()
      end;
    file_menu#add_command "Close File" command:self#close_file;
    file_menu#add_command "Close Window" command:self#quit underline:6;

    (* Edit menu *)
    edit_menu#add_command "Paste selection" command:
      begin fun () ->
	Text.insert current_tw index:(`Mark"insert",[])
	  text:(Selection.get displayof:top ())
      end;
    edit_menu#add_command "Goto..." accelerator:"C-g"
      command:(fun () -> goto_line current_tw);
    edit_menu#add_command "Search..." accelerator:"C-s"
      command:(fun () -> Jg_text.search_string current_tw);
    edit_menu#add_command "To shell" accelerator:"M-x"
      command:(fun () -> send_region (List.hd windows));
    edit_menu#add_command "Select shell..."
      command:(fun () -> select_shell (List.hd windows));

    (* Compiler menu *)
    compiler_menu#add_command "Preferences..."
      command:compiler_preferences;
    compiler_menu#add_command "Lex" accelerator:"M-l"
      command:self#lex;
    compiler_menu#add_command "Typecheck" accelerator:"M-t"
      command:self#typecheck;
    compiler_menu#add_command "Clear errors"
      command:self#clear_errors;
    compiler_menu#add_command "Signature..." command:
      begin fun () ->
	let txt = List.hd windows in if txt.signature <> [] then
	  let basename = Filename.basename txt.name in
	  let modname = String.capitalize
	      (try Filename.chop_extension basename with _ -> basename) in
	  let env =
	    Env.add_module (Ident.create modname)
	      (Types.Tmty_signature txt.signature)
	      Env.initial
	  in Viewer.view_defined (Longident.Lident modname) :env
      end;

    (* Modules *)
    module_menu#add_command "Path editor..."
      command:(fun () -> Setpath.f dir:current_dir; ());
    module_menu#add_command "Reset cache"
      command:(fun () -> Setpath.exec_update_hooks (); Env.reset_cache ());
    module_menu#add_command "Search symbol..."
      command:Viewer.search_symbol;
    module_menu#add_command "Close all"
      command:Viewer.close_all_views;

    (* pack everything *)
    pack (List.map fun:(fun m -> coe m#button)
	    [file_menu; edit_menu; compiler_menu; module_menu; window_menu]
	  @ [coe label])
      side:`Left ipadx:(`Pix 5) anchor:`W;
    pack [menus] before:(List.hd windows).frame side:`Top fill:`X
end

(* The main function starts here ! *)

let already_open : editor option ref = ref None

let editor ?:file ?:pos{= 0} () =

  if match !already_open with None -> false
  | Some ed ->
      try ed#reopen :file :pos; true
      with Protocol.TkError _ -> already_open := None; false
  then () else
    let top = Jg_toplevel.titled "Editor" in
    let menus = Frame.create parent:top name:"menubar" () in
    let ed = new editor :top :menus in
    already_open := Some ed;
    if file <> None then ed#reopen :file :pos

let f ?:file ?:pos ?:opendialog{=false} () =
  if opendialog then
    Fileselect.f title:"Open File"
      action:(function [file] -> editor :file () | _ -> ())
      filter:("*.{ml,mli}") sync:true ()
  else editor ?:file ?:pos ()
