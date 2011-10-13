(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open Tk
open Parsetree
open Location
open Jg_tk
open Mytypes

let lex_on_load = ref true
and type_on_load = ref false

let compiler_preferences master =
  let tl = Jg_toplevel.titled "Compiler" in
  Wm.transient_set tl ~master;
  let mk_chkbutton ~text ~ref ~invert =
    let variable = Textvariable.create ~on:tl () in
    if (if invert then not !ref else !ref) then
      Textvariable.set variable "1";
    Checkbutton.create tl ~text ~variable,
    (fun () ->
      ref := Textvariable.get variable = (if invert then "0" else "1"))
  in
  let use_pp = ref (!Clflags.preprocessor <> None) in
  let chkbuttons, setflags = List.split
      (List.map
         ~f:(fun (text, ref, invert) -> mk_chkbutton ~text ~ref ~invert)
         [ "No pervasives", Clflags.nopervasives, false;
           "No warnings", Typecheck.nowarnings, false;
           "No labels", Clflags.classic, false;
           "Recursive types", Clflags.recursive_types, false;
           "Lex on load", lex_on_load, false;
           "Type on load", type_on_load, false;
           "Preprocessor", use_pp, false ])
  in
  let pp_command = Entry.create tl (* ~state:(if !use_pp then `Normal else`Disabled) *) in
  begin match !Clflags.preprocessor with None -> ()
  | Some pp -> Entry.insert pp_command ~index:(`Num 0) ~text:pp
  end;
  let buttons = Frame.create tl in
  let ok = Button.create buttons ~text:"Ok" ~padx:20 ~command:
      begin fun () ->
        List.iter ~f:(fun f -> f ()) setflags;
        Clflags.preprocessor :=
          if !use_pp then Some (Entry.get pp_command) else None;
        destroy tl
      end
  and cancel = Jg_button.create_destroyer tl ~parent:buttons ~text:"Cancel"
  in
  pack chkbuttons ~side:`Top ~anchor:`W;
  pack [pp_command] ~side:`Top ~anchor:`E;
  pack [ok;cancel] ~side:`Left ~fill:`X ~expand:true;
  pack [buttons] ~side:`Bottom ~fill:`X

let rec exclude txt = function
    [] -> []
  | x :: l -> if txt.number = x.number then l else x :: exclude txt l

let goto_line tw =
  let tl = Jg_toplevel.titled "Go to" in
  Wm.transient_set tl ~master:(Winfo.toplevel tw);
  Jg_bind.escape_destroy tl;
  let ef = Frame.create tl in
  let fl = Frame.create ef
  and fi = Frame.create ef in
  let ll = Label.create fl ~text:"Line ~number:"
  and il = Entry.create fi ~width:10
  and lc = Label.create fl ~text:"Col ~number:"
  and ic = Entry.create fi ~width:10
  and get_int ew =
      try int_of_string (Entry.get ew)
      with Failure "int_of_string" -> 0
  in
  let buttons = Frame.create tl in
  let ok = Button.create buttons ~text:"Ok" ~command:
    begin fun () ->
      let l = get_int il
      and c = get_int ic in
      Text.mark_set tw ~mark:"insert" ~index:(`Linechar (l,0), [`Char c]);
      Text.see tw ~index:(`Mark "insert", []);
      destroy tl
    end
  and cancel = Jg_button.create_destroyer tl ~parent:buttons ~text:"Cancel" in

  Focus.set il;
  List.iter [il; ic] ~f:
  begin fun w ->
    Jg_bind.enter_focus w;
    Jg_bind.return_invoke w ~button:ok
  end;
  pack [ll; lc] ~side:`Top ~anchor:`W;
  pack [il; ic] ~side:`Top ~fill:`X ~expand:true;
  pack [fl; fi] ~side:`Left ~fill:`X ~expand:true;
  pack [ok; cancel] ~side:`Left ~fill:`X ~expand:true;
  pack [ef; buttons] ~side:`Top ~fill:`X ~expand:true

let select_shell txt =
  let shells = Shell.get_all () in
  let shells = List.sort shells ~cmp:compare in
  let tl = Jg_toplevel.titled "Select Shell" in
  Jg_bind.escape_destroy tl;
  Wm.transient_set tl ~master:(Winfo.toplevel txt.tw);
  let label = Label.create tl ~text:"Send to:"
  and box = Listbox.create tl
  and frame = Frame.create tl in
  Jg_bind.enter_focus box;
  let cancel = Jg_button.create_destroyer tl ~parent:frame ~text:"Cancel"
  and ok = Button.create frame ~text:"Ok" ~command:
      begin fun () ->
        try
          let name = Listbox.get box ~index:`Active in
          txt.shell <- Some (name, List.assoc name shells);
          destroy tl
        with Not_found -> txt.shell <- None; destroy tl
      end
  in
  Listbox.insert box ~index:`End ~texts:(List.map ~f:fst shells);
  Listbox.configure box ~height:(List.length shells);
  bind box ~events:[`KeyPressDetail"Return"] ~breakable:true
    ~action:(fun _ -> Button.invoke ok; break ());
  bind box ~events:[`Modified([`Double],`ButtonPressDetail 1)] ~breakable:true
    ~fields:[`MouseX;`MouseY]
    ~action:(fun ev ->
      Listbox.activate box ~index:(`Atxy (ev.ev_MouseX, ev.ev_MouseY));
      Button.invoke ok; break ());
  pack [label] ~side:`Top ~anchor:`W;
  pack [box] ~side:`Top ~fill:`Both;
  pack [frame] ~side:`Bottom ~fill:`X ~expand:true;
  pack [ok;cancel] ~side:`Left ~fill:`X ~expand:true

open Parser

let send_phrase txt =
  if txt.shell = None then begin
    match Shell.get_all () with [] -> ()
    | [sh] -> txt.shell <- Some sh
    | l ->  select_shell txt
  end;
  match txt.shell with None -> ()
  | Some (_,sh) ->
      try
        let i1,i2 = Text.tag_nextrange txt.tw ~tag:"sel" ~start:tstart in
        let phrase = Text.get txt.tw ~start:(i1,[]) ~stop:(i2,[]) in
        sh#send phrase;
        if Str.string_match (Str.regexp ";;") phrase 0
        then sh#send "\n" else sh#send ";;\n"
      with Not_found | Protocol.TkError _ ->
        let text = Text.get txt.tw ~start:tstart ~stop:tend in
        let buffer = Lexing.from_string text in
        let start = ref 0
        and block_start = ref []
        and pend = ref (-1)
        and after = ref false in
        while !pend = -1 do
          let token = Lexer.token buffer in
          let pos =
            if token = SEMISEMI then Lexing.lexeme_end buffer
            else Lexing.lexeme_start buffer
          in
          let bol = (pos = 0) || text.[pos-1] = '\n' in
          if not !after &&
            Text.compare txt.tw ~index:(tpos pos) ~op:(if bol then `Gt else `Ge)
              ~index:(`Mark"insert",[])
          then begin
            after := true;
            let anon, real =
              List.partition !block_start ~f:(fun x -> x = -1) in
            block_start := anon;
            if real <> [] then start := List.hd real;
          end;
          match token with
            CLASS | EXTERNAL | EXCEPTION | FUNCTOR
          | LET | MODULE | OPEN | TYPE | VAL | SHARP when bol ->
              if !block_start = [] then
                if !after then pend := pos else start := pos
              else block_start := pos :: List.tl !block_start
          | SEMISEMI ->
              if !block_start = [] then
                if !after then pend := Lexing.lexeme_start buffer
                else start := pos
              else block_start := pos :: List.tl !block_start
          | BEGIN | OBJECT ->
              block_start := -1 :: !block_start
          | STRUCT | SIG ->
              block_start := Lexing.lexeme_end buffer :: !block_start
          | END ->
              if !block_start = [] then
                if !after then pend := pos else ()
              else block_start := List.tl !block_start
          | EOF ->
              pend := pos
          | _ ->
              ()
        done;
        let phrase = String.sub text ~pos:!start ~len:(!pend - !start) in
        sh#send phrase;
        sh#send ";;\n"

let search_pos_window txt ~x ~y =
  if txt.type_info = [] && txt.psignature = [] then () else
  let `Linechar (l, c) = Text.index txt.tw ~index:(`Atxy(x,y), []) in
  let text = Jg_text.get_all txt.tw in
  let pos = Searchpos.lines_to_chars l ~text + c in
  try if txt.type_info <> [] then begin match
    Searchpos.search_pos_info txt.type_info ~pos
  with [] -> ()
  | (kind, env, loc) :: _ -> Searchpos.view_type kind ~env
  end else begin match
    Searchpos.search_pos_signature txt.psignature ~pos ~env:!Searchid.start_env
  with [] -> ()
  | ((kind, lid), env, loc) :: _ ->
      Searchpos.view_decl lid ~kind ~env
  end
  with Not_found -> ()

let search_pos_menu txt ~x ~y =
  if txt.type_info = [] && txt.psignature = [] then () else
  let `Linechar (l, c) = Text.index txt.tw ~index:(`Atxy(x,y), []) in
  let text = Jg_text.get_all txt.tw in
  let pos = Searchpos.lines_to_chars l ~text + c in
  try if txt.type_info <> [] then begin match
    Searchpos.search_pos_info txt.type_info ~pos
  with [] -> ()
  | (kind, env, loc) :: _ ->
      let menu = Searchpos.view_type_menu kind ~env ~parent:txt.tw in
      let x = x + Winfo.rootx txt.tw and y = y + Winfo.rooty txt.tw - 10 in
      Menu.popup menu ~x ~y
  end else begin match
    Searchpos.search_pos_signature txt.psignature ~pos ~env:!Searchid.start_env
  with [] -> ()
  | ((kind, lid), env, loc) :: _ ->
      let menu = Searchpos.view_decl_menu lid ~kind ~env ~parent:txt.tw in
      let x = x + Winfo.rootx txt.tw and y = y + Winfo.rooty txt.tw - 10 in
      Menu.popup menu ~x ~y
  end
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
    let `Linechar(l,c) = Text.index tw ~index:(ins,[])
    and line = Text.get tw ~start:(ins,[`Linestart]) ~stop:(ins,[`Lineend]) in
    ignore (Str.string_match reg line 0);
    let len = Str.match_end () in
    if len < c then Text.insert tw ~index:(ins,[]) ~text:"\t" else
    let width = string_width (Str.matched_string line) in
    Text.mark_set tw ~mark:"insert" ~index:(ins,[`Linestart;`Char len]);
    let indent =
      if l <= 1 then 2 else
      let previous =
        Text.get tw ~start:(ins,[`Line(-1);`Linestart])
          ~stop:(ins,[`Line(-1);`Lineend]) in
      ignore (Str.string_match reg previous 0);
      let previous = Str.matched_string previous in
      let width_previous = string_width previous in
      if  width_previous <= width then 2 else width_previous - width
    in
    Text.insert tw ~index:(ins,[]) ~text:(String.make indent ' ')

(* The editor class *)

class editor ~top ~menus = object (self)
  val file_menu = new Jg_menu.c "File" ~parent:menus
  val edit_menu = new Jg_menu.c "Edit" ~parent:menus
  val compiler_menu = new Jg_menu.c "Compiler" ~parent:menus
  val module_menu = new Jg_menu.c "Modules" ~parent:menus
  val window_menu = new Jg_menu.c "Windows" ~parent:menus
  initializer
    Menu.add_checkbutton menus ~state:`Disabled
      ~onvalue:"modified" ~offvalue:"unchanged"
  val mutable current_dir = Unix.getcwd ()
  val mutable error_messages = []
  val mutable windows = []
  val mutable current_tw = Text.create top
  val vwindow = Textvariable.create ~on:top ()
  val mutable window_counter = 0

  method has_window name =
    List.exists windows ~f:(fun x -> x.name = name)

  method reset_window_menu =
    Menu.delete window_menu#menu ~first:(`Num 0) ~last:`End;
    List.iter
      (List.sort windows ~cmp:
         (fun w1 w2 ->
           compare (Filename.basename w1.name) (Filename.basename w2.name)))
      ~f:
      begin fun txt ->
        Menu.add_radiobutton window_menu#menu
          ~label:(Filename.basename txt.name)
          ~variable:vwindow ~value:txt.number
          ~command:(fun () -> self#set_edit txt)
      end

  method set_file_name txt =
    Menu.configure_checkbutton menus `Last
      ~label:(Filename.basename txt.name)
      ~variable:txt.modified

  method set_edit txt  =
    if windows <> [] then
      Pack.forget [(List.hd windows).frame];
    windows <- txt :: exclude txt windows;
    self#reset_window_menu;
    current_tw <- txt.tw;
    self#set_file_name txt;
    Textvariable.set vwindow txt.number;
    Text.yview txt.tw ~scroll:(`Page 0);
    pack [txt.frame] ~fill:`Both ~expand:true ~side:`Bottom

  method new_window name =
    let tl, tw, sb = Jg_text.create_with_scrollbar top in
    Text.configure tw ~background:`White;
    Jg_bind.enter_focus tw;
    window_counter <- window_counter + 1;
    let txt =
      { name = name; tw = tw; frame = tl;
        number = string_of_int window_counter;
        modified = Textvariable.create ~on:tw ();
        shell = None;
        structure = []; type_info = []; signature = []; psignature = [] }
    in
    let control c = Char.chr (Char.code c - 96) in
    bind tw ~events:[`Modified([`Alt], `KeyPress)] ~action:ignore;
    bind tw ~events:[`KeyPress] ~fields:[`Char]
      ~action:(fun ev ->
        if ev.ev_Char <> "" &&
          (ev.ev_Char.[0] >= ' ' ||
           List.mem ev.ev_Char.[0]
             (List.map ~f:control ['d'; 'h'; 'i'; 'k'; 'o'; 't'; 'w'; 'y']))
        then Textvariable.set txt.modified "modified");
    bind tw ~events:[`KeyPressDetail"Tab"] ~breakable:true
      ~action:(fun _ ->
        indent_line tw;
        Textvariable.set txt.modified "modified";
        break ());
    bind tw ~events:[`Modified([`Control],`KeyPressDetail"k")]
      ~action:(fun _ ->
        let text =
          Text.get tw ~start:(`Mark"insert",[]) ~stop:(`Mark"insert",[`Lineend])
        in ignore (Str.string_match (Str.regexp "[ \t]*") text 0);
        if Str.match_end () <> String.length text then begin
          Clipboard.clear ();
          Clipboard.append ~data:text ()
        end);
    bind tw ~events:[`KeyRelease] ~fields:[`Char]
      ~action:(fun ev ->
        if ev.ev_Char <> "" then
          Lexical.tag tw ~start:(`Mark"insert", [`Linestart])
            ~stop:(`Mark"insert", [`Lineend]));
    bind tw ~events:[`Motion] ~action:(fun _ -> Focus.set tw);
    bind tw ~events:[`ButtonPressDetail 2]
      ~action:(fun _ ->
        Textvariable.set txt.modified "modified";
        Lexical.tag txt.tw ~start:(`Mark"insert", [`Linestart])
          ~stop:(`Mark"insert", [`Lineend]));
    bind tw ~events:[`Modified([`Double], `ButtonPressDetail 1)]
      ~fields:[`MouseX;`MouseY]
      ~action:(fun ev -> search_pos_window txt ~x:ev.ev_MouseX ~y:ev.ev_MouseY);
    bind tw ~events:[`ButtonPressDetail 3] ~fields:[`MouseX;`MouseY]
      ~action:(fun ev -> search_pos_menu txt ~x:ev.ev_MouseX ~y:ev.ev_MouseY);

    pack [sb] ~fill:`Y ~side:`Right;
    pack [tw] ~fill:`Both ~expand:true ~side:`Left;
    self#set_edit txt;
    Textvariable.set txt.modified "unchanged";
    Lexical.init_tags txt.tw

  method clear_errors () =
    Text.tag_remove current_tw ~tag:"error" ~start:tstart ~stop:tend;
    List.iter error_messages
      ~f:(fun tl -> try destroy tl with Protocol.TkError _ -> ());
    error_messages <- []

  method typecheck () =
    self#clear_errors ();
    error_messages <- Typecheck.f (List.hd windows)

  method lex () =
    List.iter [ Widget.default_toplevel; top ]
      ~f:(Toplevel.configure ~cursor:(`Xcursor "watch"));
    Text.configure current_tw ~cursor:(`Xcursor "watch");
    ignore (Timer.add ~ms:1 ~callback:
      begin fun () ->
        Text.tag_remove current_tw ~tag:"error" ~start:tstart ~stop:tend;
        Lexical.tag current_tw;
        Text.configure current_tw ~cursor:(`Xcursor "xterm");
        List.iter [ Widget.default_toplevel; top ]
          ~f:(Toplevel.configure ~cursor:(`Xcursor ""))
      end)

  method save_text ?name:l txt =
    let l = match l with None -> [txt.name] | Some l -> l in
    if l = [] then () else
    let name = List.hd l in
    if txt.name <> name then current_dir <- Filename.dirname name;
    try
      if Sys.file_exists name then
        if txt.name = name then begin
          let backup = name ^ "~" in
          if Sys.file_exists backup then Sys.remove backup;
          try Sys.rename name backup with Sys_error _ -> ()
        end else begin
          match Jg_message.ask ~master:top ~title:"Save"
              ("File `" ^ name ^ "' exists. Overwrite it?")
          with `Yes -> Sys.remove name
          | `No -> raise (Sys_error "")
          | `Cancel -> raise Exit
        end;
      let file = open_out name in
      let text = Text.get txt.tw ~start:tstart ~stop:(tposend 1) in
      output_string file text;
      close_out file;
      txt.name <- name;
      self#set_file_name txt
    with
      Sys_error _ ->
        Jg_message.info ~master:top ~title:"Error"
          ("Could not save `" ^ name ^ "'.")
    | Exit -> ()

  method load_text l =
    if l = [] then () else
    let name = List.hd l in
    try
      let index =
        try
          self#set_edit (List.find windows ~f:(fun x -> x.name = name));
          let txt = List.hd windows in
          if Textvariable.get txt.modified = "modified" then
            begin match Jg_message.ask ~master:top ~title:"Open"
                ("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
            with `Yes -> self#save_text txt
            | `No -> ()
            | `Cancel -> raise Exit
            end;
          Textvariable.set txt.modified "unchanged";
          (Text.index current_tw ~index:(`Mark"insert", []), [])
        with Not_found -> self#new_window name; tstart
      in
      current_dir <- Filename.dirname name;
      let file = open_in name
      and tw = current_tw
      and len = ref 0
      and buf = String.create 4096 in
      Text.delete tw ~start:tstart ~stop:tend;
      while
        len := input file buf 0 4096;
        !len > 0
      do
        Jg_text.output tw ~buf ~pos:0 ~len:!len
      done;
      close_in file;
      Text.mark_set tw ~mark:"insert" ~index;
      Text.see tw ~index;
      if Filename.check_suffix name ".ml" ||
        Filename.check_suffix name ".mli"
      then begin
        if !lex_on_load then self#lex ();
        if !type_on_load then self#typecheck ()
      end
    with
      Sys_error _ | Exit -> ()

  method close_window txt =
    try
      if Textvariable.get txt.modified = "modified" then
        begin match Jg_message.ask ~master:top ~title:"Close"
            ("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
        with `Yes -> self#save_text txt
        | `No -> ()
        | `Cancel -> raise Exit
        end;
      windows <- exclude txt windows;
      if windows = [] then
        self#new_window (current_dir ^ "/untitled")
      else self#set_edit (List.hd windows);
      destroy txt.frame
    with Exit -> ()

  method open_file () =
    Fileselect.f ~title:"Open File" ~action:self#load_text
      ~dir:current_dir ~filter:("*.{ml,mli}") ~sync:true ()

  method save_file () = self#save_text (List.hd windows)

  method close_file () = self#close_window (List.hd windows)

  method quit ?(cancel=true) () =
    try
      List.iter windows ~f:
        begin fun txt ->
          if Textvariable.get txt.modified = "modified" then
            match Jg_message.ask ~master:top ~title:"Quit" ~cancel
                ("`" ^ Filename.basename txt.name ^ "' modified. Save it?")
            with `Yes -> self#save_text txt
            | `No -> ()
            | `Cancel -> raise Exit
        end;
      bind top ~events:[`Destroy];
      destroy top
    with Exit -> ()

  method reopen ~file ~pos =
    if not (Winfo.ismapped top) then Wm.deiconify top;
    match file with None -> ()
    | Some file ->
        self#load_text [file];
        Text.mark_set current_tw ~mark:"insert" ~index:(tpos pos);
        try
          let index =
            Text.search current_tw ~switches:[`Backwards] ~pattern:"*)"
              ~start:(tpos pos) ~stop:(tpos pos ~modi:[`Line(-1)]) in
          let index =
            Text.search current_tw ~switches:[`Backwards] ~pattern:"(*"
              ~start:(index,[]) ~stop:(tpos pos ~modi:[`Line(-20)]) in
          let s = Text.get current_tw ~start:(index,[`Line(-1);`Linestart])
              ~stop:(index,[`Line(-1);`Lineend]) in
          for i = 0 to String.length s - 1 do
            match s.[i] with '\t'|' ' -> () | _ -> raise Not_found
          done;
          Text.yview_index current_tw ~index:(index,[`Line(-1)])
        with _ ->
          Text.yview_index current_tw ~index:(tpos pos ~modi:[`Line(-2)])

  initializer
    (* Create a first window *)
    self#new_window (current_dir ^ "/untitled");

    (* Bindings for the main window *)
    List.iter
      [ [`Control], "s", (fun () -> Jg_text.search_string current_tw);
        [`Control], "g", (fun () -> goto_line current_tw);
        [`Alt], "s", self#save_file;
        [`Alt], "x", (fun () -> send_phrase (List.hd windows));
        [`Alt], "l", self#lex;
        [`Alt], "t", self#typecheck ]
      ~f:begin fun (modi,key,act) ->
        bind top ~events:[`Modified(modi, `KeyPressDetail key)] ~breakable:true
          ~action:(fun _ -> act (); break ())
      end;

    bind top ~events:[`Destroy] ~fields:[`Widget] ~action:
      begin fun ev ->
        if Widget.name ev.ev_Widget = Widget.name top
        then self#quit ~cancel:false ()
      end;

    (* File menu *)
    file_menu#add_command "Open File..." ~command:self#open_file;
    file_menu#add_command "Reopen"
      ~command:(fun () -> self#load_text [(List.hd windows).name]);
    file_menu#add_command "Save File" ~command:self#save_file ~accelerator:"M-s";
    file_menu#add_command "Save As..." ~underline:5 ~command:
      begin fun () ->
        let txt = List.hd windows in
        Fileselect.f ~title:"Save as File"
          ~action:(fun name -> self#save_text txt ~name)
          ~dir:(Filename.dirname txt.name)
          ~filter:"*.{ml,mli}"
          ~file:(Filename.basename txt.name)
          ~sync:true ~usepath:false ()
      end;
    file_menu#add_command "Close File" ~command:self#close_file;
    file_menu#add_command "Close Window" ~command:self#quit ~underline:6;

    (* Edit menu *)
    edit_menu#add_command "Paste selection" ~command:
      begin fun () ->
        Text.insert current_tw ~index:(`Mark"insert",[])
          ~text:(Selection.get ~displayof:top ())
      end;
    edit_menu#add_command "Goto..." ~accelerator:"C-g"
      ~command:(fun () -> goto_line current_tw);
    edit_menu#add_command "Search..." ~accelerator:"C-s"
      ~command:(fun () -> Jg_text.search_string current_tw);
    edit_menu#add_command "To shell" ~accelerator:"M-x"
      ~command:(fun () -> send_phrase (List.hd windows));
    edit_menu#add_command "Select shell..."
      ~command:(fun () -> select_shell (List.hd windows));

    (* Compiler menu *)
    compiler_menu#add_command "Preferences..."
      ~command:(fun () -> compiler_preferences top);
    compiler_menu#add_command "Lex" ~accelerator:"M-l"
      ~command:self#lex;
    compiler_menu#add_command "Typecheck" ~accelerator:"M-t"
      ~command:self#typecheck;
    compiler_menu#add_command "Clear errors"
      ~command:self#clear_errors;
    compiler_menu#add_command "Signature..." ~command:
      begin fun () ->
        let txt = List.hd windows in if txt.signature <> [] then
          let basename = Filename.basename txt.name in
          let modname = String.capitalize
              (try Filename.chop_extension basename with _ -> basename) in
          let env =
            Env.add_module (Ident.create modname)
              (Types.Tmty_signature txt.signature)
              Env.initial
          in Viewer.view_defined (Longident.Lident modname) ~env ~show_all:true
      end;

    (* Modules *)
    module_menu#add_command "Path editor..."
      ~command:(fun () -> Setpath.set ~dir:current_dir);
    module_menu#add_command "Reset cache"
      ~command:(fun () -> Setpath.exec_update_hooks (); Env.reset_cache ());
    module_menu#add_command "Search symbol..."
      ~command:Viewer.search_symbol;
    module_menu#add_command "Close all"
      ~command:Viewer.close_all_views;
end

(* The main function starts here ! *)

let already_open : editor list ref = ref []

let editor ?file ?(pos=0) ?(reuse=false) () =

  if !already_open <> [] &&
    let ed = List.hd !already_open
    (*  try
        let name = match file with Some f -> f | None -> raise Not_found in
        List.find !already_open ~f:(fun ed -> ed#has_window name)
      with Not_found -> List.hd !already_open *)
    in try
      ed#reopen ~file ~pos;
      true
    with Protocol.TkError _ ->
      already_open := [] (* List.filter !already_open ~f:((<>) ed) *);
      false
  then () else
    let top = Jg_toplevel.titled "OCamlBrowser Editor" in
    let menus = Jg_menu.menubar top in
    let ed = new editor ~top ~menus in
    already_open := !already_open @ [ed];
    if file <> None then ed#reopen ~file ~pos

let f ?file ?pos ?(opendialog=false) () =
  if opendialog then
    Fileselect.f ~title:"Open File"
      ~action:(function [file] -> editor ~file () | _ -> ())
      ~filter:("*.{ml,mli}") ~sync:true ()
  else editor ?file ?pos ~reuse:(file <> None) ()
