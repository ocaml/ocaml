(* $Id$ *)

open Tk
open Jg_tk
open Mytypes
open Longident
open Types
open Typedtree
open Env
open Searchpos
open Searchid

let list_modules :path =
  List.fold_left path acc:[] fun:
  begin fun :acc dir ->
    let l = 
      List.filter (Useunix.get_files_in_directory dir)
        pred:(fun x -> Filename.check_suffix x suff:".cmi") in
    let l = List.map l fun:
    begin fun x ->
      String.capitalize (Filename.chop_suffix x suff:".cmi")
    end in
    List.fold_left l :acc
     fun:(fun :acc elt -> if List.mem acc :elt then acc else elt :: acc)
  end

let reset_modules box =
  Listbox.delete box first:(`Num 0) last:`End;
  module_list := Sort.list order:(<) (list_modules path:!Config.load_path);
  Listbox.insert box index:`End texts:!module_list;
  Jg_box.recenter box index:(`Num 0)

let view_symbol :kind :env ?:path id =
  let name = match id with
      Lident x -> x
    | Ldot (_, x) -> x
    | _ -> match kind with Pvalue | Ptype | Plabel -> "z" | _ -> "Z"
  in
  match kind with
    Pvalue ->
      let path, vd = lookup_value id env in
      view_signature_item :path :env [Tsig_value (Ident.create name, vd)]
  | Ptype -> view_type_id id :env
  | Plabel -> let ld = lookup_label id env in
      begin match ld.lbl_res.desc with
        Tconstr (path, _, _) -> view_type_decl path :env
      | _ -> ()
      end
  | Pconstructor ->
      let cd = lookup_constructor id env in
      begin match cd.cstr_res.desc with
      	Tconstr (cpath, _, _) ->
	if Path.same cpath Predef.path_exn then
	  view_signature title:(string_of_longident id) :env ?:path
	    [Tsig_exception (Ident.create name, cd.cstr_args)]
        else
	  view_type_decl cpath :env
      | _ -> ()
      end
  | Pmodule -> view_module_id id :env
  | Pmodtype -> view_modtype_id id :env
  | Pclass -> view_class_id id :env
  | Pcltype -> view_cltype_id id :env

let choose_symbol :title :env ?:signature ?:path l =
  if match path with
    None -> false
  | Some path ->
      try find_shown_module path; true with Not_found -> false
  then () else
  let tl = Jg_toplevel.titled title in
  Jg_bind.escape_destroy tl;
  top_widgets := coe tl :: !top_widgets;
  let buttons = Frame.create parent:tl () in
  let all = Button.create parent:buttons text:"Show all" padx:(`Pix 20) ()
  and ok = Jg_button.create_destroyer tl parent:buttons
  and detach = Button.create parent:buttons text:"Detach" ()
  and edit = Button.create parent:buttons text:"Impl" ()
  and intf = Button.create parent:buttons text:"Intf" () in
  let l = Sort.list l order:
      (fun (li1, _) (li2,_) ->
	string_of_longident li1 < string_of_longident li2)
  in
  let nl = List.map l fun:
    begin fun (li, k) ->
      string_of_longident li ^ " (" ^ string_of_kind k ^ ")"
    end in
  let fb = Frame.create parent:tl () in
  let box =
    new Jg_multibox.c parent:fb cols:3 texts:nl maxheight:3 width:21 () in
  box#init;
  box#bind_kbd events:[[],`KeyPressDetail"Escape"]
    action:(fun _ :index -> destroy tl; break ());
  if List.length nl > 9 then (Jg_multibox.add_scrollbar box; ());
  Jg_multibox.add_completion box action:
    begin fun pos ->
      let li, k = List.nth l :pos in
      let path =
	match path, li with
	  None, Ldot (lip, _) ->
	    begin try
	      Some (fst (lookup_module lip env))
	    with Not_found -> None
	    end
	| _ -> path
      in view_symbol li kind:k :env ?:path
    end;
  pack [buttons] side:`Bottom fill:`X;
  pack [fb] side:`Top fill:`Both expand:true;
  begin match signature with
    None -> pack [ok] fill:`X expand:true
  | Some signature ->
      Button.configure all command:
        begin fun () ->
          view_signature signature :title :env ?:path
        end;
      pack [ok; all] side:`Right fill:`X expand:true
  end;
  begin match path with None -> ()
  | Some path ->
      let frame = Frame.create parent:tl () in
      pack [frame] side:`Bottom fill:`X;
      add_shown_module path
	widgets:{ mw_frame = frame; mw_detach = detach;
		  mw_edit = edit; mw_intf = intf }
  end

let search_which = ref "itself"

let search_symbol () =
  if !module_list = [] then
  module_list := Sort.list order:(<) (list_modules path:!Config.load_path);
  let tl = Jg_toplevel.titled "Search symbol" in
  Jg_bind.escape_destroy tl;
  let ew = Entry.create parent:tl width:30 () in
  let choice = Frame.create parent:tl ()
  and which = Textvariable.create on:tl () in
  let itself = Radiobutton.create parent:choice text:"Itself"
        variable:which value:"itself" ()
  and extype = Radiobutton.create parent:choice text:"Exact type"
        variable:which value:"exact" ()
  and iotype = Radiobutton.create parent:choice text:"Included type"
      	variable:which value:"iotype" ()
  and buttons = Frame.create parent:tl () in
  let search = Button.create parent:buttons text:"Search" () command:
    begin fun () ->
      search_which := Textvariable.get which;
      let text = Entry.get ew in
      try if text = "" then () else      
        let l = match !search_which with
                "itself" -> search_string_symbol text
      	      | "iotype" -> search_string_type text mode:`included
	      | "exact" -> search_string_type text mode:`exact
        in
        if l <> [] then
        choose_symbol title:"Choose symbol" env:!start_env l
      with Searchid.Error (s,e) ->
      	Entry.selection_clear ew;
	Entry.selection_range ew start:(`Num s) end:(`Num e);
	Entry.xview_index ew index:(`Num s)
    end
  and ok = Jg_button.create_destroyer tl parent:buttons text:"Cancel" in

  Focus.set ew;
  Jg_bind.return_invoke ew button:search;
  Textvariable.set which to:!search_which;
  pack [itself; extype; iotype] side:`Left anchor:`W;
  pack [search; ok] side:`Left fill:`X expand:true;
  pack [coe ew; coe choice; coe buttons]
       side:`Top fill:`X expand:true

let view_defined modlid :env =
  try match lookup_module modlid env with
    path, Tmty_signature sign ->
    let ident_of_decl = function
      	Tsig_value (id, _) -> Lident (Ident.name id), Pvalue
      | Tsig_type (id, _) -> Lident (Ident.name id), Ptype
      | Tsig_exception (id, _) -> Ldot (modlid, Ident.name id), Pconstructor
      | Tsig_module (id, _) -> Lident (Ident.name id), Pmodule
      | Tsig_modtype (id, _) -> Lident (Ident.name id), Pmodtype
      | Tsig_class (id, _) -> Lident (Ident.name id), Pclass
      | Tsig_cltype (id, _) -> Lident (Ident.name id), Pcltype
    in
    let rec iter_sign sign idents =
      match sign with
	[] -> List.rev idents
      |	decl :: rem ->
	  let rem = match decl, rem with
	    Tsig_class _, cty :: ty1 :: ty2 :: rem -> rem
	  | Tsig_cltype _, ty1 :: ty2 :: rem -> rem
	  | _, rem -> rem
	  in iter_sign rem (ident_of_decl decl :: idents)
    in
    let l = iter_sign sign [] in
    choose_symbol l title:(string_of_path path) signature:sign
       env:(open_signature path sign env) :path
  | _ -> ()
  with Not_found -> ()
  | Env.Error err ->
      let tl, tw, finish = Jg_message.formatted title:"Error!" () in
      Env.report_error err;
      finish ()

let close_all_views () =
    List.iter !top_widgets
      fun:(fun tl -> try destroy tl with Protocol.TkError _ -> ());
    top_widgets := []


let shell_counter = ref 1
let default_shell = ref "ocaml"

let start_shell () =
  let tl = Jg_toplevel.titled "Start New Shell" in
  Wm.transient_set tl master:Widget.default_toplevel;
  let input = Frame.create parent:tl ()
  and buttons = Frame.create parent:tl () in
  let ok = Button.create parent:buttons text:"Ok" ()
  and cancel = Jg_button.create_destroyer tl parent:buttons text:"Cancel"
  and labels = Frame.create parent:input ()
  and entries = Frame.create parent:input () in
  let l1 = Label.create parent:labels text:"Command:" ()
  and l2 = Label.create parent:labels text:"Title:" ()
  and e1 =
    Jg_entry.create parent:entries command:(fun _ -> Button.invoke ok) ()
  and e2 =
    Jg_entry.create parent:entries command:(fun _ -> Button.invoke ok) ()
  and names = List.map fun:fst (Shell.get_all ()) in
  Entry.insert e1 index:`End text:!default_shell;
  while List.mem names elt:("Shell #" ^ string_of_int !shell_counter) do
    incr shell_counter
  done;
  Entry.insert e2 index:`End text:("Shell #" ^ string_of_int !shell_counter);
  Button.configure ok command:(fun () ->
      if not (List.mem names elt:(Entry.get e2)) then begin
	default_shell := Entry.get e1;
      	Shell.f prog:!default_shell title:(Entry.get e2);
      	destroy tl
      end);
  pack [l1;l2] side:`Top anchor:`W;
  pack [e1;e2] side:`Top fill:`X expand:true;
  pack [labels;entries] side:`Left fill:`X expand:true;
  pack [ok;cancel] side:`Left fill:`X expand:true;
  pack [input;buttons] side:`Top fill:`X expand:true

let f ?:dir{= Unix.getcwd()} ?:on () =
  let tl = match on with
    None ->
      let tl = Jg_toplevel.titled "Module viewer" in
      Jg_bind.escape_destroy tl; coe tl
  | Some top ->
      Wm.title_set top title:"LablBrowser";
      Wm.iconname_set top name:"LablBrowser";
      let tl = Frame.create parent:top () in
      pack [tl] expand:true fill:`Both;
      coe tl
  in
  let menus = Frame.create parent:tl name:"menubar" () in
  let filemenu = new Jg_menu.c "File" parent:menus
  and modmenu = new Jg_menu.c "Modules" parent:menus in
  let fmbox, mbox, msb = Jg_box.create_with_scrollbar parent:tl () in

  Jg_box.add_completion mbox nocase:true action:
    begin fun index ->
      view_defined (Lident (Listbox.get mbox :index)) env:!start_env
    end;
  Setpath.add_update_hook (fun () -> reset_modules mbox);

  let ew = Entry.create parent:tl () in
  let buttons = Frame.create parent:tl () in
  let search = Button.create parent:buttons text:"Search" pady:(`Pix 1) ()
    command:
    begin fun () ->
      let s = Entry.get ew in
      let is_type = ref false and is_long = ref false in
      for i = 0 to String.length s - 2 do
      	if s.[i] = '-' & s.[i+1] = '>' then is_type := true;
	if s.[i] = '.' then is_long := true
      done;
      let l =
 	if !is_type then try
	  search_string_type mode:`included s
	with Searchid.Error (start,stop) ->
	  Entry.icursor ew index:(`Num start); []
 	else if !is_long then
	  search_string_symbol s
 	else
	  search_pattern_symbol s in
      match l with [] -> ()
      |	[lid,kind] when !is_long -> view_symbol lid :kind env:!start_env
      |	_ -> choose_symbol title:"Choose symbol" env:!start_env l
    end
  and close =
    Button.create parent:buttons text:"Close all" pady:(`Pix 1) ()
                  command:close_all_views
  in
  (* bindings *)
  Jg_bind.enter_focus ew;
  Jg_bind.return_invoke ew button:search;
  bind close events:[[`Double], `ButtonPressDetail 1]
    action:(`Set ([], fun _ -> destroy tl));

  (* File menu *)
  filemenu#add_command "Open..."
    command:(fun () -> !editor_ref opendialog:true ());
  filemenu#add_command "Editor..." command:(fun () -> !editor_ref ());
  filemenu#add_command "Shell..." command:start_shell;
  filemenu#add_command "Quit" command:(fun () -> destroy tl);

  (* modules menu *)
  modmenu#add_command "Path editor..." command:(fun () -> Setpath.f :dir; ());
  modmenu#add_command "Reset cache"
    command:(fun () -> reset_modules mbox; Env.reset_cache ());
  modmenu#add_command "Search symbol..." command:search_symbol;

  pack [filemenu#button; modmenu#button] side:`Left ipadx:(`Pix 5) anchor:`W;
  pack [menus] side:`Top fill:`X;      
  pack [close; search] fill:`X side:`Right expand:true;
  pack [coe buttons; coe ew] fill:`X side:`Bottom;
  pack [msb] side:`Right fill:`Y;
  pack [mbox] side:`Left fill:`Both expand:true;
  pack [fmbox] fill:`Both expand:true side:`Top;
  reset_modules mbox
