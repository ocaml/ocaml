(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

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

(* Managing the module list *)

let list_modules ~path =
  List.fold_left path ~init:[] ~f:
  begin fun modules dir ->
    let l = 
      List.filter (Useunix.get_files_in_directory dir)
        ~f:(fun x -> Filename.check_suffix x ".cmi") in
    let l = List.map l ~f:
    begin fun x ->
      String.capitalize (Filename.chop_suffix x ".cmi")
    end in
    List.fold_left l ~init:modules
     ~f:(fun modules item ->
          if List.mem item modules then modules else item :: modules)
  end

let reset_modules box =
  Listbox.delete box ~first:(`Num 0) ~last:`End;
  module_list := Sort.list ~order:(Jg_completion.lt_string ~nocase:true)
      (list_modules ~path:!Config.load_path);
  Listbox.insert box ~index:`End ~texts:!module_list;
  Jg_box.recenter box ~index:(`Num 0)


(* How to display a symbol *)

let view_symbol ~kind ~env ?path id =
  let name = match id with
      Lident x -> x
    | Ldot (_, x) -> x
    | _ -> match kind with Pvalue | Ptype | Plabel -> "z" | _ -> "Z"
  in
  match kind with
    Pvalue ->
      let path, vd = lookup_value id env in
      view_signature_item ~path ~env [Tsig_value (Ident.create name, vd)]
  | Ptype -> view_type_id id ~env
  | Plabel -> let ld = lookup_label id env in
      begin match ld.lbl_res.desc with
        Tconstr (path, _, _) -> view_type_decl path ~env
      | _ -> ()
      end
  | Pconstructor ->
      let cd = lookup_constructor id env in
      begin match cd.cstr_res.desc with
        Tconstr (cpath, _, _) ->
        if Path.same cpath Predef.path_exn then
          view_signature ~title:(string_of_longident id) ~env ?path
            [Tsig_exception (Ident.create name, cd.cstr_args)]
        else
          view_type_decl cpath ~env
      | _ -> ()
      end
  | Pmodule -> view_module_id id ~env
  | Pmodtype -> view_modtype_id id ~env
  | Pclass -> view_class_id id ~env
  | Pcltype -> view_cltype_id id ~env


(* Create a list of symbols you can choose from *)

let choose_symbol ~title ~env ?signature ?path l =
  if match path with
    None -> false
  | Some path -> is_shown_module path
  then () else
  let tl = Jg_toplevel.titled title in
  Jg_bind.escape_destroy tl;
  top_widgets := coe tl :: !top_widgets;
  let buttons = Frame.create tl in
  let all = Button.create buttons ~text:"Show all" ~padx:20
  and ok = Jg_button.create_destroyer tl ~parent:buttons
  and detach = Button.create buttons ~text:"Detach"
  and edit = Button.create buttons ~text:"Impl"
  and intf = Button.create buttons ~text:"Intf" in
  let l = Sort.list l ~order:
      (fun (li1, _) (li2,_) ->
        string_of_longident li1 < string_of_longident li2)
  in
  let nl = List.map l ~f:
    begin fun (li, k) ->
      string_of_longident li ^ " (" ^ string_of_kind k ^ ")"
    end in
  let fb = Frame.create tl in
  let box =
    new Jg_multibox.c fb ~cols:3 ~texts:nl ~maxheight:3 ~width:21 in
  box#init;
  box#bind_kbd ~events:[`KeyPressDetail"Escape"]
    ~action:(fun _ ~index -> destroy tl; break ());
  if List.length nl > 9 then ignore (Jg_multibox.add_scrollbar box);
  Jg_multibox.add_completion box ~action:
    begin fun pos ->
      let li, k = List.nth l pos in
      let path =
        match path, li with
          None, Ldot (lip, _) ->
            begin try
              Some (fst (lookup_module lip env))
            with Not_found -> None
            end
        | _ -> path
      in view_symbol li ~kind:k ~env ?path
    end;
  pack [buttons] ~side:`Bottom ~fill:`X;
  pack [fb] ~side:`Top ~fill:`Both ~expand:true;
  begin match signature with
    None -> pack [ok] ~fill:`X ~expand:true
  | Some signature ->
      Button.configure all ~command:
        begin fun () ->
          view_signature signature ~title ~env ?path
        end;
      pack [ok; all] ~side:`Right ~fill:`X ~expand:true
  end;
  begin match path with None -> ()
  | Some path ->
      let frame = Frame.create tl in
      pack [frame] ~side:`Bottom ~fill:`X;
      add_shown_module path
        ~widgets:{ mw_frame = frame; mw_detach = detach;
                   mw_edit = edit; mw_intf = intf }
  end

let choose_symbol_ref = ref choose_symbol


(* Search, both by type and name *)

let guess_search_mode s : [`Type | `Long | `Pattern] =
  let is_type = ref false and is_long = ref false in
  for i = 0 to String.length s - 2 do
    if s.[i] = '-' && s.[i+1] = '>' then is_type := true;
    if s.[i] = '.' then is_long := true
  done;
  if !is_type then `Type else if !is_long then `Long else `Pattern

let search_which = ref "itself"

let search_symbol () =
  if !module_list = [] then
  module_list := Sort.list ~order:(<) (list_modules ~path:!Config.load_path);
  let tl = Jg_toplevel.titled "Search symbol" in
  Jg_bind.escape_destroy tl;
  let ew = Entry.create tl ~width:30 in
  let choice = Frame.create tl
  and which = Textvariable.create ~on:tl () in
  let itself = Radiobutton.create choice ~text:"Itself"
        ~variable:which ~value:"itself"
  and extype = Radiobutton.create choice ~text:"Exact type"
        ~variable:which ~value:"exact"
  and iotype = Radiobutton.create choice ~text:"Included type"
        ~variable:which ~value:"iotype"
  and buttons = Frame.create tl in
  let search = Button.create buttons ~text:"Search" ~command:
    begin fun () ->
      search_which := Textvariable.get which;
      let text = Entry.get ew in
      try if text = "" then () else      
        let l =
          match !search_which with
            "itself" ->
              begin match guess_search_mode text with
                `Long -> search_string_symbol text
              | `Pattern -> search_pattern_symbol text
              | `Type -> search_string_type text ~mode:`included
              end
          | "iotype" -> search_string_type text ~mode:`included
          | "exact" -> search_string_type text ~mode:`exact
          | _ -> assert false
        in
        if l <> [] then
        choose_symbol ~title:"Choose symbol" ~env:!start_env l
      with Searchid.Error (s,e) ->
        Entry.selection_clear ew;
        Entry.selection_range ew ~start:(`Num s) ~stop:(`Num e);
        Entry.xview_index ew ~index:(`Num s)
    end
  and ok = Jg_button.create_destroyer tl ~parent:buttons ~text:"Cancel" in

  Focus.set ew;
  Jg_bind.return_invoke ew ~button:search;
  Textvariable.set which !search_which;
  pack [itself; extype; iotype] ~side:`Left ~anchor:`W;
  pack [search; ok] ~side:`Left ~fill:`X ~expand:true;
  pack [coe ew; coe choice; coe buttons]
       ~side:`Top ~fill:`X ~expand:true


(* Display the contents of a module *)

let view_defined modlid ~env =
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
      | decl :: rem ->
          let rem = match decl, rem with
            Tsig_class _, cty :: ty1 :: ty2 :: rem -> rem
          | Tsig_cltype _, ty1 :: ty2 :: rem -> rem
          | _, rem -> rem
          in iter_sign rem (ident_of_decl decl :: idents)
    in
    let l = iter_sign sign [] in
    !choose_symbol_ref l ~title:(string_of_path path) ~signature:sign
       ~env:(open_signature path sign env) ~path
  | _ -> ()
  with Not_found -> ()
  | Env.Error err ->
      let tl, tw, finish = Jg_message.formatted ~title:"Error!" () in
      Env.report_error Format.std_formatter err;
      finish ()


(* Manage toplevel windows *)

let close_all_views () =
    List.iter !top_widgets
      ~f:(fun tl -> try destroy tl with Protocol.TkError _ -> ());
    top_widgets := []


(* Launch a shell *)

let shell_counter = ref 1
let default_shell = ref "ocaml"

let start_shell () =
  let tl = Jg_toplevel.titled "Start New Shell" in
  Wm.transient_set tl ~master:Widget.default_toplevel;
  let input = Frame.create tl
  and buttons = Frame.create tl in
  let ok = Button.create buttons ~text:"Ok"
  and cancel = Jg_button.create_destroyer tl ~parent:buttons ~text:"Cancel"
  and labels = Frame.create input
  and entries = Frame.create input in
  let l1 = Label.create labels ~text:"Command:"
  and l2 = Label.create labels ~text:"Title:"
  and e1 =
    Jg_entry.create entries ~command:(fun _ -> Button.invoke ok)
  and e2 =
    Jg_entry.create entries ~command:(fun _ -> Button.invoke ok)
  and names = List.map ~f:fst (Shell.get_all ()) in
  Entry.insert e1 ~index:`End ~text:!default_shell;
  let shell_name () = "Shell #" ^ string_of_int !shell_counter in
  while List.mem (shell_name ()) names do
    incr shell_counter
  done;
  Entry.insert e2 ~index:`End ~text:(shell_name ());
  Button.configure ok ~command:(fun () ->
      if not (List.mem (Entry.get e2) names) then begin
        default_shell := Entry.get e1;
        Shell.f ~prog:!default_shell ~title:(Entry.get e2);
        destroy tl
      end);
  pack [l1;l2] ~side:`Top ~anchor:`W;
  pack [e1;e2] ~side:`Top ~fill:`X ~expand:true;
  pack [labels;entries] ~side:`Left ~fill:`X ~expand:true;
  pack [ok;cancel] ~side:`Left ~fill:`X ~expand:true;
  pack [input;buttons] ~side:`Top ~fill:`X ~expand:true


(* Help window *)

let show_help () =
  let tl = Jg_toplevel.titled "OCamlBrowser Help" in
  Jg_bind.escape_destroy tl;
  let fw, tw, sb = Jg_text.create_with_scrollbar tl in
  let ok = Jg_button.create_destroyer ~parent:tl ~text:"Ok" tl in
  Text.insert tw ~index:tend ~text:Help.text;
  pack [tw] ~side:`Left ~fill:`Both ~expand:true;
  pack [sb] ~side:`Right ~fill:`Y;
  pack [fw] ~side:`Top ~expand:true ~fill:`Both;
  pack [ok] ~side:`Bottom ~fill:`X

(* Launch the classical viewer *)

let f ?(dir=Unix.getcwd()) ?on () =
  let tl = match on with
    None ->
      let tl = Jg_toplevel.titled "Module viewer" in
      ignore (Jg_bind.escape_destroy tl); coe tl
  | Some top ->
      Wm.title_set top "OCamlBrowser";
      Wm.iconname_set top "OCamlBrowser";
      let tl = Frame.create top in
      bind tl ~events:[`Destroy] ~action:(fun _ -> exit 0);
      pack [tl] ~expand:true ~fill:`Both;
      coe tl
  in
  let menus = Frame.create tl ~name:"menubar" in
  let filemenu = new Jg_menu.c "File" ~parent:menus
  and modmenu = new Jg_menu.c "Modules" ~parent:menus in
  let fmbox, mbox, msb = Jg_box.create_with_scrollbar tl in

  Jg_box.add_completion mbox ~nocase:true ~action:
    begin fun index ->
      view_defined (Lident (Listbox.get mbox ~index)) ~env:!start_env
    end;
  Setpath.add_update_hook (fun () -> reset_modules mbox);

  let ew = Entry.create tl in
  let buttons = Frame.create tl in
  let search = Button.create buttons ~text:"Search" ~pady:1 ~command:
    begin fun () ->
      let s = Entry.get ew in
      let mode = guess_search_mode s in
      let l =
        match mode with
        | `Long -> search_string_symbol s
        | `Pattern ->  search_pattern_symbol s
        | `Type ->
            try  search_string_type ~mode:`included s
            with Searchid.Error (start,stop) ->
              Entry.icursor ew ~index:(`Num start); []
      in
      match l with [] -> ()
      | [lid,kind] when mode = `Long ->
          view_symbol lid ~kind ~env:!start_env
      | _ -> choose_symbol ~title:"Choose symbol" ~env:!start_env l
    end
  and close =
    Button.create buttons ~text:"Close all" ~pady:1 ~command:close_all_views
  in
  (* bindings *)
  Jg_bind.enter_focus ew;
  Jg_bind.return_invoke ew ~button:search;
  bind close ~events:[`Modified([`Double], `ButtonPressDetail 1)]
    ~action:(fun _ -> destroy tl);

  (* File menu *)
  filemenu#add_command "Open..."
    ~command:(fun () -> !editor_ref ~opendialog:true ());
  filemenu#add_command "Editor..." ~command:(fun () -> !editor_ref ());
  filemenu#add_command "Shell..." ~command:start_shell;
  filemenu#add_command "Quit" ~command:(fun () -> destroy tl);

  (* modules menu *)
  modmenu#add_command "Path editor..."
    ~command:(fun () -> Setpath.set ~dir);
  modmenu#add_command "Reset cache"
    ~command:(fun () -> reset_modules mbox; Env.reset_cache ());
  modmenu#add_command "Search symbol..." ~command:search_symbol;

  pack [filemenu#button; modmenu#button] ~side:`Left ~ipadx:5 ~anchor:`W;
  pack [menus] ~side:`Top ~fill:`X;      
  pack [close; search] ~fill:`X ~side:`Right ~expand:true;
  pack [coe buttons; coe ew] ~fill:`X ~side:`Bottom;
  pack [msb] ~side:`Right ~fill:`Y;
  pack [mbox] ~side:`Left ~fill:`Both ~expand:true;
  pack [fmbox] ~fill:`Both ~expand:true ~side:`Top;
  reset_modules mbox

(* Smalltalk-like version *)

class st_viewer ?(dir=Unix.getcwd()) ?on () =
  let tl = match on with
    None ->
      let tl = Jg_toplevel.titled "Module viewer" in
      ignore (Jg_bind.escape_destroy tl); coe tl
  | Some top ->
      Wm.title_set top "OCamlBrowser";
      Wm.iconname_set top "OCamlBrowser";
      let tl = Frame.create top in
      bind tl ~events:[`Destroy] ~action:(fun _ -> exit 0);
      pack [tl] ~expand:true ~fill:`Both;
      coe tl
  in
  let menus = Frame.create tl ~name:"menubar" in
  let filemenu = new Jg_menu.c "File" ~parent:menus
  and modmenu = new Jg_menu.c "Modules" ~parent:menus
  and helpmenu = new Jg_menu.c "Help" ~parent:menus in
  let boxes_frame = Frame.create tl ~name:"boxes" in
  let view = Frame.create tl in
  let buttons = Frame.create tl in
  let all = Button.create buttons ~text:"Show all" ~padx:20
  and close = Button.create buttons ~text:"Close all" ~command:close_all_views
  and detach = Button.create buttons ~text:"Detach"
  and edit = Button.create buttons ~text:"Impl"
  and intf = Button.create buttons ~text:"Intf" in
object (self)
  val mutable boxes = []

  method create_box =
    let fmbox, mbox, sb = Jg_box.create_with_scrollbar boxes_frame in
    boxes <- boxes @ [fmbox, mbox];
    pack [sb] ~side:`Right ~fill:`Y;
    pack [mbox] ~side:`Left ~fill:`Both ~expand:true;
    pack [fmbox] ~side:`Left ~fill:`Both ~expand:true;
    fmbox, mbox

  initializer
    (* Boxes *)
    let fmbox, mbox = self#create_box in
    Jg_box.add_completion mbox ~nocase:true ~double:false ~action:
      begin fun index ->
        view_defined (Lident (Listbox.get mbox ~index)) ~env:!start_env
      end;
    Setpath.add_update_hook (fun () -> reset_modules mbox; self#hide_after 1);
    List.iter [1;2] ~f:(fun _ -> ignore self#create_box);
    Searchpos.default_frame := Some
      { mw_frame = view; mw_detach = detach; mw_edit = edit; mw_intf = intf };

    (* Buttons *)
    pack [close] ~side:`Right ~fill:`X ~expand:true;
    bind close ~events:[`Modified([`Double], `ButtonPressDetail 1)]
      ~action:(fun _ -> destroy tl);

    (* File menu *)
    filemenu#add_command "Open..."
      ~command:(fun () -> !editor_ref ~opendialog:true ());
    filemenu#add_command "Editor..." ~command:(fun () -> !editor_ref ());
    filemenu#add_command "Shell..." ~command:start_shell;
    filemenu#add_command "Quit" ~command:(fun () -> destroy tl);

    (* modules menu *)
    modmenu#add_command "Path editor..."
      ~command:(fun () -> Setpath.set ~dir);
    modmenu#add_command "Reset cache"
      ~command:(fun () -> reset_modules mbox; Env.reset_cache ());
    modmenu#add_command "Search symbol..." ~command:search_symbol;

    (* Help menu *)
    helpmenu#add_command "Manual..." ~command:show_help;

    pack [filemenu#button; modmenu#button] ~side:`Left ~ipadx:5 ~anchor:`W;
    pack [helpmenu#button] ~side:`Right ~anchor:`E ~ipadx:5;
    pack [menus] ~side:`Top ~fill:`X;      
    (* pack [close; search] ~fill:`X ~side:`Right ~expand:true; *)
    pack [boxes_frame] ~fill:`Both ~expand:true;
    pack [view] ~fill:`X ~expand:false;
    pack [buttons] ~fill:`X ~side:`Bottom ~expand:false;
    reset_modules mbox

  val mutable shown_paths = []

  method hide_after n =
    for i = n to List.length boxes - 1 do
      let fm, box = List.nth boxes i in
      if i < 3 then Listbox.delete box ~first:(`Num 0) ~last:`End
      else destroy fm
    done;
    let rec firsts n = function [] -> []
      | a :: l -> if n > 0 then a :: firsts (pred n) l else [] in
    shown_paths <- firsts (n-1) shown_paths;
    boxes <- firsts (max 3 n) boxes

  method get_box ~path =
    let rec path_index p = function
        [] -> raise Not_found
      | a :: l -> if Path.same p a then 1 else path_index p l + 1 in
    try
      let n = path_index path shown_paths in
      self#hide_after (n+1);
      n
    with Not_found ->
      match path with
        Path.Pdot (path', _, _) ->
          let n = self#get_box ~path:path' in
          shown_paths <- shown_paths @ [path];
          if n + 1 >= List.length boxes then ignore self#create_box;
          n+1
      | _ ->
          self#hide_after 2;
          shown_paths <- [path];
          1
        
  method choose_symbol ~title ~env ?signature ?path l =
    let n =
      match path with None -> 1
      | Some path -> self#get_box ~path
    in

    let l = Sort.list l ~order:
        (fun (li1, _) (li2,_) ->
          string_of_longident li1 < string_of_longident li2)
    in
    let nl = List.map l ~f:
        begin fun (li, k) ->
          string_of_longident li ^ " (" ^ string_of_kind k ^ ")"
        end in
    let _, box = List.nth boxes n in
    Listbox.delete box ~first:(`Num 0) ~last:`End;
    Listbox.insert box ~index:`End ~texts:nl;
    Jg_box.add_completion box ~double:false ~action:
      begin fun index ->
        let `Num pos = Listbox.index box ~index in
        let li, k = List.nth l pos in
        let path =
          match path, li with
            None, Ldot (lip, _) ->
              begin try
                Some (fst (lookup_module lip env))
              with Not_found -> None
              end
          | _ -> path
        in
        view_symbol li ~kind:k ~env ?path;
      end;
    begin match signature with
      None -> ()
    | Some signature ->
        Button.configure all ~command:
          begin fun () ->
            view_signature signature ~title ~env ?path
          end;
        pack [all] ~side:`Right ~fill:`X ~expand:true
    end
end

let st_viewer ?dir ?on () =
  let viewer = new st_viewer ?dir ?on () in
  choose_symbol_ref := viewer#choose_symbol
