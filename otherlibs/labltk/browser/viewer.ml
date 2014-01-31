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
  module_list := Sort.list (Jg_completion.lt_string ~nocase:true)
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
      view_signature_item ~path ~env [Sig_value (Ident.create name, vd)]
  | Ptype -> view_type_id id ~env
  | Plabel -> let ld = lookup_label id env in
      begin match ld.lbl_res.desc with
        Tconstr (path, _, _) -> view_type_decl path ~env
      | _ -> ()
      end
  | Pconstructor ->
      let cd = lookup_constructor id env in
      begin match cd.cstr_res.desc with
        Tconstr (cpath, cargs, _) -> begin
          match cd.cstr_tag with
              Cstr_exception _ ->
                let exn =
                  { Types.exn_loc = Location.none;
                    exn_args = cd.cstr_args }
                in
                  view_signature ~title:(string_of_longident id) ~env ?path
                    [Sig_exception (Ident.create name, exn)]
            | Cstr_ext_constant _ | Cstr_ext_block _ ->
                let ext =
                  { Types.ext_loc = Location.none;
                    ext_type_path = cpath;
                    ext_type_params =
                      if cd.cstr_generalized then List.map (fun _ -> Ctype.newvar ()) cargs
                      else cargs;
                    ext_args = cd.cstr_args;
                    ext_ret_type = if cd.cstr_generalized then Some cd.cstr_res else None;
                    ext_private = cd.cstr_private }
                in
                  view_signature ~title:(string_of_longident id) ~env ?path
                    [Sig_extension (Ident.create name, ext, Text_first)]
            | Cstr_constant _ | Cstr_block _ ->
                view_type_decl cpath ~env
        end
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
  let l = List.sort l ~cmp:(fun (li1, _) (li2,_) -> compare li1 li2) in
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
        ~widgets:{ mw_frame = frame; mw_title = None; mw_detach = detach;
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


let search_string ?(mode="symbol") ew =
  let text = Entry.get ew in
  try
    if text = "" then () else
    let l = match mode with
      "Name" ->
        begin match guess_search_mode text with
          `Long -> search_string_symbol text
        | `Pattern -> search_pattern_symbol text
        | `Type -> search_string_type text ~mode:`Included
        end
    | "Type" -> search_string_type text ~mode:`Included
    | "Exact" -> search_string_type text ~mode:`Exact
    | _ -> assert false
    in
    match l with [] -> ()
    | [lid,kind] -> view_symbol lid ~kind ~env:!start_env
    | l          -> choose_symbol ~title:"Choose symbol" ~env:!start_env l
  with Searchid.Error (s,e) ->
    Entry.icursor ew ~index:(`Num s)

let search_which = ref "Name"

let search_symbol () =
  if !module_list = [] then
  module_list := List.sort ~cmp:compare (list_modules ~path:!Config.load_path);
  let tl = Jg_toplevel.titled "Search symbol" in
  Jg_bind.escape_destroy tl;
  let ew = Entry.create tl ~width:30 in
  let choice = Frame.create tl
  and which = Textvariable.create ~on:tl () in
  let itself = Radiobutton.create choice ~text:"Itself"
        ~variable:which ~value:"Name"
  and extype = Radiobutton.create choice ~text:"Exact type"
        ~variable:which ~value:"Exact"
  and iotype = Radiobutton.create choice ~text:"Included type"
        ~variable:which ~value:"Type"
  and buttons = Frame.create tl in
  let search = Button.create buttons ~text:"Search" ~command:
    begin fun () ->
      search_which := Textvariable.get which;
      search_string ew ~mode:!search_which
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

let ident_of_decl ~modlid = function
    Sig_value (id, _) -> Lident (Ident.name id), Pvalue
  | Sig_type (id, _, _) -> Lident (Ident.name id), Ptype
  | Sig_extension (id, _, _) -> Ldot (modlid, Ident.name id), Pconstructor
  | Sig_exception (id, _) -> Ldot (modlid, Ident.name id), Pconstructor
  | Sig_module (id, _, _) -> Lident (Ident.name id), Pmodule
  | Sig_modtype (id, _) -> Lident (Ident.name id), Pmodtype
  | Sig_class (id, _, _) -> Lident (Ident.name id), Pclass
  | Sig_class_type (id, _, _) -> Lident (Ident.name id), Pcltype

let view_defined ~env ?(show_all=false) modlid =
  try match lookup_module modlid env with path, Mty_signature sign ->
    let rec iter_sign sign idents =
      match sign with
        [] -> List.rev idents
      | decl :: rem ->
          let rem = match decl, rem with
            Sig_class _, cty :: ty1 :: ty2 :: rem -> rem
          | Sig_class_type _, ty1 :: ty2 :: rem -> rem
          | _, rem -> rem
          in iter_sign rem (ident_of_decl ~modlid decl :: idents)
    in
    let l = iter_sign sign [] in
    let title = string_of_path path in
    let env = open_signature Asttypes.Fresh path sign env in
    !choose_symbol_ref l ~title ~signature:sign ~env ~path;
    if show_all then view_signature sign ~title ~env ~path
  | _ -> ()
  with Not_found -> ()
  | Env.Error err ->
      let tl, tw, finish = Jg_message.formatted ~title:"Error!" () in
      Env.report_error Format.std_formatter err;
      finish ()
  | Cmi_format.Error err ->
      let tl, tw, finish = Jg_message.formatted ~title:"Error!" () in
      Cmi_format.report_error Format.std_formatter err;
      finish ()


(* Manage toplevel windows *)

let close_all_views () =
    List.iter !top_widgets
      ~f:(fun tl -> try destroy tl with Protocol.TkError _ -> ());
    top_widgets := []


(* Launch a shell *)

let shell_counter = ref 1
let default_shell = ref "ocaml"

let start_shell master =
  let tl = Jg_toplevel.titled "Start New Shell" in
  Wm.transient_set tl ~master;
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
  Text.configure tw ~state:`Disabled;
  Jg_bind.enter_focus tw;
  pack [tw] ~side:`Left ~fill:`Both ~expand:true;
  pack [sb] ~side:`Right ~fill:`Y;
  pack [fw] ~side:`Top ~expand:true ~fill:`Both;
  pack [ok] ~side:`Bottom ~fill:`X

(* Launch the classical viewer *)

let f ?(dir=Unix.getcwd()) ?on () =
  let (top, tl) = match on with
    None ->
      let tl = Jg_toplevel.titled "Module viewer" in
      ignore (Jg_bind.escape_destroy tl); (tl, coe tl)
  | Some top ->
      Wm.title_set top "OCamlBrowser";
      Wm.iconname_set top "OCamlBrowser";
      let tl = Frame.create top in
      bind tl ~events:[`Destroy] ~action:(fun _ -> exit 0);
      pack [tl] ~expand:true ~fill:`Both;
      (top, coe tl)
  in
  let menus = Jg_menu.menubar top in
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
  let search = Button.create buttons ~text:"Search" ~pady:1
      ~command:(fun () -> search_string ew)
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
  filemenu#add_command "Shell..." ~command:(fun () -> start_shell tl);
  filemenu#add_command "Quit" ~command:(fun () -> destroy tl);

  (* modules menu *)
  modmenu#add_command "Path editor..."
    ~command:(fun () -> Setpath.set ~dir);
  modmenu#add_command "Reset cache"
    ~command:(fun () -> reset_modules mbox; Env.reset_cache ());
  modmenu#add_command "Search symbol..." ~command:search_symbol;

  pack [close; search] ~fill:`X ~side:`Right ~expand:true;
  pack [coe buttons; coe ew] ~fill:`X ~side:`Bottom;
  pack [msb] ~side:`Right ~fill:`Y;
  pack [mbox] ~side:`Left ~fill:`Both ~expand:true;
  pack [fmbox] ~fill:`Both ~expand:true ~side:`Top;
  reset_modules mbox

(* Smalltalk-like version *)

class st_viewer ?(dir=Unix.getcwd()) ?on () =
  let (top, tl) = match on with
    None ->
      let tl = Jg_toplevel.titled "Module viewer" in
      ignore (Jg_bind.escape_destroy tl); (tl, coe tl)
  | Some top ->
      Wm.title_set top "OCamlBrowser";
      Wm.iconname_set top "OCamlBrowser";
      let tl = Frame.create top in
      bind tl ~events:[`Destroy] ~action:(fun _ -> exit 0);
      pack [tl] ~side:`Bottom ~expand:true ~fill:`Both;
      (top, coe tl)
  in
  let menus = Menu.create top ~name:"menubar" ~typ:`Menubar in
  let () = Toplevel.configure top ~menu:menus in
  let filemenu = new Jg_menu.c "File" ~parent:menus
  and modmenu = new Jg_menu.c "Modules" ~parent:menus
  and viewmenu = new Jg_menu.c "View" ~parent:menus
  and helpmenu = new Jg_menu.c "Help" ~parent:menus in
  let search_frame = Frame.create tl in
  let boxes_frame = Frame.create tl ~name:"boxes" in
  let label = Label.create tl ~anchor:`W ~padx:5 in
  let view = Frame.create tl in
  let buttons = Frame.create tl in
  let _all = Button.create buttons ~text:"Show all" ~padx:20
  and close = Button.create buttons ~text:"Close all" ~command:close_all_views
  and detach = Button.create buttons ~text:"Detach"
  and edit = Button.create buttons ~text:"Impl"
  and intf = Button.create buttons ~text:"Intf" in
object (self)
  val mutable boxes = []
  val mutable show_all = fun () -> ()

  method create_box =
    let fmbox, mbox, sb = Jg_box.create_with_scrollbar boxes_frame in
    bind mbox ~events:[`Modified([`Double], `ButtonPressDetail 1)]
      ~action:(fun _ -> show_all ());
    bind mbox ~events:[`Modified([`Double], `KeyPressDetail "Return")]
      ~action:(fun _ -> show_all ());
    boxes <- boxes @ [fmbox, mbox];
    pack [sb] ~side:`Right ~fill:`Y;
    pack [mbox] ~side:`Left ~fill:`Both ~expand:true;
    pack [fmbox] ~side:`Left ~fill:`Both ~expand:true;
    fmbox, mbox

  initializer
    (* Search *)
    let ew = Entry.create search_frame
    and searchtype = Textvariable.create ~on:tl () in
    bind ew ~events:[`KeyPressDetail "Return"] ~action:
      (fun _ -> search_string ew ~mode:(Textvariable.get searchtype));
    Jg_bind.enter_focus ew;
    let search_button ?value text =
      Radiobutton.create search_frame
        ~text ~variable:searchtype ~value:text in
    let symbol = search_button "Name"
    and atype = search_button "Type" in
    Radiobutton.select symbol;
    pack [Label.create search_frame ~text:"Search"] ~side:`Left ~ipadx:5;
    pack [ew] ~fill:`X ~expand:true ~side:`Left;
    pack [Label.create search_frame ~text:"by"] ~side:`Left ~ipadx:5;
    pack [symbol; atype] ~side:`Left;
    pack [Label.create search_frame] ~side:`Right

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
        { mw_frame = view; mw_title = Some label;
          mw_detach = detach; mw_edit = edit; mw_intf = intf };
    Searchpos.set_path := self#set_path;

    (* Buttons *)
    pack [close] ~side:`Right ~fill:`X ~expand:true;
    bind close ~events:[`Modified([`Double], `ButtonPressDetail 1)]
      ~action:(fun _ -> destroy tl);

    (* File menu *)
    filemenu#add_command "Open..."
      ~command:(fun () -> !editor_ref ~opendialog:true ());
    filemenu#add_command "Editor..." ~command:(fun () -> !editor_ref ());
    filemenu#add_command "Shell..." ~command:(fun () -> start_shell tl);
    filemenu#add_command "Quit" ~command:(fun () -> destroy tl);

    (* View menu *)
    viewmenu#add_command "Show all defs" ~command:(fun () -> show_all ());
    let show_search = Textvariable.create ~on:tl () in
    Textvariable.set show_search "1";
    Menu.add_checkbutton viewmenu#menu ~label:"Search Entry"
      ~variable:show_search ~indicatoron:true ~state:`Active
      ~command:
      begin fun () ->
        let v = Textvariable.get show_search in
        if v = "1" then begin
          pack [search_frame] ~after:menus ~fill:`X
        end else Pack.forget [search_frame]
      end;

    (* modules menu *)
    modmenu#add_command "Path editor..."
      ~command:(fun () -> Setpath.set ~dir);
    modmenu#add_command "Reset cache"
      ~command:(fun () -> reset_modules mbox; Env.reset_cache ());
    modmenu#add_command "Search symbol..." ~command:search_symbol;

    (* Help menu *)
    helpmenu#add_command "Manual..." ~command:show_help;

    pack [search_frame] ~fill:`X;
    pack [boxes_frame] ~fill:`Both ~expand:true;
    pack [buttons] ~fill:`X ~side:`Bottom;
    pack [view] ~fill:`Both ~side:`Bottom ~expand:true;
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

  method set_path path ~sign =
    let rec path_elems l path =
      match path with
        Path.Pdot (path, _, _) -> path_elems (path::l) path
      | _ -> []
    in
    let path_elems path =
      match path with
      | Path.Pident _ -> [path]
      | _ -> path_elems [] path
    in
    let see_path ~box:n ?(sign=[]) path =
      let (_, box) = List.nth boxes n in
      let texts = Listbox.get_range box ~first:(`Num 0) ~last:`End in
      let rec index s = function
          [] -> raise Not_found
        | a :: l -> if a = s then 0 else 1 + index s l
      in
      try
        let modlid, s =
          match path with
            Path.Pdot (p, s, _) -> longident_of_path p, s
          | Path.Pident i -> Longident.Lident "M", Ident.name i
          | _ -> assert false
        in
        let li, k =
          if sign = [] then Longident.Lident s, Pmodule else
          ident_of_decl ~modlid (List.hd sign) in
        let s =
          if n = 0 then string_of_longident li else
          string_of_longident li ^ " (" ^ string_of_kind k ^ ")" in
        let n = index s texts in
        Listbox.see box (`Num n);
        Listbox.activate box (`Num n)
      with Not_found -> ()
    in
    let l = path_elems path in
    if l <> [] then begin
      List.iter l ~f:
        begin fun path ->
          if not (List.mem path shown_paths) then
            view_symbol (longident_of_path path) ~kind:Pmodule
              ~env:Env.initial ~path;
          let n = self#get_box path - 1 in
          see_path path ~box:n
        end;
      see_path path ~box:(self#get_box path) ~sign
    end

  method choose_symbol ~title ~env ?signature ?path l =
    let n =
      match path with None -> 1
      | Some path -> self#get_box ~path
    in
    let l = List.sort l ~cmp:(fun (li1, _) (li2,_) -> compare li1 li2) in
    let nl = List.map l ~f:
        begin fun (li, k) ->
          string_of_longident li ^ " (" ^ string_of_kind k ^ ")"
        end in
    let _, box = List.nth boxes n in
    Listbox.delete box ~first:(`Num 0) ~last:`End;
    Listbox.insert box ~index:`End ~texts:nl;

    let current = ref None in
    let display index =
      let `Num pos = Listbox.index box ~index in
      try
        let li, k = List.nth l pos in
        self#hide_after (n+1);
        if !current = Some (li,k) then () else
        let path =
          match path, li with
            None, Ldot (lip, _) ->
              begin try
                Some (fst (lookup_module lip env))
              with Not_found -> None
              end
          | _ -> path
        in
        current := Some (li,k);
        view_symbol li ~kind:k ~env ?path
      with Failure "nth" -> ()
    in
    Jg_box.add_completion box ~double:false ~action:display;
    bind box ~events:[`KeyRelease] ~fields:[`Char]
      ~action:(fun ev -> display `Active);

    begin match signature with
      None -> ()
    | Some signature ->
        show_all <-
          begin fun () ->
            current := None;
            view_signature signature ~title ~env ?path
          end
    end
end

let st_viewer ?dir ?on () =
  let viewer = new st_viewer ?dir ?on () in
  choose_symbol_ref := viewer#choose_symbol
