(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
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

(* file selection box *)

open StdLabels
open Str
open Filename
open Tk

open Useunix

(**** Memoized rexgexp *)

let (~!) = Jg_memo.fast ~f:Str.regexp

(************************************************************ Path name *)

(* Convert Windows-style directory separator '\' to caml-style '/' *)
let caml_dir path =
  if Sys.os_type = "Win32" then
    global_replace ~!"\\\\" "/" path
  else path

let parse_filter s = 
  let s = caml_dir s in
  (* replace // by / *)
  let s = global_replace ~!"/+" "/" s in
  (* replace /./ by / *)
  let s = global_replace ~!"/\\./" "/" s in
  (* replace hoge/../ by "" *)
  let s = global_replace
          ~!"\\([^/]\\|[^\\./][^/]\\|[^/][^\\./]\\|[^/][^/]+\\)/\\.\\./" "" s in
  (* replace hoge/..$ by *)
  let s = global_replace
          ~!"\\([^/]\\|[^\\./][^/]\\|[^/][^\\./]\\|[^/][^/]+\\)/\\.\\.$" "" s in
  (* replace ^/hoge/../ by / *)
  let s = global_replace ~!"^\\(/\\.\\.\\)+/" "/" s in
  if string_match ~!"^\\([^\\*?[]*[/:]\\)\\(.*\\)" s 0 then 
    let dirs = matched_group 1 s
    and ptrn = matched_group 2 s
    in
      dirs, ptrn
  else "", s

let rec fixpoint ~f v =
  let v' = f v in
  if v = v' then v else fixpoint ~f v'

let unix_regexp s =
  let s = Str.global_replace ~!"[$^.+]" "\\\\\\0" s in
  let s = Str.global_replace ~!"\\*" ".*" s in
  let s = Str.global_replace ~!"\\?" ".?" s in
  let s =
    fixpoint s
      ~f:(Str.replace_first ~!"\\({.*\\),\\(.*}\\)" "\\1\\|\\2") in
  let s =
    Str.global_replace ~!"{\\(.*\\)}" "\\(\\1\\)" s in
  Str.regexp s

let exact_match ~pat s =
  Str.string_match pat s 0 && Str.match_end () = String.length s

let ls ~dir ~pattern =
  let files = get_files_in_directory dir in
  let regexp = unix_regexp pattern in
  List.filter files ~f:(exact_match ~pat:regexp)

(********************************************* Creation *)
let load_in_path = ref false

let search_in_path ~name = Misc.find_in_path !Config.load_path name

let f ~title ~action:proc ?(dir = Unix.getcwd ())
    ?filter:(deffilter ="*") ?file:(deffile ="")
    ?(multi=false) ?(sync=false) ?(usepath=true) () =

  let current_pattern = ref ""
  and current_dir = ref (caml_dir dir) in

  let may_prefix name =
    if Filename.is_relative name then concat !current_dir name else name in
  
  let tl = Jg_toplevel.titled title in
  Focus.set tl;

  let new_var () = Textvariable.create ~on:tl () in
  let filter_var = new_var ()
  and selection_var = new_var ()
  and sync_var = new_var () in
  Textvariable.set filter_var deffilter;

  let frm = Frame.create tl ~borderwidth:1 ~relief:`Raised in
    let df = Frame.create frm in
      let dfl = Frame.create df in
        let dfll = Label.create dfl ~text:"Directories" in
        let dflf, directory_listbox, directory_scrollbar =
            Jg_box.create_with_scrollbar dfl in
      let dfr = Frame.create df in
        let dfrl = Label.create dfr ~text:"Files" in
        let dfrf, filter_listbox, filter_scrollbar =
            Jg_box.create_with_scrollbar dfr in
  let cfrm = Frame.create tl ~borderwidth:1 ~relief:`Raised in

  let configure ~filter =
    let filter = may_prefix filter in
    let dir, pattern = parse_filter filter in
    let dir = if !load_in_path && usepath then "" else
              (current_dir := Filename.dirname dir; dir)
    and pattern = if pattern = "" then "*" else pattern in
      current_pattern := pattern;
    let filter =
        if !load_in_path && usepath then pattern else dir ^ pattern in
    let directories = get_directories_in_files ~path:dir 
          (get_files_in_directory dir) in
    let matched_files = (* get matched file by subshell call. *)
      if !load_in_path && usepath then
      List.fold_left !Config.load_path ~init:[] ~f:
      begin fun acc dir ->
        let files = ls ~dir ~pattern in
        Sort.merge (<) files
          (List.fold_left files ~init:acc
           ~f:(fun acc name -> List2.exclude name acc))
      end
      else
        List.fold_left directories ~init:(ls ~dir ~pattern)
          ~f:(fun acc dir -> List2.exclude dir acc)
    in
      Textvariable.set filter_var filter;
      Textvariable.set selection_var (dir ^ deffile); 
      Listbox.delete filter_listbox ~first:(`Num 0) ~last:`End;
      Listbox.insert filter_listbox ~index:`End ~texts:matched_files;
      Jg_box.recenter filter_listbox ~index:(`Num 0);
      if !load_in_path && usepath then
        Listbox.configure directory_listbox ~takefocus:false
      else
      begin
        Listbox.configure directory_listbox ~takefocus:true;
        Listbox.delete directory_listbox ~first:(`Num 0) ~last:`End;
        Listbox.insert directory_listbox ~index:`End ~texts:directories;
        Jg_box.recenter directory_listbox ~index:(`Num 0)
      end
  in
  
  let selected_files = ref [] in (* used for synchronous mode *)
  let activate l =
    Grab.release tl;
    destroy tl;
    let l =
      if !load_in_path && usepath then
        List.fold_right l ~init:[] ~f:
        begin fun name acc ->
          if not (Filename.is_implicit name) then
            may_prefix name :: acc
          else try search_in_path ~name :: acc with Not_found -> acc
        end
      else
        List.map l ~f:may_prefix
    in
    if sync then 
      begin
        selected_files := l;
        Textvariable.set sync_var "1"
      end
    else proc l 
  in
  
  (* entries *)
  let fl = Label.create frm ~text:"Filter" in
  let sl = Label.create frm ~text:"Selection" in
  let filter_entry = Jg_entry.create frm ~textvariable:filter_var
      ~command:(fun filter -> configure ~filter) in
  let selection_entry = Jg_entry.create frm ~textvariable:selection_var
      ~command:(fun file -> activate [file]) in

  (* and buttons *)
  let set_path = Button.create dfl ~text:"Path editor" ~command:
    begin fun () ->
      Setpath.add_update_hook (fun () -> configure ~filter:!current_pattern);
      let w = Setpath.f ~dir:!current_dir in
      Grab.set w;
      bind w ~events:[`Destroy] ~extend:true ~action:(fun _ -> Grab.set tl)
    end in
  let toggle_in_path = Checkbutton.create dfl ~text:"Use load path"
    ~command:
    begin fun () ->
      load_in_path := not !load_in_path;
      if !load_in_path then
        pack [set_path] ~side:`Bottom ~fill:`X ~expand:true
      else
        Pack.forget [set_path];
      configure ~filter:(Textvariable.get filter_var)
    end
  and okb = Button.create cfrm ~text:"Ok" ~command:
    begin fun () -> 
      let files = 
        List.map (Listbox.curselection filter_listbox) ~f:
        begin fun x ->
          !current_dir ^ Listbox.get filter_listbox ~index:x
        end
      in
      let files = if files = [] then [Textvariable.get selection_var] 
                                else files in
      activate [Textvariable.get selection_var]
    end
  and flb = Button.create cfrm ~text:"Filter"
      ~command:(fun () -> configure ~filter:(Textvariable.get filter_var))
  and ccb = Button.create cfrm ~text:"Cancel"
      ~command:(fun () -> activate []) in

  (* binding *)
  bind tl ~events:[`KeyPressDetail "Escape"] ~action:(fun _ -> activate []);
  Jg_box.add_completion filter_listbox
    ~action:(fun index -> activate [Listbox.get filter_listbox ~index]);
  if multi then Listbox.configure filter_listbox ~selectmode:`Multiple else
  bind filter_listbox ~events:[`ButtonPressDetail 1] ~fields:[`MouseY]
    ~action:(fun ev ->
      let name = Listbox.get filter_listbox
          ~index:(Listbox.nearest filter_listbox ~y:ev.ev_MouseY) in
      if !load_in_path && usepath then
        try Textvariable.set selection_var (search_in_path ~name)
        with Not_found -> ()
      else Textvariable.set selection_var (may_prefix name));

  Jg_box.add_completion directory_listbox ~action:
    begin fun index ->
      let filter =
        may_prefix (Listbox.get directory_listbox ~index) ^
        "/" ^ !current_pattern
      in configure ~filter
    end;

    pack [frm] ~fill:`Both ~expand:true;
    (* filter *)
    pack [fl] ~side:`Top ~anchor:`W;
    pack [filter_entry] ~side:`Top ~fill:`X;

    (* directory + files *)
    pack [df] ~side:`Top ~fill:`Both ~expand:true;
    (* directory *)
    pack [dfl] ~side:`Left ~fill:`Both ~expand:true;
    pack [dfll] ~side:`Top ~anchor:`W;
    if usepath then pack [toggle_in_path] ~side:`Bottom ~anchor:`W;
    pack [dflf] ~side:`Top ~fill:`Both ~expand:true;
    pack [directory_scrollbar] ~side:`Right ~fill:`Y;
    pack [directory_listbox] ~side:`Left ~fill:`Both ~expand:true;
    (* files *)
    pack [dfr] ~side:`Right ~fill:`Both ~expand:true;
    pack [dfrl] ~side:`Top ~anchor:`W;
    pack [dfrf] ~side:`Top ~fill:`Both ~expand:true;
    pack [filter_scrollbar] ~side:`Right ~fill:`Y; 
    pack [filter_listbox] ~side:`Left ~fill:`Both ~expand:true;

    (* selection *)
    pack [sl] ~before:df ~side:`Bottom ~anchor:`W;
    pack [selection_entry] ~before:sl ~side:`Bottom ~fill:`X;

    (* create OK, Filter and Cancel buttons *)
    pack [okb; flb; ccb] ~side:`Left ~fill:`X ~expand:true;
    pack [cfrm] ~before:frm ~side:`Bottom ~fill:`X;

  if !load_in_path && usepath then begin
    load_in_path := false;
    Checkbutton.invoke toggle_in_path;
    Checkbutton.select toggle_in_path
  end
  else configure ~filter:deffilter;

    Tkwait.visibility tl;
    Grab.set tl;

    if sync then
      begin
        Tkwait.variable sync_var;
        proc !selected_files
      end;
    ()
