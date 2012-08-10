(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of OCaml                     *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the OCaml source tree.          *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* file selection box *)

(* This file selecter works only under the OS with the full unix support.
   For the portability, Tk.getOpenFile and Tk.getSaveFile are recommended. *)

open StdLabels
open UnixLabels
open Str
open Filename

open Tk
open Widget

exception Not_selected

(********************************************************** Search directory *)
(* Default is curdir *)
let global_dir = ref (getcwd ())

(***************************************************** Some widgets creation *)

(* from frx_listbox.ml *)
let scroll_link sb lb =
  Listbox.configure lb ~yscrollcommand: (Scrollbar.set sb);
  Scrollbar.configure sb ~command: (Listbox.yview lb)

(* focus when enter binding *)
let bind_enter_focus w =
  bind w ~events:[`Enter] ~action:(fun _ -> Focus.set w);;

let myentry_create p ~variable =
  let w = Entry.create p ~relief: `Sunken ~textvariable: variable in
  bind_enter_focus w; w

(************************************************************* Subshell call *)

let subshell cmd =
  let r,w = pipe () in
    match fork () with
      0 -> close r; dup2 ~src:w ~dst:stdout;
           execv ~prog:"/bin/sh" ~args:[| "/bin/sh"; "-c"; cmd |]
    | id ->
        close w;
        let rc = in_channel_of_descr r in
        let rec it l =
          match
            try Some(input_line rc) with _ -> None
          with
            Some x -> it (x::l)
          | None -> List.rev l
        in
        let answer = it [] in
        close_in rc;  (* because of finalize_channel *)
        let _ = waitpid ~mode:[] id in answer

(***************************************************************** Path name *)

(* find directory name which doesn't contain "?*[" *)
let dirget = regexp "^\\([^\\*?[]*/\\)\\(.*\\)"

let parse_filter src =
  (* replace // by / *)
  let s = global_replace (regexp "/+") "/" src in
  (* replace /./ by / *)
  let s = global_replace (regexp "/\\./") "/" s in
  (* replace ????/../ by "" *)
  let s = global_replace
      (regexp "\\([^/]\\|[^\\./][^/]\\|[^/][^\\./]\\|[^/][^/]+\\)/\\.\\./")
      ""
      s in
  (* replace ????/..$ by "" *)
  let s = global_replace
      (regexp "\\([^/]\\|[^\\./][^/]\\|[^/][^\\./]\\|[^/][^/]+\\)/\\.\\.$")
      ""
      s in
  (* replace ^/../../ by / *)
  let s = global_replace (regexp "^\\(/\\.\\.\\)+/") "/" s in
  if string_match dirget s 0 then
    let dirs = matched_group 1 s
    and ptrn = matched_group 2 s
    in
      dirs, ptrn
  else "", s

let ls dir pattern =
  subshell ("cd " ^ dir ^ ";/bin/ls -ad " ^ pattern ^" 2>/dev/null")

(*************************************************************** File System *)

let get_files_in_directory dir =
  let dirh = opendir dir in
  let rec get_them l =
    match
      try Some(Unix.readdir dirh) with _ -> None
    with
    | None ->
        Unix.closedir dirh; l
    | Some x ->
        get_them (x::l)
  in
  List.sort ~cmp:compare (get_them [])

let rec get_directories_in_files path =
  List.filter
    ~f:(fun x -> try (stat (path ^ x)).st_kind = S_DIR with _ -> false)

let remove_directories path =
  List.filter
    ~f:(fun x -> try (stat (path ^ x)).st_kind <> S_DIR with _ -> false)

(************************* a nice interface to listbox - from frx_listbox.ml *)

let add_completion lb action =
  let prefx = ref ""              (* current match prefix *)
  and maxi = ref 0                (* maximum index (doesn'y matter actually) *)
  and current = ref 0              (* current position *)
  and lastevent = ref 0 in

  let rec move_forward () =
    if Listbox.get lb ~index:(`Num !current) < !prefx then
      if !current < !maxi then begin incr current; move_forward() end

  and recenter () =
    let element = `Num !current in
     (* Clean the selection *)
     Listbox.selection_clear lb ~first:(`Num 0) ~last:`End;
     (* Set it to our unique element *)
     Listbox.selection_set lb ~first:element ~last:element;
     (* Activate it, to keep consistent with Up/Down.
        You have to be in Extended or Browse mode *)
     Listbox.activate lb ~index:element;
     Listbox.selection_anchor lb ~index:element;
     Listbox.see lb ~index:element in

  let complete time s =
    if time - !lastevent < 500 then   (* sorry, hard coded limit *)
      prefx := !prefx ^ s
    else begin (* reset *)
      current := 0;
      prefx := s
    end;
    lastevent := time;
    move_forward();
    recenter() in


  bind lb ~events:[`KeyPress] ~fields:[`Char; `Time]
    (* consider only keys producing characters. The callback is called
       if you press Shift. *)
    ~action:(fun ev -> if ev.ev_Char <> "" then complete ev.ev_Time ev.ev_Char);
  (* Key specific bindings override KeyPress *)
  bind lb ~events:[`KeyPressDetail "Return"] ~action;
  (* Finally, we have to set focus, otherwise events dont get through *)
  Focus.set lb;
  recenter()   (* so that first item is selected *);
  (* returns init_completion function *)
  (fun lb ->
    prefx := "";
    maxi := Listbox.size lb - 1;
    current := 0)

(****************************************************************** Creation *)

let f ~title ~action:proc ~filter:deffilter ~file:deffile ~multi ~sync =
  (* Ah ! Now I regret about the names of the widgets... *)

  let current_pattern = ref ""
  and current_dir = ref "" in

  (* init_completions *)
  let filter_init_completion = ref (fun _ -> ())
  and directory_init_completion = ref (fun _ -> ()) in

  let tl = Toplevel.create default_toplevel in
  Focus.set tl;
  Wm.title_set tl title;

  let filter_var = Textvariable.create ~on:tl () (* new_temporary *)
  and selection_var = Textvariable.create ~on:tl ()
  and sync_var = Textvariable.create ~on:tl () in

  let frm' = Frame.create tl ~borderwidth: 1 ~relief: `Raised in
    let frm = Frame.create frm' ~borderwidth: 8 in
    let fl = Label.create  frm ~text: "Filter" in
    let df = Frame.create frm in
      let dfl = Frame.create df in
        let dfll = Label.create dfl ~text: "Directories" in
        let dflf = Frame.create dfl in
          let directory_listbox = Listbox.create dflf ~relief: `Sunken
          and directory_scrollbar = Scrollbar.create dflf in
            scroll_link directory_scrollbar directory_listbox;
      let dfr = Frame.create df in
        let dfrl = Label.create dfr ~text: "Files" in
        let dfrf = Frame.create dfr in
          let filter_listbox = Listbox.create dfrf ~relief: `Sunken in
          let filter_scrollbar = Scrollbar.create dfrf in
            scroll_link filter_scrollbar filter_listbox;
    let sl = Label.create frm ~text: "Selection" in
    let filter_entry = myentry_create frm ~variable: filter_var in
    let selection_entry = myentry_create frm ~variable: selection_var
    in
  let cfrm' = Frame.create tl ~borderwidth: 1 ~relief: `Raised in
    let cfrm = Frame.create cfrm' ~borderwidth: 8 in
    let dumf = Frame.create cfrm in
    let dumf2 = Frame.create cfrm in

  let configure filter =
    (* OLDER let curdir = getcwd () in *)
(* Printf.eprintf "CURDIR %s\n" curdir; *)
    let filter =
      if string_match (regexp "^/.*") filter 0 then filter
      else
        if filter = "" then !global_dir ^ "/*"
        else !global_dir ^ "/" ^ filter in
(* Printf.eprintf "FILTER %s\n" filter; *)
    let dirname, patternname = parse_filter filter in
(* Printf.eprintf "DIRNAME %s PATTERNNAME %s\n" dirname patternname; *)
      current_dir := dirname;
      global_dir := dirname;
    let patternname = if patternname = "" then "*" else patternname in
      current_pattern := patternname;
    let filter = dirname ^ patternname in
(* Printf.eprintf "FILTER : %s\n\n" filter; *)
(* flush Pervasives.stderr; *)
    try
      let directories = get_directories_in_files dirname
            (get_files_in_directory dirname) in
      (* get matched file by subshell call. *)
      let matched_files = remove_directories dirname (ls dirname patternname)
      in
        Textvariable.set filter_var filter;
        Textvariable.set selection_var (dirname ^ deffile);
        Listbox.delete directory_listbox ~first:(`Num 0) ~last:`End;
        Listbox.insert directory_listbox ~index:`End ~texts:directories;
        Listbox.delete filter_listbox ~first:(`Num 0) ~last:`End;
        Listbox.insert filter_listbox ~index:`End ~texts:matched_files;
        !directory_init_completion directory_listbox;
        !filter_init_completion filter_listbox
    with
      Unix_error (ENOENT,_,_) ->
        (* Directory is not found (maybe) *)
        Bell.ring ()
  in

  let selected_files = ref [] in (* used for synchronous mode *)
  let activate l () =
    Grab.release tl;
    destroy tl;
    if sync then
      begin
        selected_files := l;
        Textvariable.set sync_var "1"
      end
    else
      begin
        proc l;
        break ()
      end
  in

  (* and buttons *)
    let okb = Button.create cfrm ~text: "OK" ~command:
      begin fun () ->
        let files =
          List.map (Listbox.curselection filter_listbox)
            ~f:(fun x -> !current_dir ^ (Listbox.get filter_listbox ~index:x))
        in
        let files = if files = [] then [Textvariable.get selection_var]
                                  else files in
        activate files ()
      end
    in
    let flb = Button.create cfrm ~text: "Filter"
      ~command: (fun () -> configure (Textvariable.get filter_var)) in
    let ccb = Button.create cfrm ~text: "Cancel"
      ~command: (fun () -> activate [] ()) in

  (* binding *)
  bind selection_entry ~events:[`KeyPressDetail "Return"] ~breakable:true
    ~action:(fun _ -> activate [Textvariable.get selection_var] ());
  bind filter_entry ~events:[`KeyPressDetail "Return"]
      ~action:(fun _ -> configure (Textvariable.get filter_var));

  let action _ =
      let files =
        List.map (Listbox.curselection filter_listbox)
          ~f:(fun x -> !current_dir ^ (Listbox.get filter_listbox ~index:x))
      in
        activate files ()
  in
  bind filter_listbox ~events:[`Modified([`Double], `ButtonPressDetail 1)]
    ~breakable:true ~action;
  if multi then Listbox.configure filter_listbox ~selectmode: `Multiple;
  filter_init_completion := add_completion filter_listbox action;

  let action _ =
    try
      configure (!current_dir ^ ((function
          [x] -> Listbox.get directory_listbox ~index:x
        | _ -> (* you must choose at least one directory. *)
            Bell.ring (); raise Not_selected)
       (Listbox.curselection directory_listbox)) ^ "/" ^ !current_pattern)
    with _ -> () in
  bind directory_listbox ~events:[`Modified([`Double], `ButtonPressDetail 1)]
    ~breakable:true ~action;
  Listbox.configure directory_listbox ~selectmode: `Browse;
  directory_init_completion := add_completion directory_listbox action;

    pack [frm'; frm] ~fill: `X;
    (* filter *)
    pack [fl] ~side: `Top ~anchor: `W;
    pack [filter_entry] ~side: `Top ~fill: `X;
    (* directory + files *)
    pack [df] ~side: `Top ~fill: `X ~ipadx: 8;
    (* directory *)
    pack [dfl] ~side: `Left;
    pack [dfll] ~side: `Top ~anchor: `W;
    pack [dflf] ~side: `Top;
    pack [coe directory_listbox; coe directory_scrollbar]
                                          ~side: `Left ~fill: `Y;
    (* files *)
    pack [dfr] ~side: `Right;
    pack [dfrl] ~side: `Top ~anchor: `W;
    pack [dfrf] ~side: `Top;
    pack [coe filter_listbox; coe filter_scrollbar] ~side: `Left ~fill: `Y;
    (* selection *)
    pack [sl] ~side: `Top ~anchor: `W;
    pack [selection_entry] ~side: `Top ~fill: `X;

    (* create OK, Filter and Cancel buttons *)
    pack [cfrm'] ~fill: `X;
    pack [cfrm] ~fill: `X;
    pack [okb] ~side: `Left;
    pack [dumf] ~side: `Left ~expand: true;
    pack [flb] ~side: `Left;
    pack [dumf2] ~side: `Left ~expand: true;
    pack [ccb] ~side: `Left;

    configure deffilter;

    Tkwait.visibility tl;
    Grab.set tl;

    if sync then
      begin
        Tkwait.variable sync_var;
        proc !selected_files
      end;
    ()
