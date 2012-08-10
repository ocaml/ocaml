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

(* Listboxes *)

let update_hooks = ref []

let add_update_hook f = update_hooks := f :: !update_hooks

let exec_update_hooks () =
    update_hooks := List.filter !update_hooks ~f:
      begin fun f ->
        try f (); true
        with Protocol.TkError _ -> false
      end

let set_load_path l =
    Config.load_path := l;
    exec_update_hooks ()

let get_load_path () = !Config.load_path

let renew_dirs box ~var ~dir =
  Textvariable.set var dir;
  Listbox.delete box ~first:(`Num 0) ~last:`End;
  Listbox.insert box ~index:`End
    ~texts:(Useunix.get_directories_in_files ~path:dir
                 (Useunix.get_files_in_directory dir));
  Jg_box.recenter box ~index:(`Num 0)

let renew_path box =
  Listbox.delete box ~first:(`Num 0) ~last:`End;
  Listbox.insert box ~index:`End ~texts:!Config.load_path;
  Jg_box.recenter box ~index:(`Num 0)

let add_to_path ~dirs ?(base="") box =
  let dirs =
    if base = "" then dirs else
    if dirs = [] then [base] else
    List.map dirs ~f:
      begin function
          "." -> base
        | ".." -> Filename.dirname base
        | x -> Filename.concat base x
      end
  in
  set_load_path
    (dirs @ List.fold_left dirs ~init:(get_load_path ())
              ~f:(fun acc x -> List2.exclude x acc))

let remove_path box ~dirs =
  set_load_path
    (List.fold_left dirs ~init:(get_load_path ())
       ~f:(fun acc x -> List2.exclude x acc))

(* main function *)

let f ~dir =
  let current_dir = ref dir in
  let tl = Jg_toplevel.titled "Edit Load Path" in
  Jg_bind.escape_destroy tl;
  let var_dir = Textvariable.create ~on:tl () in
  let caplab = Label.create tl ~text:"Path"
  and dir_name = Entry.create tl ~textvariable:var_dir
  and browse = Frame.create tl in
  let dirs = Frame.create browse
  and path = Frame.create browse in
  let dirframe, dirbox, dirsb = Jg_box.create_with_scrollbar dirs
  and pathframe, pathbox, pathsb = Jg_box.create_with_scrollbar path
  in
  add_update_hook (fun () -> renew_path pathbox);
  Listbox.configure pathbox ~width:40 ~selectmode:`Multiple;
  Listbox.configure dirbox ~selectmode:`Multiple;
  Jg_box.add_completion dirbox ~action:
    begin fun index ->
      begin match Listbox.get dirbox ~index with
        "." -> ()
      | ".." -> current_dir := Filename.dirname !current_dir
      | x -> current_dir := !current_dir ^ "/" ^ x
      end;
      renew_dirs dirbox ~var:var_dir ~dir:!current_dir;
      Listbox.selection_clear dirbox ~first:(`Num 0) ~last:`End
    end;
  Jg_box.add_completion pathbox ~action:
    begin fun index ->
      current_dir := Listbox.get pathbox ~index;
      renew_dirs dirbox ~var:var_dir ~dir:!current_dir
    end;

  bind dir_name ~events:[`KeyPressDetail"Return"]
    ~action:(fun _ ->
      let dir = Textvariable.get var_dir in
      if Useunix.is_directory dir then begin
        current_dir := dir;
        renew_dirs dirbox ~var:var_dir ~dir
      end);

  (* Avoid space being used by the completion mechanism *)
  let bind_space_toggle lb =
    bind lb ~events:[`KeyPressDetail "space"] ~extend:true ~action:ignore in
  bind_space_toggle dirbox;
  bind_space_toggle pathbox;

  let add_paths _ =
    add_to_path pathbox ~base:!current_dir
      ~dirs:(List.map (Listbox.curselection dirbox)
              ~f:(fun x -> Listbox.get dirbox ~index:x));
    Listbox.selection_clear dirbox ~first:(`Num 0) ~last:`End
  and remove_paths _ =
    remove_path pathbox
      ~dirs:(List.map (Listbox.curselection pathbox)
              ~f:(fun x -> Listbox.get pathbox ~index:x))
  in
  bind dirbox ~events:[`KeyPressDetail "Insert"] ~action:add_paths;
  bind pathbox ~events:[`KeyPressDetail "Delete"] ~action:remove_paths;

  let dirlab = Label.create dirs ~text:"Directories"
  and pathlab = Label.create path ~text:"Load path"
  and addbutton = Button.create dirs ~text:"Add to path" ~command:add_paths
  and pathbuttons = Frame.create path in
  let removebutton =
    Button.create pathbuttons ~text:"Remove from path" ~command:remove_paths
  and ok =
    Jg_button.create_destroyer tl ~parent:pathbuttons
  in
  renew_dirs dirbox ~var:var_dir ~dir:!current_dir;
  renew_path pathbox;
  pack [dirsb] ~side:`Right ~fill:`Y;
  pack [dirbox] ~side:`Left ~fill:`Y ~expand:true;
  pack [pathsb] ~side:`Right ~fill:`Y;
  pack [pathbox] ~side:`Left ~fill:`Both ~expand:true;
  pack [dirlab] ~side:`Top ~anchor:`W ~padx:10;
  pack [addbutton] ~side:`Bottom ~fill:`X;
  pack [dirframe] ~fill:`Y ~expand:true;
  pack [pathlab] ~side:`Top ~anchor:`W ~padx:10;
  pack [removebutton; ok] ~side:`Left ~fill:`X ~expand:true;
  pack [pathbuttons] ~fill:`X ~side:`Bottom;
  pack [pathframe] ~fill:`Both ~expand:true;
  pack [dirs] ~side:`Left ~fill:`Y;
  pack [path] ~side:`Right ~fill:`Both ~expand:true;
  pack [caplab] ~side:`Top ~anchor:`W ~padx:10;
  pack [dir_name] ~side:`Top ~anchor:`W ~fill:`X;
  pack [browse] ~side:`Bottom ~expand:true ~fill:`Both;
  tl

let set ~dir = ignore (f ~dir);;
