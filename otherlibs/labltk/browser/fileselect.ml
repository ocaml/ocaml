(* $Id$ *)

(* file selection box *)

open Useunix
open Str
open Filename

open Tk

(**** Memoized rexgexp *)

let regexp = (new Jg_memo.c fun:Str.regexp)#get

(************************************************************ Path name *)

let parse_filter src = 
  (* replace // by / *)
  let s = global_replace (regexp "/+") with:"/" src in
  (* replace /./ by / *)
  let s = global_replace (regexp "/\./") with:"/" s in
  (* replace hoge/../ by "" *)
  let s = global_replace
     (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\./") with:"" s in
  (* replace hoge/..$ by *)
  let s = global_replace
     (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\.$") with:"" s in
  (* replace ^/../../ by / *)
  let s = global_replace (regexp "^\(/\.\.\)+/") with:"/" s in
  if string_match (regexp "^\([^\*?[]*/\)\(.*\)") s pos:0 then 
    let dirs = matched_group 1 s
    and ptrn = matched_group 2 s
    in
      dirs, ptrn
  else "", s

let fixpoint fun:f v =
  let v1 = ref v and v2 = ref (f v) in
  while !v1 <> !v2 do v1 := !v2; v2 := f !v2 done;
  !v1

let unix_regexp s =
  let s = Str.global_replace (regexp "[$^.+]") with:"\\\\\\0" s in
  let s = Str.global_replace (regexp "\\*") with:".*" s in
  let s = Str.global_replace (regexp "\\?") with:".?" s in
  let s =
    fixpoint s fun:(fun s ->
      Str.global_replace (regexp "\\({.*\\),\\(.*}\\)") s
	with:"\\1\\|\\2") in
  let s =
    Str.global_replace (regexp "{\\(.*\\)}") with:"\\(\\1\\)" s in
  Str.regexp s

let exact_match s :regexp =
  Str.string_match regexp s pos:0 & Str.match_end () = String.length s

let ls :dir :pattern =
  let files = get_files_in_directory dir in
  let regexp = unix_regexp pattern in
  List.filter files pred:(exact_match :regexp)

(*
let ls :dir :pattern =
  subshell cmd:("cd " ^ dir ^ ";/bin/ls -ad " ^ pattern ^" 2>/dev/null")
*)
 
(********************************************* Creation *)
let load_in_path = ref false

let search_in_path :name = Misc.find_in_path !Config.load_path name

let f :title action:proc ?:dir{=Unix.getcwd ()}
    ?filter:deffilter{="*"} ?file:deffile{=""}
    ?:multi{=false} ?:sync{=false} ?:usepath{=true} () =

  let current_pattern = ref ""
  and current_dir = ref dir in
  
  let tl = Jg_toplevel.titled title in
  Focus.set tl;

  let new_var () = Textvariable.create on:tl () in
  let filter_var = new_var ()
  and selection_var = new_var ()
  and sync_var = new_var () in
  Textvariable.set filter_var to:deffilter;

  let frm = Frame.create parent:tl borderwidth:(`Pix 1) relief:`Raised () in
    let df = Frame.create parent:frm () in
      let dfl = Frame.create parent:df () in
        let dfll = Label.create parent:dfl text:"Directories" () in
	let dflf, directory_listbox, directory_scrollbar =
	    Jg_box.create_with_scrollbar parent:dfl () in
      let dfr = Frame.create parent:df () in
        let dfrl = Label.create parent:dfr text:"Files" () in
	let dfrf, filter_listbox, filter_scrollbar =
	    Jg_box.create_with_scrollbar parent:dfr () in
  let cfrm = Frame.create parent:tl borderwidth:(`Pix 1) relief:`Raised () in

  let configure :filter =
    let filter =
      if string_match  (regexp "^/.*") filter pos:0
      then filter
      else !current_dir ^ "/" ^ filter
    in
    let dir, pattern = parse_filter filter in
    let dir = if !load_in_path & usepath then "" else
      	      (current_dir := Filename.dirname dir; dir)
    and pattern = if pattern = "" then "*" else pattern in
      current_pattern := pattern;
    let filter =
        if !load_in_path & usepath then pattern else dir ^ pattern in
    let directories = get_directories_in_files path:dir 
      	  (get_files_in_directory dir) in
    let matched_files = (* get matched file by subshell call. *)
      if !load_in_path & usepath then
      List.fold_left !Config.load_path acc:[] fun:
      begin fun :acc dir ->
      	let files = ls :dir :pattern in
	Sort.merge order:(<) files
      	  (List.fold_left files :acc
      	   fun:(fun :acc name -> List2.exclude elt:name acc))
      end
      else
	List.fold_left directories acc:(ls :dir :pattern)
	  fun:(fun :acc dir -> List2.exclude elt:dir acc)
    in
      Textvariable.set filter_var to:filter;
      Textvariable.set selection_var to:(dir ^ deffile); 
      Listbox.delete filter_listbox first:(`Num 0) last:`End;
      Listbox.insert filter_listbox index:`End texts:matched_files;
      Jg_box.recenter filter_listbox index:(`Num 0);
      if !load_in_path & usepath then
        Listbox.configure directory_listbox takefocus:false
      else
      begin
      	Listbox.configure directory_listbox takefocus:true;
      	Listbox.delete directory_listbox first:(`Num 0) last:`End;
      	Listbox.insert directory_listbox index:`End texts:directories;
      	Jg_box.recenter directory_listbox index:(`Num 0)
      end
  in
  
  let selected_files = ref [] in (* used for synchronous mode *)
  let activate l =
    Grab.release tl;
    destroy tl;
    let l =
      if !load_in_path & usepath then
        List.fold_right l acc:[] fun:
      	begin fun name :acc ->
      	  if name <> "" & name.[0] = '/' then name :: acc else
      	  try search_in_path :name :: acc with Not_found -> acc
      	end
      else
        List.map l fun:
      	begin fun x ->
      	  if x <> "" & x.[0] = '/' then x
          else !current_dir ^ "/" ^ x
	end
    in
    if sync then 
      begin
        selected_files := l;
      	Textvariable.set sync_var to:"1"
      end
    else proc l 
  in
  
  (* entries *)
  let fl = Label.create parent:frm text:"Filter" () in
  let sl = Label.create parent:frm text:"Selection" () in
  let filter_entry = Jg_entry.create parent:frm textvariable:filter_var ()
      command:(fun filter -> configure :filter) in
  let selection_entry = Jg_entry.create parent:frm textvariable:selection_var
      command:(fun file -> activate [file]) () in

  (* and buttons *)
  let set_path = Button.create parent:dfl text:"Path editor" () command:
    begin fun () ->
      Setpath.add_update_hook (fun () -> configure filter:!current_pattern);
      let w = Setpath.f dir:!current_dir in
      Grab.set w;
      bind w events:[[], `Destroy]
        action:(`Extend ([], fun _ -> Grab.set tl))
    end in
  let toggle_in_path = Checkbutton.create parent:dfl text:"Use load path" ()
    command:
    begin fun () ->
      load_in_path := not !load_in_path;
      if !load_in_path then
        pack [set_path] side:`Bottom fill:`X expand:true
      else
      	Pack.forget [set_path];
      configure filter:(Textvariable.get filter_var)
    end
  and okb = Button.create parent:cfrm text:"Ok" () command:
    begin fun () -> 
      let files = 
      	List.map (Listbox.curselection filter_listbox) fun:
	begin fun x ->
      	  !current_dir ^ Listbox.get filter_listbox index:x
	end
      in
      let files = if files = [] then [Textvariable.get selection_var] 
                                else files in
      activate [Textvariable.get selection_var]
    end
  and flb = Button.create parent:cfrm text:"Filter" ()
      command:(fun () -> configure filter:(Textvariable.get filter_var))
  and ccb = Button.create parent:cfrm text:"Cancel" ()
      command:(fun () -> activate []) in

  (* binding *)
  bind tl events:[[], `KeyPressDetail "Escape"]
    action:(`Set ([], fun _ -> activate []));
  Jg_box.add_completion filter_listbox
    action:(fun index -> activate [Listbox.get filter_listbox :index]);
  if multi then Listbox.configure filter_listbox selectmode:`Multiple else
  bind filter_listbox events:[[], `ButtonPressDetail 1]
    action:(`Set ([`MouseY], fun ev ->
      let name = Listbox.get filter_listbox
          index:(Listbox.nearest filter_listbox y:ev.ev_MouseY) in
      if !load_in_path & usepath then
        try Textvariable.set selection_var to:(search_in_path :name)
	with Not_found -> ()
      else Textvariable.set selection_var to:(!current_dir ^ "/" ^ name)));

  Jg_box.add_completion directory_listbox action:
    begin fun index ->
      let filter =
      	!current_dir ^ "/" ^
       	(Listbox.get directory_listbox :index) ^
       	"/" ^ !current_pattern
      in configure :filter
    end;

    pack [frm] fill:`Both expand:true;
    (* filter *)
    pack [fl] side:`Top anchor:`W;
    pack [filter_entry] side:`Top fill:`X;

    (* directory + files *)
    pack [df] side:`Top fill:`Both expand:true;
    (* directory *)
    pack [dfl] side:`Left fill:`Both expand:true;
    pack [dfll] side:`Top anchor:`W;
    if usepath then pack [toggle_in_path] side:`Bottom anchor:`W;
    pack [dflf] side:`Top fill:`Both expand:true;
    pack [directory_scrollbar] side:`Right fill:`Y;
    pack [directory_listbox] side:`Left fill:`Both expand:true;
    (* files *)
    pack [dfr] side:`Right fill:`Both expand:true;
    pack [dfrl] side:`Top anchor:`W;
    pack [dfrf] side:`Top fill:`Both expand:true;
    pack [filter_scrollbar] side:`Right fill:`Y; 
    pack [filter_listbox] side:`Left fill:`Both expand:true;

    (* selection *)
    pack [sl] before:df side:`Bottom anchor:`W;
    pack [selection_entry] before:sl side:`Bottom fill:`X;

    (* create OK, Filter and Cancel buttons *)
    pack [okb; flb; ccb] side:`Left fill:`X expand:true;
    pack [cfrm] before:frm side:`Bottom fill:`X;

  if !load_in_path & usepath then begin
    load_in_path := false;
    Checkbutton.invoke toggle_in_path;
    Checkbutton.select toggle_in_path
  end
  else configure filter:deffilter;

    Tkwait.visibility tl;
    Grab.set tl;

    if sync then
      begin
        Tkwait.variable sync_var;
	proc !selected_files
      end;
    ()
