(* $Id$ *)

(* file selection box *)

open Unix
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
  Listbox.configure lb yscrollcommand: (Scrollbar.set sb);
  Scrollbar.configure sb command: (Listbox.yview lb)

(* focus when enter binding *)
let bind_enter_focus w = 
  bind w events: [[], `Enter] 
         action: (`Set ([], fun _ -> Focus.set w));;

let myentry_create p :variable =
  let w = Entry.create parent:p relief: `Sunken textvariable: variable () in
  bind_enter_focus w; w

(************************************************************* Subshell call *)

let subshell cmd = 
  let r,w = pipe () in
    match fork () with
      0 -> close r; dup2 w stdout; 
      	   execv prog:"/bin/sh" args:[| "/bin/sh"; "-c"; cmd |]; 
           exit 127
    | id -> 
        close w; 
        let rc = in_channel_of_descr r in
        let rec it () = try 
       	    let x = input_line rc in x:: it ()
          with _ -> []
        in 
      	  let answer = it() in
	  close_in rc;	(* because of finalize_channel *)
	  let p, st = waitpid flags:[] id in answer

(***************************************************************** Path name *)

(* find directory name which doesn't contain "?*[" *)
let dirget = regexp "^\([^\*?[]*/\)\(.*\)"

let parse_filter src = 
  (* replace // by / *)
  let s = global_replace (regexp "/+") with:"/" src in
  (* replace /./ by / *)
  let s = global_replace (regexp "/\./") with:"/" s in
  (* replace ????/../ by "" *)
  let s = global_replace 
    (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\./") 
      with:""  s in
  (* replace ????/..$ by "" *)
  let s = global_replace 
    (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\.$") 
      with:"" s in
  (* replace ^/../../ by / *)
  let s = global_replace (regexp "^\(/\.\.\)+/") with:"/" s in
  if string_match dirget s pos:0 then 
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
  let rec get_them () =
   try 
    let x = readdir dirh in  (* no let cause Out of memory *)
      x::(get_them ())
   with
      End_of_file -> closedir dirh; [] 
  in
    Sort.list order:(<) (get_them ())
      
let rec get_directories_in_files path = function
    [] -> []
  | x::xs -> 
      if try (stat (path ^ x)).st_kind = S_DIR with _ -> false then
      	x::(get_directories_in_files path xs)
      else get_directories_in_files path xs

let remove_directories dirname = 
  let rec remove = function
    [] -> []
  | x :: xs ->
    if try (stat (dirname ^ x)).st_kind = S_DIR with _ -> true then 
      remove xs
    else  
      x :: (remove xs)
  in remove

(************************* a nice interface to listbox - from frx_listbox.ml *)

let add_completion lb action =
  let prefx = ref ""		  (* current match prefix *)
  and maxi = ref 0                (* maximum index (doesn'y matter actually) *)
  and current = ref 0              (* current position *)
  and lastevent = ref 0 in

  let rec move_forward () =
    if Listbox.get lb index:(`Num !current) < !prefx then
      if !current < !maxi then begin incr current; move_forward() end

  and recenter () =
    let element = `Num !current in
     (* Clean the selection *)
     Listbox.selection_clear lb first:(`Num 0) last:`End;
     (* Set it to our unique element *)
     Listbox.selection_set lb first:element last:element;
     (* Activate it, to keep consistent with Up/Down.
        You have to be in Extended or Browse mode *)
     Listbox.activate lb index:element;
     Listbox.selection_anchor lb index:element;
     Listbox.see lb index:element in

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


  bind lb events:[[], `KeyPress] 
      action: (`Set([`Char; `Time], 
      	  (function ev -> 
	     (* consider only keys producing characters. The callback is called
	      * even if you press Shift.
              *)
      	     if ev.ev_Char <> "" then complete ev.ev_Time ev.ev_Char)));
  (* Key specific bindings override KeyPress *)
  bind lb events:[[], `KeyPressDetail "Return"] action:(`Set([], action));
  (* Finally, we have to set focus, otherwise events dont get through *)
  Focus.set lb;
  recenter()   (* so that first item is selected *);
  (* returns init_completion function *)
  (fun lb ->
    prefx := "";
    maxi := Listbox.size lb - 1;
    current := 0)

(****************************************************************** Creation *)

let f :title action:proc filter:deffilter file:deffile :multi :sync =
  (* Ah ! Now I regret about the names of the widgets... *)

  let current_pattern = ref ""
  and current_dir = ref "" in
  
  (* init_completions *)
  let filter_init_completion = ref (fun _ -> ())
  and directory_init_completion = ref (fun _ -> ()) in
  
  let tl = Toplevel.create parent:default_toplevel () in
  Focus.set tl;
  Wm.title_set tl :title;

  let filter_var = Textvariable.create on:tl () (* new_temporary *)
  and selection_var = Textvariable.create on:tl ()
  and sync_var = Textvariable.create on:tl () in

  let frm' = Frame.create parent:tl borderwidth: (`Pix 1) relief: `Raised () in
    let frm = Frame.create parent:frm' borderwidth: (`Pix 8) () in
    let fl = Label.create parent: frm text: "Filter" () in
    let df = Frame.create parent:frm () in
      let dfl = Frame.create parent:df () in
        let dfll = Label.create parent:dfl text: "Directories" () in
	let dflf = Frame.create parent:dfl () in
          let directory_listbox = Listbox.create parent:dflf relief: `Sunken ()
          and directory_scrollbar = Scrollbar.create parent:dflf () in
            scroll_link directory_scrollbar directory_listbox; 
      let dfr = Frame.create parent:df () in
        let dfrl = Label.create parent:dfr text: "Files" () in
	let dfrf = Frame.create parent:dfr () in
	  let filter_listbox = Listbox.create parent:dfrf relief: `Sunken () in
	  let filter_scrollbar = Scrollbar.create parent:dfrf () in
	    scroll_link filter_scrollbar filter_listbox;
    let sl = Label.create parent:frm text: "Selection" () in
    let filter_entry = myentry_create frm variable: filter_var in
    let selection_entry = myentry_create frm variable: selection_var
    in
  let cfrm' = Frame.create parent:tl borderwidth: (`Pix 1) relief: `Raised () in
    let cfrm = Frame.create parent:cfrm' borderwidth: (`Pix 8) () in
    let dumf = Frame.create parent:cfrm () in
    let dumf2 = Frame.create parent:cfrm () in

  let configure filter =
    (* OLDER let curdir = getcwd () in *)
(* Printf.eprintf "CURDIR %s\n" curdir; *)
    let filter =
      if string_match  (regexp "^/.*") filter pos:0 then filter
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
  	Textvariable.set filter_var to:filter;
  	Textvariable.set selection_var to:(dirname ^ deffile); 
  	Listbox.delete directory_listbox first:(`Num 0) last:`End;
  	Listbox.insert directory_listbox index:`End texts:directories;
  	Listbox.delete filter_listbox first:(`Num 0) last:`End;
  	Listbox.insert filter_listbox index:`End texts:matched_files;
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
      	Textvariable.set sync_var to:"1"
      end
    else 
      begin
	proc l; 
	break ()
      end 
  in
  
  (* and buttons *)
    let okb = Button.create parent:cfrm text: "OK" () command:
      begin fun () -> 
      	let files = 
      	  List.map (Listbox.curselection filter_listbox) 
	    fun:(fun x -> !current_dir ^ (Listbox.get filter_listbox index:x))
        in
        let files = if files = [] then [Textvariable.get selection_var] 
                                  else files in
      	activate files ()
      end
    in
    let flb = Button.create parent:cfrm text: "Filter" ()
      command: (fun () -> configure (Textvariable.get filter_var)) in
    let ccb = Button.create parent:cfrm text: "Cancel" ()
      command: (fun () -> activate [] ()) in

  (* binding *)
  bind selection_entry events:[[], `KeyPressDetail "Return"] 
    action:(`Setbreakable ([], fun _ -> 
      activate [Textvariable.get selection_var] () )); 
  bind filter_entry events:[[], `KeyPressDetail "Return"] action:(`Set ([], 
    fun _ -> configure (Textvariable.get filter_var) ));
  
  let action _ = 
      let files = 
      	List.map (Listbox.curselection filter_listbox)
	  fun:(fun x -> !current_dir ^ (Listbox.get filter_listbox index:x)) 
      in
      	activate files () 
  in
  bind filter_listbox events:[[`Double], `ButtonPressDetail 1] 
			      action:(`Setbreakable ([], action));
  if multi then Listbox.configure filter_listbox selectmode: `Multiple;
  filter_init_completion := add_completion filter_listbox action;

  let action _ =
    try
      configure (!current_dir ^ ((function
      	  [x] -> Listbox.get directory_listbox index:x
      	| _ -> (* you must choose at least one directory. *)
      	    Bell.ring (); raise Not_selected)
       (Listbox.curselection directory_listbox)) ^ "/" ^ !current_pattern) 
    with _ -> () in
  bind directory_listbox events:[[`Double], `ButtonPressDetail 1]
				 action:(`Setbreakable ([], action));
  Listbox.configure directory_listbox selectmode: `Browse;
  directory_init_completion := add_completion directory_listbox action;

    pack [frm'; frm] fill: `X;
    (* filter *)
    pack [fl] side: `Top anchor: `W;
    pack [filter_entry] side: `Top fill: `X;
    (* directory + files *)
    pack [df] side: `Top fill: `X ipadx: (`Pix 8);
    (* directory *)
    pack [dfl] side: `Left;
    pack [dfll] side: `Top anchor: `W;
    pack [dflf] side: `Top;
    pack [coe directory_listbox; coe directory_scrollbar] 
					  side: `Left fill: `Y;
    (* files *)
    pack [dfr] side: `Right;
    pack [dfrl] side: `Top anchor: `W;
    pack [dfrf] side: `Top;
    pack [coe filter_listbox; coe filter_scrollbar] side: `Left fill: `Y; 
    (* selection *)
    pack [sl] side: `Top anchor: `W;
    pack [selection_entry] side: `Top fill: `X;

    (* create OK, Filter and Cancel buttons *)
    pack [cfrm'] fill: `X;
    pack [cfrm] fill: `X;
    pack [okb] side: `Left;
    pack [dumf] side: `Left expand: true;
    pack [flb] side: `Left;
    pack [dumf2] side: `Left expand: true;
    pack [ccb] side: `Left;

    configure deffilter;

    Tkwait.visibility tl;
    Grab.set tl;

    if sync then
      begin
        Tkwait.variable sync_var;
	proc !selected_files
      end;
    ()
