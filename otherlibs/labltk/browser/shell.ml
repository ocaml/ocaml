(* $Id$ *)

open Tk
open Jg_tk

(* Nice history class. May reuse *)

class ['a] history () = object
  val mutable history = ([] : 'a list)
  val mutable count = 0
  method empty = history = []
  method add s = count <- 0; history <- s :: history
  method previous =
    let s = List.nth pos:count history in
    count <- (count + 1) mod List.length history;
    s
  method next =
    let l = List.length history in
    count <- (l + count - 1) mod l;
    List.nth history pos:((l + count - 1) mod l)
end

(* The shell class. Now encapsulated *)

let protect f x = try f x with _ -> ()

class shell :textw :prog :args :env =
  let (in2,out1) = Unix.pipe ()
  and (in1,out2) = Unix.pipe ()
  and (err1,err2) = Unix.pipe () in
object (self)
  val pid = Unix.create_process_env :prog :args :env in:in2 out:out2 err:err2
  val out = Unix.out_channel_of_descr out1
  val h = new history ()
  val mutable alive = true
  val mutable reading = false
  method alive = alive
  method kill =
    if Winfo.exists textw then Text.configure textw state:`Disabled;
    if alive then begin
      alive <- false;
      protect close_out out;
      List.iter fun:(protect Unix.close) [in1; err1; in2; out2; err2];
      try
	Fileevent.remove_fileinput fd:in1;
	Fileevent.remove_fileinput fd:err1;
	Unix.kill :pid signal:Sys.sigkill;
	Unix.waitpid flags:[] pid; ()
      with _ -> ()
    end
  method interrupt =
    if alive then try
      reading <- false;
      Unix.kill :pid signal:Sys.sigint
    with Unix.Unix_error _ -> ()
  method send s =
    if alive then try
      output_string s to:out;
      flush out
    with Sys_error _ -> ()
  method private read :fd :len =
    try
      let buffer = String.create :len in
      let len = Unix.read fd :buffer pos:0 :len in
      self#insert (String.sub buffer pos:0 :len);
      Text.mark_set textw mark:"input" index:(`Mark"insert",[`Char(-1)])
    with Unix.Unix_error _ -> ()
  method history (dir : [`next|`previous]) =
    if not h#empty then begin
      if reading then begin
	Text.delete textw start:(`Mark"input",[`Char 1])
	  end:(`Mark"insert",[])
      end else begin
	reading <- true;
	Text.mark_set textw mark:"input"
	  index:(`Mark"insert",[`Char(-1)])
      end;
      self#insert (if dir = `previous then h#previous else h#next)
    end
  method private lex ?:start{= `Mark"insert",[`Linestart]}
      ?end:endx{= `Mark"insert",[`Lineend]} () =
    Lexical.tag textw :start end:endx
  method insert text =
    let idx = Text.index textw
	index:(`Mark"insert",[`Char(-1);`Linestart]) in
    Text.insert textw :text index:(`Mark"insert",[]);
    self#lex start:(idx,[`Linestart]) ();
    Text.see textw index:(`Mark"insert",[])
  method private keypress c =
    if not reading & c > " " then begin
      reading <- true;
      Text.mark_set textw mark:"input" index:(`Mark"insert",[`Char(-1)])
    end
  method private keyrelease c = if c <> "" then self#lex ()
  method private return =
    if reading then reading <- false
    else Text.mark_set textw mark:"input"
	index:(`Mark"insert",[`Linestart;`Char 1]);
    self#lex start:(`Mark"input",[`Linestart]) ();
    let s =
      (* input is one character before real input *)
      Text.get textw start:(`Mark"input",[`Char 1])
	end:(`Mark"insert",[]) in
    h#add s;
    self#send s;
    self#send "\n"
  method private paste ev =
    if not reading then begin
      reading <- true;
      Text.mark_set textw mark:"input"
	index:(`Atxy(ev.ev_MouseX, ev.ev_MouseY),[`Char(-1)])
    end
  initializer
    Lexical.init_tags textw;
    let rec bindings =
      [ ([[],`KeyPress],[`Char],fun ev -> self#keypress ev.ev_Char);
	([[],`KeyRelease],[`Char],fun ev -> self#keyrelease ev.ev_Char);
	([[],`KeyPressDetail"Return"],[],fun _ -> self#return);
	([[],`ButtonPressDetail 2], [`MouseX; `MouseY], self#paste);
	([[`Alt],`KeyPressDetail"p"],[],fun _ -> self#history `previous);
	([[`Alt],`KeyPressDetail"n"],[],fun _ -> self#history `next);
	([[`Meta],`KeyPressDetail"p"],[],fun _ -> self#history `previous);
	([[`Meta],`KeyPressDetail"n"],[],fun _ -> self#history `next);
	([[`Control],`KeyPressDetail"c"],[],fun _ -> self#interrupt);
	([[],`Destroy],[],fun _ -> self#kill) ]
    in
    List.iter bindings
      fun:(fun (events,fields,f) ->
	bind textw :events action:(`Set(fields,f)));
    begin try
      List.iter [in1;err1] fun:
	begin fun fd ->
	  Fileevent.add_fileinput :fd
	    callback:(fun () -> self#read :fd len:1024)
	end
    with _ -> ()
    end
end

(* Specific use of shell, for LablBrowser *)

let shells : (string * shell) list ref = ref []

(* Called before exiting *)
let kill_all () =
  List.iter !shells fun:(fun (_,sh) -> if sh#alive then sh#kill);
  shells := []

let get_all () =
  let all = List.filter !shells pred:(fun (_,sh) -> sh#alive) in
  shells := all;
  all

let may_exec prog =
  try
    let stats = Unix.stat prog in
    stats.Unix.st_perm land 1 <> 0 or
    stats.Unix.st_perm land 8 <> 0
      & List.mem elt:stats.Unix.st_gid (Array.to_list (Unix.getgroups ())) or
    stats.Unix.st_perm land 64 <> 0 & stats.Unix.st_uid = Unix.getuid ()
  with Unix.Unix_error _ -> false

let f :prog :title =
  let progargs =
    List.filter pred:((<>) "") (Str.split sep:(Str.regexp " ") prog) in
  if progargs = [] then () else
  let prog = List.hd progargs in
  let path = try Sys.getenv "PATH" with Not_found -> "/bin:/usr/bin" in
  let exec_path = Str.split sep:(Str.regexp":") path in
  let exists =
    if not (Filename.is_implicit prog) then may_exec prog else
    List.exists exec_path
      pred:(fun dir -> may_exec (Filename.concat dir prog)) in
  if not exists then () else
  let tl = Jg_toplevel.titled title in
  let menus = Frame.create parent:tl name:"menubar" () in
  let file_menu = new Jg_menu.c "File" parent:menus
  and history_menu = new Jg_menu.c "History" parent:menus
  and signal_menu = new Jg_menu.c "Signal" parent:menus in
  pack [menus] side:`Top fill:`X;
  pack [file_menu#button; history_menu#button; signal_menu#button]
    side:`Left ipadx:(`Pix 5) anchor:`W;
  let frame, tw, sb = Jg_text.create_with_scrollbar parent:tl in
  Text.configure tw background:`White;
  pack [sb] fill:`Y side:`Right;
  pack [tw] fill:`Both expand:true side:`Left;
  pack [frame] fill:`Both expand:true;
  let reg = Str.regexp "TERM=" in
  let env = Array.map (Unix.environment ()) fun:
      begin fun s ->
 	if Str.string_match reg s pos:0 then "TERM=dumb" else s
      end in
  let load_path =
    List2.flat_map !Config.load_path fun:(fun dir -> ["-I"; dir]) in
  let args = Array.of_list (progargs @ load_path) in
  let sh = new shell textw:tw :prog :env :args in
  let current_dir = ref (Unix.getcwd ()) in
  file_menu#add_command "Use..." command:
    begin fun () ->
      Fileselect.f title:"Use File" filter:"*.ml" sync:true dir:!current_dir ()
	action:(fun l ->
	  if l = [] then () else
	  let name = List.hd l in
	  current_dir := Filename.dirname name;
	  if Filename.check_suffix name suff:".ml"
	  then
	    let cmd = "#use \"" ^ name ^ "\";;\n" in
	    sh#insert cmd; sh#send cmd)
    end;
  file_menu#add_command "Load..." command:
    begin fun () ->
      Fileselect.f title:"Load File" filter:"*.cm[oa]" sync:true ()
	dir:!current_dir
	action:(fun l ->
	  if l = [] then () else
	  let name = List.hd l in
	  current_dir := Filename.dirname name;
	  if Filename.check_suffix name suff:".cmo" or
	    Filename.check_suffix name suff:".cma"
	  then
	    let cmd = "#load \"" ^ name ^ "\";;\n" in
	    sh#insert cmd; sh#send cmd)
    end;
  file_menu#add_command "Import path" command:
    begin fun () ->
      List.iter (List.rev !Config.load_path)
	fun:(fun dir -> sh#send ("#directory \"" ^ dir ^ "\";;\n"))
    end;
  file_menu#add_command "Close" command:(fun () -> destroy tl);
  history_menu#add_command "Previous  " accelerator:"M-p"
    command:(fun () -> sh#history `previous);
  history_menu#add_command "Next" accelerator:"M-n"
    command:(fun () -> sh#history `next);
  signal_menu#add_command "Interrupt  " accelerator:"C-c"
    command:(fun () -> sh#interrupt);
  signal_menu#add_command "Kill" command:(fun () -> sh#kill);
  shells := (title, sh) :: !shells
