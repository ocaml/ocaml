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
open Dummy

(* Here again, memoize regexps *)

let (~) = Jg_memo.fast f:Str.regexp

(* Nice history class. May reuse *)

class ['a] history () = object
  val mutable history = ([] : 'a list)
  val mutable count = 0
  method empty = history = []
  method add s = count <- 0; history <- s :: history
  method previous =
    let s = List.nth history count in
    count <- (count + 1) mod List.length history;
    s
  method next =
    let l = List.length history in
    count <- (l + count - 1) mod l;
    List.nth history ((l + count - 1) mod l)
end

let dump_mem ?(:pos = 0) ?:len obj =
  if not (Obj.is_block obj) then invalid_arg "Shell.dump_mem";
  let len =
    match len with
    | None -> Obj.size obj * Sys.word_size / 8 - pos
    | Some x -> x in
  let buf = Buffer.create 256 in
  for i = pos to len - 1 do
    let c = String.unsafe_get (Obj.obj obj) i in
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
  done;
  Buffer.contents buf

(* The shell class. Now encapsulated *)

let protect f x = try f x with _ -> ()

class shell :textw :prog :args :env =
  let (in2,out1) = Unix.pipe ()
  and (in1,out2) = Unix.pipe ()
  and (err1,err2) = Unix.pipe ()
  and (sig2,sig1) = Unix.pipe () in
object (self)
  val pid =
    let env =
      if Sys.os_type = "Win32" then
        let sigdef = "CAMLSIGPIPE=" ^ dump_mem (Obj.repr sig2) in
        Array.append env [|sigdef|]
      else env
    in
    Unix.create_process_env :prog :args :env
      stdin:in2 stdout:out2 stderr:err2
  val out = Unix.out_channel_of_descr out1
  val h = new history ()
  val mutable alive = true
  val mutable reading = false
  val ibuffer = Buffer.create 1024
  val imutex = Mutex.create ()
  val mutable ithreads = []
  method alive = alive
  method kill =
    if Winfo.exists textw then Text.configure textw state:`Disabled;
    if alive then begin
      alive <- false;
      protect close_out out;
      try
        if Sys.os_type = "Win32" then begin
          ignore (Unix.write sig1 buf:"T" pos:0 len:1);
          List.iter f:(protect Unix.close) [sig1; sig2]
        end else begin
          List.iter f:(protect Unix.close) [in1; err1; sig1; sig2];
          Fileevent.remove_fileinput fd:in1;
          Fileevent.remove_fileinput fd:err1;
          Unix.kill :pid signal:Sys.sigkill;
          ignore (Unix.waitpid mode:[] pid)
        end
      with _ -> ()
    end
  method interrupt =
    if alive then try
      reading <- false;
      if Sys.os_type = "Win32" then begin
        ignore (Unix.write sig1 buf:"C" pos:0 len:1);
        self#send " "
      end else
        Unix.kill :pid signal:Sys.sigint
    with Unix.Unix_error _ -> ()
  method send s =
    if alive then try
      output_string out s;
      flush out
    with Sys_error _ -> ()
  method private read :fd :len =
    begin try
      let buf = String.create len in
      let len = Unix.read fd :buf pos:0 :len in
      if len > 0 then begin
        self#insert (String.sub buf pos:0 :len);
        Text.mark_set textw mark:"input" index:(`Mark"insert",[`Char(-1)])
      end;
      len
    with Unix.Unix_error _ -> 0
    end;
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
  method private lex ?(:start = `Mark"insert",[`Linestart])
      ?(:end = `Mark"insert",[`Lineend]) () =
    Lexical.tag textw :start :end
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
    Text.mark_set textw mark:"insert"index:(`Mark"insert",[`Line 1]);
    self#lex start:(`Mark"input",[`Linestart]) ();
    let s =
      (* input is one character before real input *)
      Text.get textw start:(`Mark"input",[`Char 1])
        end:(`Mark"insert",[]) in
    h#add s;
    Text.insert textw index:(`Mark"insert",[]) text:"\n";
    Text.yview_index textw index:(`Mark"insert",[]);
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
      [ ([], `KeyPress, [`Char], fun ev -> self#keypress ev.ev_Char);
        ([], `KeyRelease, [`Char], fun ev -> self#keyrelease ev.ev_Char);
        (* [], `KeyPressDetail"Return", [], fun _ -> self#return; *)
        ([], `ButtonPressDetail 2, [`MouseX; `MouseY],  self#paste);
        ([`Alt], `KeyPressDetail"p", [], fun _ -> self#history `previous);
        ([`Alt], `KeyPressDetail"n", [], fun _ -> self#history `next);
        ([`Meta], `KeyPressDetail"p", [], fun _ -> self#history `previous);
        ([`Meta], `KeyPressDetail"n", [], fun _ -> self#history `next);
        ([`Control], `KeyPressDetail"c", [], fun _ -> self#interrupt);
        ([], `Destroy, [], fun _ -> self#kill) ]
    in
    List.iter bindings f:
      begin fun (modif,event,fields,action) ->
        bind textw events:[`Modified(modif,event)] :fields :action
      end;
    bind textw events:[`KeyPressDetail"Return"] breakable:true
      action:(fun _ -> self#return; break());
    List.iter f:Unix.close [in2;out2;err2];
    if Sys.os_type = "Win32" then begin
      let fileinput_thread fd =
        let buf = String.create 1024 in
        let len = ref 0 in
        try while len := ThreadUnix.read fd :buf pos:0 len:1024; !len > 0 do
          Mutex.lock imutex;
          Buffer.add_substring ibuffer buf pos:0 len:!len;
          Mutex.unlock imutex
        done with Unix.Unix_error _ -> ()
      in
      ithreads <- List.map [in1; err1] f:(Thread.create fileinput_thread);
      let rec read_buffer () =
        Mutex.lock imutex;
        if Buffer.length ibuffer > 0 then begin
          self#insert (Str.global_replace pat:~"\r\n" templ:"\n"
                         (Buffer.contents ibuffer));
          Buffer.reset ibuffer;
          Text.mark_set textw mark:"input" index:(`Mark"insert",[`Char(-1)])
        end;
        Mutex.unlock imutex;
        Timer.set ms:100 callback:read_buffer
      in
      read_buffer ()
    end else begin
      try
        List.iter [in1;err1] f:
          begin fun fd ->
            Fileevent.add_fileinput :fd
              callback:(fun () -> ignore (self#read :fd len:1024))
          end
      with _ -> ()
    end
end

(* Specific use of shell, for OCamlBrowser *)

let shells : (string * shell) list ref = ref []

(* Called before exiting *)
let kill_all () =
  List.iter !shells f:(fun (_,sh) -> if sh#alive then sh#kill);
  shells := []

let get_all () =
  let all = List.filter !shells f:(fun (_,sh) -> sh#alive) in
  shells := all;
  all

let may_exec_unix prog =
  try Unix.access file:prog perm:[Unix.X_OK]; true
  with Unix.Unix_error _ -> false

let may_exec_win prog =
  List.exists f:may_exec_unix [prog; prog^".exe"; prog^".cmo"; prog^".bat"]

let may_exec =
  if Sys.os_type = "Win32" then may_exec_win else may_exec_unix

let path_sep = if Sys.os_type = "Win32" then ";" else ":"

let warnings = ref "A"

let f :prog :title =
  let progargs =
    List.filter f:((<>) "") (Str.split sep:~" " prog) in
  if progargs = [] then () else
  let prog = List.hd progargs in
  let path =
    try Sys.getenv "PATH" with Not_found -> "/bin" ^ path_sep ^ "/usr/bin" in
  let exec_path = Str.split sep:~path_sep path in
  let exists =
    if not (Filename.is_implicit prog) then may_exec prog else
    List.exists exec_path
      f:(fun dir -> may_exec (Filename.concat dir prog)) in
  if not exists then () else
  let tl = Jg_toplevel.titled title in
  let menus = Frame.create tl name:"menubar" in
  let file_menu = new Jg_menu.c "File" parent:menus
  and history_menu = new Jg_menu.c "History" parent:menus
  and signal_menu = new Jg_menu.c "Signal" parent:menus in
  pack [menus] side:`Top fill:`X;
  pack [file_menu#button; history_menu#button; signal_menu#button]
    side:`Left ipadx:5 anchor:`W;
  let frame, tw, sb = Jg_text.create_with_scrollbar tl in
  Text.configure tw background:`White;
  pack [sb] fill:`Y side:`Right;
  pack [tw] fill:`Both expand:true side:`Left;
  pack [frame] fill:`Both expand:true;
  let env = Array.map (Unix.environment ()) f:
      begin fun s ->
        if Str.string_match pat:~"TERM=" s pos:0 then "TERM=dumb" else s
      end in
  let load_path =
    List2.flat_map !Config.load_path f:(fun dir -> ["-I"; dir]) in
  let modern = if !Clflags.classic then [] else ["-label"] in
  let warnings =
    if List.mem "-w" progargs || !warnings = "A" then []
    else ["-w"; !warnings]
  in
  let args = Array.of_list (progargs @ modern @ warnings @ load_path) in
  let sh = new shell textw:tw :prog :env :args in
  let current_dir = ref (Unix.getcwd ()) in
  file_menu#add_command "Use..." command:
    begin fun () ->
      Fileselect.f title:"Use File" filter:"*.ml" sync:true dir:!current_dir ()
        action:(fun l ->
          if l = [] then () else
          let name = List.hd l in
          current_dir := Filename.dirname name;
          if Filename.check_suffix name ".ml"
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
          if Filename.check_suffix name ".cmo" or
            Filename.check_suffix name ".cma"
          then
            let cmd = "#load \"" ^ name ^ "\";;\n" in
            sh#insert cmd; sh#send cmd)
    end;
  file_menu#add_command "Import path" command:
    begin fun () ->
      List.iter (List.rev !Config.load_path)
        f:(fun dir -> sh#send ("#directory \"" ^ dir ^ "\";;\n"))
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
