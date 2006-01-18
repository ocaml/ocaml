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

open StdLabels
module Unix = UnixLabels
open Tk
open Jg_tk
open Dummy

(* Here again, memoize regexps *)

let (~!) = Jg_memo.fast ~f:Str.regexp

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

let dump_handle (h : Unix.file_descr) =
  let obj = Obj.repr h in
  if Obj.is_int obj || Obj.tag obj <> Obj.custom_tag then
    invalid_arg "Shell.dump_handle";
  Nativeint.format "%x" (Obj.obj obj)

(* The shell class. Now encapsulated *)

let protect f x = try f x with _ -> ()

let is_win32 = Sys.os_type = "Win32"
let use_threads = is_win32
let use_sigpipe = is_win32

class shell ~textw ~prog ~args ~env ~history =
  let (in2,out1) = Unix.pipe ()
  and (in1,out2) = Unix.pipe ()
  and (err1,err2) = Unix.pipe ()
  and (sig2,sig1) = Unix.pipe () in
object (self)
  val pid =
    let env =
      if use_sigpipe then
        let sigdef = "CAMLSIGPIPE=" ^ dump_handle sig2 in
        Array.append env [|sigdef|]
      else env
    in
    Unix.create_process_env ~prog ~args ~env
      ~stdin:in2 ~stdout:out2 ~stderr:err2
  val out = Unix.out_channel_of_descr out1
  val h : _ history = history
  val mutable alive = true
  val mutable reading = false
  val ibuffer = Buffer.create 1024
  val imutex = Mutex.create ()
  val mutable ithreads = []
  method alive = alive
  method kill =
    if Winfo.exists textw then Text.configure textw ~state:`Disabled;
    if alive then begin
      alive <- false;
      protect close_out out;
      try
        if use_sigpipe then ignore (Unix.write sig1 ~buf:"T" ~pos:0 ~len:1);
        List.iter ~f:(protect Unix.close) [in1; err1; sig1; sig2];
        if not use_threads then begin
          Fileevent.remove_fileinput ~fd:in1;
          Fileevent.remove_fileinput ~fd:err1;
        end;
        if not use_sigpipe then begin
          Unix.kill ~pid ~signal:Sys.sigkill;
          ignore (Unix.waitpid ~mode:[] pid)
        end
      with _ -> ()
    end
  method interrupt =
    if alive then try
      reading <- false;
      if use_sigpipe then begin
        ignore (Unix.write sig1 ~buf:"C" ~pos:0 ~len:1);
        self#send " "
      end else
        Unix.kill ~pid ~signal:Sys.sigint
    with Unix.Unix_error _ -> ()
  method send s =
    if alive then try
      output_string out s;
      flush out
    with Sys_error _ -> ()
  method private read ~fd ~len =
    begin try
      let buf = String.create len in
      let len = Unix.read fd ~buf ~pos:0 ~len in
      if len > 0 then begin
        self#insert (String.sub buf ~pos:0 ~len);
        Text.mark_set textw ~mark:"input" ~index:(`Mark"insert",[`Char(-1)])
      end;
      len
    with Unix.Unix_error _ -> 0
    end;
  method history (dir : [`Next|`Previous]) =
    if not h#empty then begin
      if reading then begin
        Text.delete textw ~start:(`Mark"input",[`Char 1])
          ~stop:(`Mark"insert",[])
      end else begin
        reading <- true;
        Text.mark_set textw ~mark:"input"
          ~index:(`Mark"insert",[`Char(-1)])
      end;
      self#insert (if dir = `Previous then h#previous else h#next)
    end
  method private lex ?(start = `Mark"insert",[`Linestart])
      ?(stop = `Mark"insert",[`Lineend]) () =
    Lexical.tag textw ~start ~stop
  method insert text =
    let idx = Text.index textw
        ~index:(`Mark"insert",[`Char(-1);`Linestart]) in
    Text.insert textw ~text ~index:(`Mark"insert",[]);
    self#lex ~start:(idx,[`Linestart]) ();
    Text.see textw ~index:(`Mark"insert",[])
  method private keypress c =
    if not reading && c > " " then begin
      reading <- true;
      Text.mark_set textw ~mark:"input" ~index:(`Mark"insert",[`Char(-1)])
    end
  method private keyrelease c = if c <> "" then self#lex ()
  method private return =
    if reading then reading <- false
    else Text.mark_set textw ~mark:"input"
        ~index:(`Mark"insert",[`Linestart;`Char 1]);
    Text.mark_set textw ~mark:"insert" ~index:(`Mark"insert",[`Lineend]);
    self#lex ~start:(`Mark"input",[`Linestart]) ();
    let s =
      (* input is one character before real input *)
      Text.get textw ~start:(`Mark"input",[`Char 1])
        ~stop:(`Mark"insert",[]) in
    h#add s;
    Text.insert textw ~index:(`Mark"insert",[]) ~text:"\n";
    Text.yview_index textw ~index:(`Mark"insert",[]);
    self#send s;
    self#send "\n"
  method private paste ev =
    if not reading then begin
      reading <- true;
      Text.mark_set textw ~mark:"input"
        ~index:(`Atxy(ev.ev_MouseX, ev.ev_MouseY),[`Char(-1)])
    end
  initializer
    Lexical.init_tags textw;
    let rec bindings =
      [ ([], `KeyPress, [`Char], fun ev -> self#keypress ev.ev_Char);
        ([], `KeyRelease, [`Char], fun ev -> self#keyrelease ev.ev_Char);
        (* [], `KeyPressDetail"Return", [], fun _ -> self#return; *)
        ([], `ButtonPressDetail 2, [`MouseX; `MouseY],  self#paste);
        ([`Alt], `KeyPressDetail"p", [], fun _ -> self#history `Previous);
        ([`Alt], `KeyPressDetail"n", [], fun _ -> self#history `Next);
        ([`Meta], `KeyPressDetail"p", [], fun _ -> self#history `Previous);
        ([`Meta], `KeyPressDetail"n", [], fun _ -> self#history `Next);
        ([`Control], `KeyPressDetail"c", [], fun _ -> self#interrupt);
        ([], `Destroy, [], fun _ -> self#kill) ]
    in
    List.iter bindings ~f:
      begin fun (modif,event,fields,action) ->
        bind textw ~events:[`Modified(modif,event)] ~fields ~action
      end;
    bind textw ~events:[`KeyPressDetail"Return"] ~breakable:true
      ~action:(fun _ -> self#return; break());
    List.iter ~f:Unix.close [in2;out2;err2];
    if use_threads then begin
      let fileinput_thread fd =
        let buf = String.create 1024 in
        let len = ref 0 in
        try while len := Unix.read fd ~buf ~pos:0 ~len:1024; !len > 0 do
          Mutex.lock imutex;
          Buffer.add_substring ibuffer buf 0 !len;
          Mutex.unlock imutex
        done with Unix.Unix_error _ -> ()
      in
      ithreads <- List.map [in1; err1] ~f:(Thread.create fileinput_thread);
      let rec read_buffer () =
        Mutex.lock imutex;
        if Buffer.length ibuffer > 0 then begin
          self#insert (Str.global_replace ~!"\r\n" "\n"
                         (Buffer.contents ibuffer));
          Buffer.reset ibuffer;
          Text.mark_set textw ~mark:"input" ~index:(`Mark"insert",[`Char(-1)])
        end;
        Mutex.unlock imutex;
        Timer.set ~ms:100 ~callback:read_buffer
      in
      read_buffer ()
    end else begin
      try
        List.iter [in1;err1] ~f:
          begin fun fd ->
            Fileevent.add_fileinput ~fd
              ~callback:(fun () -> ignore (self#read ~fd ~len:1024))
          end
      with _ -> ()
    end
end

(* Specific use of shell, for OCamlBrowser *)

let shells : (string * shell) list ref = ref []

(* Called before exiting *)
let kill_all () =
  List.iter !shells ~f:(fun (_,sh) -> if sh#alive then sh#kill);
  shells := []

let get_all () =
  let all = List.filter !shells ~f:(fun (_,sh) -> sh#alive) in
  shells := all;
  all

let may_exec_unix prog =
  try Unix.access prog ~perm:[Unix.X_OK]; prog
  with Unix.Unix_error _ -> ""

let may_exec_win prog =
  let has_ext =
    List.exists ~f:(Filename.check_suffix prog) ["exe"; "com"; "bat"] in
  if has_ext then may_exec_unix prog else
  List.fold_left [prog^".bat"; prog^".exe"; prog^".com"] ~init:""
    ~f:(fun res prog -> if res = "" then may_exec_unix prog else res)

let may_exec =
  if is_win32 then may_exec_win else may_exec_unix

let path_sep = if is_win32 then ";" else ":"

let warnings = ref "Al"

let program_not_found prog =
  Jg_message.info ~title:"Error"
    ("Program \"" ^ prog ^ "\"\nwas not found in path")

let protect_arg s =
  if String.contains s ' ' then "\"" ^ s ^ "\"" else s

let f ~prog ~title =
  let progargs =
    List.filter ~f:((<>) "") (Str.split ~!" " prog) in
  if progargs = [] then () else
  let prog = List.hd progargs in
  let path =
    try Sys.getenv "PATH" with Not_found -> "/bin" ^ path_sep ^ "/usr/bin" in
  let exec_path = Str.split ~!path_sep path in
  let exec_path = if is_win32 then "."::exec_path else exec_path in
  let progpath =
    if not (Filename.is_implicit prog) then may_exec prog else
    List.fold_left exec_path ~init:"" ~f:
      (fun res dir ->
        if res = "" then may_exec (Filename.concat dir prog) else res) in
  if progpath = "" then program_not_found prog else
  let tl = Jg_toplevel.titled title in
  let menus = Menu.create tl ~name:"menubar" ~typ:`Menubar in
  Toplevel.configure tl ~menu:menus;
  let file_menu = new Jg_menu.c "File" ~parent:menus
  and history_menu = new Jg_menu.c "History" ~parent:menus
  and signal_menu = new Jg_menu.c "Signal" ~parent:menus in
  let frame, tw, sb = Jg_text.create_with_scrollbar tl in
  Text.configure tw ~background:`White;
  pack [sb] ~fill:`Y ~side:`Right;
  pack [tw] ~fill:`Both ~expand:true ~side:`Left;
  pack [frame] ~fill:`Both ~expand:true;
  let env = Array.map (Unix.environment ()) ~f:
      begin fun s ->
        if Str.string_match ~!"TERM=" s 0 then "TERM=dumb" else s
      end in
  let load_path =
    List2.flat_map !Config.load_path ~f:(fun dir -> ["-I"; dir]) in
  let load_path =
    if is_win32 then List.map ~f:protect_arg load_path else load_path in
  let labels = if !Clflags.classic then ["-nolabels"] else [] in
  let rectypes = if !Clflags.recursive_types then ["-rectypes"] else [] in
  let warnings =
    if List.mem "-w" progargs || !warnings = "Al" then []
    else ["-w"; !warnings]
  in
  let args =
    Array.of_list (progargs @ labels @ warnings @ rectypes @ load_path) in
  let history = new history () in
  let start_shell () =
    let sh = new shell ~textw:tw ~prog:progpath ~env ~args ~history in
    shells := (title, sh) :: !shells;
    sh
  in
  let sh = ref (start_shell ()) in
  let current_dir = ref (Unix.getcwd ()) in
  file_menu#add_command "Restart" ~command:
    begin fun () ->
      (!sh)#kill;
      Text.configure tw ~state:`Normal;
      Text.insert tw ~index:(`End,[]) ~text:"\n";
      Text.see tw ~index:(`End,[]);
      Text.mark_set tw ~mark:"insert" ~index:(`End,[]);
      sh := start_shell ();
    end;
  file_menu#add_command "Use..." ~command:
    begin fun () ->
      Fileselect.f ~title:"Use File" ~filter:"*.ml"
        ~sync:true ~dir:!current_dir ()
        ~action:(fun l ->
          if l = [] then () else
          let name = Fileselect.caml_dir (List.hd l) in
          current_dir := Filename.dirname name;
          if Filename.check_suffix name ".ml"
          then
            let cmd = "#use \"" ^ String.escaped name ^ "\";;\n" in
            (!sh)#insert cmd; (!sh)#send cmd)
    end;
  file_menu#add_command "Load..." ~command:
    begin fun () ->
      Fileselect.f ~title:"Load File" ~filter:"*.cm[oa]" ~sync:true ()
        ~dir:!current_dir
        ~action:(fun l ->
          if l = [] then () else
          let name = Fileselect.caml_dir (List.hd l) in
          current_dir := Filename.dirname name;
          if Filename.check_suffix name ".cmo" ||
            Filename.check_suffix name ".cma"
          then
            let cmd = "#load \"" ^ String.escaped name ^ "\";;\n" in
            (!sh)#insert cmd; (!sh)#send cmd)
    end;
  file_menu#add_command "Import path" ~command:
    begin fun () ->
      List.iter (List.rev !Config.load_path) ~f:
        (fun dir ->
          (!sh)#send ("#directory \"" ^ String.escaped dir ^ "\";;\n"))
    end;
  file_menu#add_command "Close" ~command:(fun () -> destroy tl);
  history_menu#add_command "Previous  " ~accelerator:"M-p"
    ~command:(fun () -> (!sh)#history `Previous);
  history_menu#add_command "Next" ~accelerator:"M-n"
    ~command:(fun () -> (!sh)#history `Next);
  signal_menu#add_command "Interrupt  " ~accelerator:"C-c"
    ~command:(fun () -> (!sh)#interrupt);
  signal_menu#add_command "Kill" ~command:(fun () -> (!sh)#kill)
