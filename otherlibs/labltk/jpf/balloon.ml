(* $Id$ *)

(* easy balloon help facility *)

open Tk
open Widget
open Protocol

(* switch -- if you do not want balloons, set false *)
let flag = ref true
let debug = ref false

(* We assume we have at most one popup label at a time *)
let topw = ref default_toplevel
and popupw = ref (Obj.magic dummy : message widget)

let configure_cursor w cursor = 
  (* DDDDDDDDDIIIIIIIRRRRRRRRTTTTTTTTYYYYYYY *)
  Protocol.tkEval [| TkToken (name w); 
		    TkToken "configure";
		    TkToken "-cursor";
		    TkToken cursor |];
  ()

let put on: w ms: millisec mesg = 
  let t = ref None in
  let cursor = ref "" in

  let reset () = 
      begin
  	match !t with
  	  Some t -> Timer.remove t
  	| _ -> ()
      end;
      (* if there is a popup label, unmap it *)
      if Winfo.exists !topw && Wm.state !topw <> "withdrawn" then 
	begin
	  Wm.withdraw !topw;
	  if Winfo.exists w then configure_cursor w !cursor
	end
  and set ev =
    if !flag then
      t := Some (Timer.add ms: millisec callback: (fun () -> 
	t := None;
	if !debug then
	  prerr_endline ("Balloon: " ^ Widget.name w);
	update_idletasks();
	Message.configure !popupw text: mesg; 
	raise_window !topw;
	Wm.geometry_set !topw (* 9 & 8 are some kind of magic... *)
	  geometry: ("+"^(string_of_int (ev.ev_RootX + 9))^
		     "+"^(string_of_int (ev.ev_RootY + 8)));
	Wm.deiconify !topw;
	cursor := cget w `Cursor;
	configure_cursor w "hand2"))
  in

  List.iter fun: (fun x ->
    bind w events: x action: (`Extend ([], (fun _ -> 
(*      begin
	match x with
	  [[],Leave] -> prerr_endline " LEAVE reset "
	| _ -> prerr_endline " Other reset "
      end; 
*)
      reset ()))))
      [[[], `Leave]; [[], `ButtonPress]; [[], `ButtonRelease]; [[], `Destroy];
       [[], `KeyPress]; [[], `KeyRelease]];
  List.iter fun: (fun x ->
    bind w events:x action: (`Extend ([`RootX; `RootY], (fun ev -> 
(*
      begin
	match x with
	  [[],Enter] -> prerr_endline " Enter set "
	| [[],Motion] -> prerr_endline " Motion set "
	| _ -> prerr_endline " ??? set "
      end;
*)
      reset (); set ev))))
      [[[], `Enter]; [[], `Motion]]

let init () =
  let t = Hashtbl.create 101 in
  Protocol.add_destroy_hook (fun w ->
    Hashtbl.remove t key:w);
  topw := Toplevel.create parent:default_toplevel ();
  Wm.overrideredirect_set !topw to: true;
  Wm.withdraw !topw;
  popupw := Message.create parent:!topw name: "balloon" ()
	      background: (`Color "yellow") aspect: 300;
  pack [!popupw];
  class_bind "all" 
    events: [[], `Enter] action: (`Extend ([`Widget], (function w ->
    try Hashtbl.find t key: w.ev_Widget with
      Not_found -> begin
	Hashtbl.add t key:w.ev_Widget data: ();
	let x = Option.get w.ev_Widget name: "balloon" class: "Balloon" in
	if x <> "" then put on: w.ev_Widget ms: 1000 x
      end)))

