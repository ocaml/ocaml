(* $Id$ *)

open Tk

let add_scrollbar lb  =
  let sb =
    Scrollbar.create parent:(Winfo.parent lb) command:(Listbox.yview lb) () in
  Listbox.configure lb yscrollcommand:(Scrollbar.set sb); sb

let create_with_scrollbar :parent ?:selectmode () =
  let frame = Frame.create :parent () in
  let lb = Listbox.create parent:frame ?:selectmode () in
  frame, lb, add_scrollbar lb

(* from frx_listbox,adapted *)

let recenter lb :index =
   Listbox.selection_clear lb first:(`Num 0) last:`End;
     (* Activate it, to keep consistent with Up/Down.
        You have to be in Extended or Browse mode *)
   Listbox.activate lb :index;
   Listbox.selection_anchor lb :index;
   Listbox.yview_index lb :index

class timed ?:wait ?:nocase get_texts = object
  val get_texts = get_texts
  inherit Jg_completion.timed [] ?:wait ?:nocase as super
  method reset =
    texts <- get_texts ();
    super#reset
end

let add_completion ?:action ?:wait ?:nocase lb =
  let comp =
    new timed ?:wait ?:nocase
      (fun () -> Listbox.get_range lb first:(`Num 0) last:`End) in

  Jg_bind.enter_focus lb;

  bind lb events:[[], `KeyPress] 
    action:(`Set([`Char], fun ev -> 
      (* consider only keys producing characters. The callback is called
	 even if you press Shift. *)
      if ev.ev_Char <> "" then
 	recenter lb index:(`Num (comp#add ev.ev_Char))));

  begin match action with 
    Some action ->
      bind lb events:[[], `KeyPressDetail "Return"]
      	action:(`Set ([], fun _ -> action `Active));
      bind lb events:[[`Double], `ButtonPressDetail 1]
      	action:(`Setbreakable ([`MouseY], fun ev ->
	  action (Listbox.nearest lb y:ev.ev_MouseY); break ()))
  | None -> ()
  end;

  recenter lb index:(`Num 0)   (* so that first item is active *)
