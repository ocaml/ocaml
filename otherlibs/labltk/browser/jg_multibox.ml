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

let rec gen_list ~f:f ~len =
  if len = 0 then [] else f () :: gen_list ~f:f ~len:(len - 1)

let rec make_list ~len ~fill =
  if len = 0 then [] else fill :: make_list ~len:(len - 1) ~fill

(* By column version
let rec firsts ~len l =
  if len = 0 then ([],l) else
  match l with
    a::l ->
      let (f,l) = firsts l len:(len - 1) in
      (a::f,l)
  | [] ->
      (l,[])

let rec split ~len = function
    [] -> []
  | l ->
      let (f,r) = firsts l ~len in
      let ret = split ~len r in
      f :: ret

let extend l ~len ~fill =
  if List.length l >= len then l
  else l @ make_list ~fill len:(len - List.length l)
*)

(* By row version *)

let rec first l ~len =
  if len = 0 then [], l else
  match l with
    [] -> make_list ~len ~fill:"", []
  | a::l ->
      let (l',r) = first ~len:(len - 1) l in a::l',r

let rec split l ~len =
  if l = [] then make_list ~len ~fill:[] else
  let (cars,r) = first l ~len in
  let cdrs = split r ~len in
  List.map2 cars cdrs ~f:(fun a l -> a::l)
  

open Tk

class c ~cols ~texts ?maxheight ?width parent = object (self)
  val parent' = coe parent
  val length = List.length texts
  val boxes =
    let height = (List.length texts - 1) / cols + 1 in
    let height =
      match maxheight with None -> height
      | Some max -> min max height
    in
    gen_list ~len:cols ~f:
      begin fun () ->
        Listbox.create parent ~height ?width
          ~highlightthickness:0
          ~borderwidth:1
      end
  val mutable current = 0
  method cols = cols
  method texts = texts
  method parent = parent'
  method boxes = boxes
  method current = current
  method recenter ?(aligntop=false) n =
    current <-
       if n < 0 then 0 else
       if n < length then n else length - 1;
    (* Activate it, to keep consistent with Up/Down.
       You have to be in Extended or Browse mode *)
    let box = List.nth boxes (current mod cols)
    and index = `Num (current / cols) in
    List.iter boxes ~f:
      begin fun box ->
        Listbox.selection_clear box ~first:(`Num 0) ~last:`End;
        Listbox.selection_anchor box ~index;
        Listbox.activate box ~index
      end;
    Focus.set box;
    if aligntop then Listbox.yview_index box ~index
    else Listbox.see box ~index;
    let (first,last) = Listbox.yview_get box in
    List.iter boxes ~f:(Listbox.yview ~scroll:(`Moveto first))
  method init =
    let textl = split ~len:cols texts in
    List.iter2 boxes textl ~f:
      begin fun box texts ->
        Jg_bind.enter_focus box;
        Listbox.insert box ~texts ~index:`End
      end;
    pack boxes ~side:`Left ~expand:true ~fill:`Both;
    self#bind_mouse ~events:[`ButtonPressDetail 1]
      ~action:(fun _ ~index:n -> self#recenter n; break ());
    let current_height () =
      let (top,bottom) = Listbox.yview_get (List.hd boxes) in
      truncate ((bottom -. top) *. float (Listbox.size (List.hd boxes))
                  +. 0.99)
    in
    List.iter
      [ "Right", (fun n -> n+1);
        "Left", (fun n -> n-1);
        "Up", (fun n -> n-cols);
        "Down", (fun n -> n+cols);
        "Prior", (fun n -> n - current_height () * cols);
        "Next", (fun n -> n + current_height () * cols);
        "Home", (fun _ -> 0);
        "End", (fun _ -> List.length texts) ]
      ~f:begin fun (key,f) ->
        self#bind_kbd ~events:[`KeyPressDetail key]
          ~action:(fun _ ~index:n -> self#recenter (f n); break ())
      end;
    self#recenter 0
  method bind_mouse ~events ~action =
    let i = ref 0 in
    List.iter boxes ~f:
      begin fun box ->
        let b = !i in
        bind box ~events ~breakable:true ~fields:[`MouseX;`MouseY]
          ~action:(fun ev ->
            let `Num n = Listbox.nearest box ~y:ev.ev_MouseY
            in action ev ~index:(n * cols + b));
        incr i
      end
  method bind_kbd ~events ~action =
    let i = ref 0 in
    List.iter boxes ~f:
      begin fun box ->
        let b = !i in
        bind box ~events ~breakable:true ~fields:[`Char]
          ~action:(fun ev ->
            let `Num n = Listbox.index box ~index:`Active in
            action ev ~index:(n * cols + b));
        incr i
      end
end

let add_scrollbar (box : c) =
  let boxes = box#boxes in
  let sb =
    Scrollbar.create (box#parent)
      ~command:(fun ~scroll -> List.iter boxes ~f:(Listbox.yview ~scroll)) in
  List.iter boxes
    ~f:(fun lb -> Listbox.configure lb ~yscrollcommand:(Scrollbar.set sb));
  pack [sb] ~before:(List.hd boxes) ~side:`Right ~fill:`Y;
  sb

let add_completion ?action ?wait (box : c) =
  let comp = new Jg_completion.timed (box#texts) ?wait in
  box#bind_kbd ~events:[`KeyPress]
    ~action:(fun ev ~index -> 
      (* consider only keys producing characters. The callback is called
       * even if you press Shift. *)
      if ev.ev_Char <> "" then
        box#recenter (comp#add ev.ev_Char) ~aligntop:true);
  match action with
    Some action ->
      box#bind_kbd ~events:[`KeyPressDetail "space"]
        ~action:(fun ev ~index -> action (box#current));
      box#bind_kbd ~events:[`KeyPressDetail "Return"]
        ~action:(fun ev ~index -> action (box#current));
      box#bind_mouse ~events:[`ButtonPressDetail 1]
        ~action:(fun ev ~index ->
          box#recenter index; action (box#current); break ())
  | None -> ()
