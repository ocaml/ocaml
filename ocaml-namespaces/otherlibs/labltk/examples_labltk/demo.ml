(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Some CamlTk4 Demonstration by JPF *)

(* First, open these modules for convenience *)
open StdLabels
open Tk

(* Dummy let *)
let _ =

(* Initialize Tk *)
let top = openTk () in
(* Title setting *)
Wm.title_set top "LablTk demo";

(* Base frame *)
let base = Frame.create top in
pack [base];

(* Menu bar *)
let bar = Frame.create ~borderwidth:2 ~relief:`Raised  base in
pack ~fill:`X [bar];

  (* Menu and Menubutton *)
  let meb = Menubutton.create ~text:"Menu" bar in
  let men = Menu.create meb in
  Menu.add_command ~label:"Quit" ~command:(fun () -> closeTk (); exit 0) men;
  Menubutton.configure ~menu:men meb;

  (* Frames *)
  let base2 = Frame.create base in
  let left = Frame.create base2 in
  let right = Frame.create base2 in
  pack [base2];
  pack ~side:`Left [left; right];

    (* Widgets on left and right *)

    (* Button *)
    let but = Button.create ~text:"Welcome to LablTk" left in

    (* Canvas *)
    let can =
      Canvas.create ~width:100 ~height:100 ~borderwidth:1 ~relief:`Sunken left
    in
    let oval = Canvas.create_oval ~x1: 10 ~y1: 10
                                  ~x2: 90 ~y2: 90
                                  ~fill: `Red
                                  can
    in ignore oval;

    (* Check button *)
    let che = Checkbutton.create ~text:"Check" left in

    (* Entry *)
    let ent = Entry.create ~width:10 left in

    (* Label *)
    let lab = Label.create ~text:"Welcome to LablTk" left in

    (* Listbox *)
    let lis = Listbox.create left in
    Listbox.insert lis ~index:`End ~texts:["This"; "is"; "Listbox"];

    (* Message *)
    let mes = Message.create
        ~text: "Hello this is a message widget with very long text, but ..."
        left in

    (* Radio buttons *)
    let tv = Textvariable.create () in
    Textvariable.set tv "One";
    let radf = Frame.create right in
    let rads = List.map
        ~f:(fun t -> Radiobutton.create ~text:t ~value:t ~variable:tv radf)
        ["One"; "Two"; "Three"] in

    (* Scale *)
    let sca = Scale.create ~label:"Scale" ~length:100 ~showvalue:true right in

    (* Text and scrollbar *)
    let texf = Frame.create right in

      (* Text *)
      let tex = Text.create ~width:20 ~height:8 texf in
      Text.insert ~index:(`End,[]) ~text:"This is a text widget." tex;

      (* Scrollbar *)
      let scr = Scrollbar.create texf in

      (* Text and Scrollbar widget link *)
      let scroll_link sb tx =
        Text.configure ~yscrollcommand:(Scrollbar.set sb) tx;
        Scrollbar.configure ~command:(Text.yview tx) sb in
      scroll_link scr tex;

      pack ~side:`Right ~fill:`Y [scr];
      pack ~side:`Left ~fill:`Both ~expand:true [tex];

    (* Pack them *)
    pack ~side:`Left [meb];
    pack [coe but; coe can; coe che; coe ent; coe lab; coe lis; coe mes];
    pack [coe radf; coe sca; coe texf];
    pack rads;

  (* Toplevel *)
  let top2 = Toplevel.create top in
  Wm.title_set top2 "LablTk demo control";
  let defcol = `Color "#dfdfdf" in
  let selcol = `Color "#ffdfdf" in
  let buttons =
    List.map ~f:(fun (w, t, c, a) ->
        let b = Button.create ~text:t ~command:c top2 in
        bind ~events:[`Enter] ~action:(fun _ -> a selcol) b;
        bind ~events:[`Leave] ~action:(fun _ -> a defcol) b;
        b)
      [coe bar, "Frame", (fun () -> ()),
       (fun background -> Frame.configure ~background bar);
       coe meb, "Menubutton", (fun () -> ()),
       (fun background -> Menubutton.configure ~background meb);
       coe but, "Button", (fun () -> ()),
       (fun background -> Button.configure ~background but);
       coe can, "Canvas", (fun () -> ()),
       (fun background -> Canvas.configure ~background can);
       coe che, "CheckButton", (fun () -> ()),
       (fun background -> Checkbutton.configure ~background che);
       coe ent, "Entry", (fun () -> ()),
       (fun background -> Entry.configure ~background ent);
       coe lab, "Label", (fun () -> ()),
       (fun background -> Label.configure ~background lab);
       coe lis, "Listbox", (fun () -> ()),
       (fun background -> Listbox.configure ~background lis);
       coe mes, "Message", (fun () -> ()),
       (fun background -> Message.configure ~background mes);
       coe radf, "Radiobox", (fun () -> ()),
       (fun background ->
         List.iter ~f:(fun b -> Radiobutton.configure ~background b) rads);
       coe sca, "Scale", (fun () -> ()),
       (fun background -> Scale.configure ~background sca);
       coe tex, "Text", (fun () -> ()),
       (fun background -> Text.configure ~background tex);
       coe scr, "Scrollbar", (fun () -> ()),
       (fun background -> Scrollbar.configure ~background scr)
      ]
  in
    pack ~fill:`X buttons;

(* Main Loop *)
Printexc.print mainLoop ()
