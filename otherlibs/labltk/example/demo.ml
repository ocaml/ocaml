(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

(* Some CamlTk4 Demonstration by JPF *)

(* First, open these modules for convenience *)
open Tk

(* Dummy let *)
let _ =

(* Initialize Tk *)
let top = openTk () in 
(* Title setting *)
Wm.title_set top title:"LablTk demo";

(* Base frame *)
let base = Frame.create top in
pack [base];

(* Menu bar *)
let bar = Frame.create base borderwidth: 2 relief: `Raised  in 
pack [bar] fill: `X;

  (* Menu and Menubutton *)
  let meb = Menubutton.create bar text: "Menu" in
  let men = Menu.create meb in
  Menu.add_command men label: "Quit" command: (fun () -> closeTk (); exit 0);
  Menubutton.configure meb menu: men; 
  
  (* Frames *)
  let base2 = Frame.create base in
  let left = Frame.create base2 in
  let right = Frame.create base2 in
  pack [base2];
  pack [left; right] side: `Left;
  
    (* Widgets on left and right *)
    
    (* Button *)
    let but = Button.create left text: "Welcome to LablTk" in
    
    (* Canvas *)
    let can = Canvas.create left width: 100
      height: 100 borderwidth: 1 relief: `Sunken
    in
    Canvas.create_oval can x1: 10 y1: 10 
                           x2: 90 y2: 90
                           fill:`Red; 

    (* Check button *)
    let che = Checkbutton.create left text: "Check" in
    
    (* Entry *)
    let ent = Entry.create left width: 10 in 
    
    (* Label *)
    let lab = Label.create left text: "Welcome to LablTk" in
    
    (* Listbox *)
    let lis = Listbox.create left in
    Listbox.insert lis index: `End texts: ["This"; "is"; "Listbox"];
    
    (* Message *)
    let mes = Message.create left
      text: "Hello this is a message widget with very long text, but ..." in
    
    (* Radio buttons *)
    let tv = Textvariable.create () in
    Textvariable.set tv to: "One";
    let radf = Frame.create right in
    let rads = List.map
        fun:(fun t -> Radiobutton.create radf text: t value: t variable: tv)
        ["One"; "Two"; "Three"] in
    
    (* Scale *)
    let sca = Scale.create right label: "Scale" length: 100
      showvalue: true in
    
    (* Text and scrollbar *)
    let texf = Frame.create right in 
    
      (* Text *)
      let tex = Text.create texf width: 20 height: 8 in
      Text.insert tex text: "This is a text widget." index: (`End,[])
         tags: [];
      
      (* Scrollbar *)
      let scr = Scrollbar.create texf in
      
      (* Text and Scrollbar widget link *)
      let scroll_link sb tx =
        Text.configure tx yscrollcommand: (Scrollbar.set sb);
        Scrollbar.configure sb command: (Text.yview tx) in
      scroll_link scr tex;
      
      pack [scr] side: `Right fill: `Y;
      pack [tex] side: `Left fill: `Both expand: true;
       
    (* Pack them *)
    pack [meb] side: `Left;
    pack [coe but; coe can; coe che; coe ent; coe lab; coe lis; coe mes]; 
    pack [coe radf; coe sca; coe texf];
    pack rads;

  (* Toplevel *)
  let top2 = Toplevel.create top in
  Wm.title_set top2 title:"LablTk demo control";
  let defcol = `Color "#dfdfdf" in
  let selcol = `Color "#ffdfdf" in
  let buttons = 
    List.map fun:(fun (w, t, c, a) ->
        let b = Button.create top2 text:t command:c in
        bind b events: [`Enter] action:(fun _ -> a selcol);
        bind b events: [`Leave] action:(fun _ -> a defcol);
        b)
      [coe bar, "Frame", (fun () -> ()),
       (fun background -> Frame.configure bar :background);
       coe meb, "Menubutton", (fun () -> ()),
       (fun background -> Menubutton.configure meb :background);
       coe but, "Button", (fun () -> ()),
       (fun background -> Button.configure but :background);
       coe can, "Canvas", (fun () -> ()),
       (fun background -> Canvas.configure can :background);
       coe che, "CheckButton", (fun () -> ()),
       (fun background -> Checkbutton.configure che :background);
       coe ent, "Entry", (fun () -> ()),
       (fun background -> Entry.configure ent :background);
       coe lab, "Label", (fun () -> ()),
       (fun background -> Label.configure lab :background);
       coe lis, "Listbox", (fun () -> ()),
       (fun background -> Listbox.configure lis :background);
       coe mes, "Message", (fun () -> ()),
       (fun background -> Message.configure mes :background);
       coe radf, "Radiobox", (fun () -> ()),
       (fun background ->
         List.iter rads fun:(fun b -> Radiobutton.configure b :background));
       coe sca, "Scale", (fun () -> ()),
       (fun background -> Scale.configure sca :background);
       coe tex, "Text", (fun () -> ()),
       (fun background -> Text.configure tex :background);
       coe scr, "Scrollbar", (fun () -> ()),
       (fun background -> Scrollbar.configure scr :background)
      ]
  in
    pack buttons fill: `X;

(* Main Loop *)
Printexc.print mainLoop () 

