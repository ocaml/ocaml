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
open Camltk
open Protocol

let rec mapi f n l =
  match l with
    [] -> [] 
  | x::l -> let v = f n x in v::(mapi f (succ n) l)

(* Same as tk_dialog, but not sharing the tkwait variable *)
(* w IS the parent widget *)
let f w name title mesg bitmap def buttons =
  let t = Toplevel.create_named w name [Class "Dialog"] in
    Wm.title_set t title;
    Wm.iconname_set t "Dialog";
    Wm.protocol_set t "WM_DELETE_WINDOW" (function () -> ());
    (* Wm.transient_set t (Winfo.toplevel w); *)
  let ftop = 
   Frame.create_named t "top" [Relief Raised; BorderWidth (Pixels 1)]
  and fbot =
   Frame.create_named t "bot" [Relief Raised; BorderWidth (Pixels 1)]
   in
     pack [ftop][Side Side_Top; Fill Fill_Both];
     pack [fbot][Side Side_Bottom; Fill Fill_Both];

  let l =
   Label.create_named ftop "msg" 
     [Justify Justify_Left; Text mesg; WrapLength (Pixels 600)] in
     pack [l][Side Side_Right; Expand true; Fill Fill_Both;
              PadX (Millimeters 3.0); PadY (Millimeters 3.0)];
  begin match bitmap with
     Predefined "" -> ()
  |  _ ->
    let b = 
      Label.create_named ftop "bitmap" [Bitmap bitmap] in
     pack [b][Side Side_Left; PadX (Millimeters 3.0); PadY (Millimeters 3.0)]
  end;
  
  let waitv = Textvariable.create_temporary t in
 
  let buttons =
    mapi (fun i bname ->
     let b = Button.create t 
              [Text bname; 
               Command (fun () -> Textvariable.set waitv (string_of_int i))] in
    if i = def then begin
      let f = Frame.create_named fbot "default" 
                 [Relief Sunken; BorderWidth (Pixels 1)] in
        raise_window_above b f;
        pack [f][Side Side_Left; Expand true; 
                 PadX (Millimeters 3.0); PadY (Millimeters 2.0)];
        pack [b][In f; PadX (Millimeters 2.0); PadY (Millimeters 2.0)];
        bind t [[], KeyPressDetail "Return"]
         (BindSet ([], (fun _ -> Button.flash b; Button.invoke b)))
        end
    else
      pack [b][In fbot; Side Side_Left; Expand true; 
               PadX (Millimeters 3.0); PadY (Millimeters 2.0)];
    b
    )
    0 buttons in

   Wm.withdraw t;
   update_idletasks();
   let x = (Winfo.screenwidth t)/2 - (Winfo.reqwidth t)/2 -
             (Winfo.vrootx (Winfo.parent t))
   and y = (Winfo.screenheight t)/2 - (Winfo.reqheight t)/2 -
             (Winfo.vrooty (Winfo.parent t)) in
   Wm.geometry_set t (Printf.sprintf "+%d+%d" x y);
   Wm.deiconify t;

   let oldfocus = try Some (Focus.get()) with _ -> None
   and oldgrab = Grab.current ~displayof: t ()
   and grabstatus = ref None in
    begin match oldgrab with 
      [] -> ()
    | x::l -> grabstatus := Some(Grab.status x)
    end;

   (* avoid errors here because it makes the entire app useless *)
   (try Grab.set t with TkError _ -> ());
   Tkwait.visibility t;
   Focus.set (if def >= 0 then List.nth buttons def else t);

   Tkwait.variable waitv;
   begin match oldfocus with
       None -> ()
     | Some w -> try Focus.set w with _ -> ()
   end;
   destroy t;
   begin match oldgrab with
     [] -> ()
   | x::l -> 
      try
        match !grabstatus with
          Some(GrabGlobal) -> Grab.set_global x
        | _ -> Grab.set x
      with TkError _ -> ()
   end;

   int_of_string (Textvariable.get waitv)
