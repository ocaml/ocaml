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

(* A simple calculator demonstrating OO programming with O'Labl
   and LablTk.

   LablTk itself is not OO, but it is good to wrap complex
   structures in objects. Even if the absence of initializers
   makes things a little bit awkward.
*)

open StdLabels
open Tk

let mem_string ~elt:c s =
  try
    for i = 0 to String.length s -1 do
      if s.[i] = c then raise Exit
    done; false
  with Exit -> true

let ops = ['+',(+.); '-',(-.); '*',( *.); '/',(/.)]

(* The abstract calculator class.
   Does not use Tk (only Textvariable) *)

class calc () = object (calc)
  val variable = Textvariable.create ()
  val mutable x = 0.0
  val mutable op = None
  val mutable displaying = true

  method set = Textvariable.set variable
  method get = Textvariable.get variable
  method insert s = calc#set (calc#get ^ s)
  method get_float = float_of_string (calc#get)

  method command s =
    if s <> "" then match s.[0] with
      '0'..'9' ->
        if displaying then (calc#set ""; displaying <- false);
        calc#insert s
    | '.' ->
        if displaying then
          (calc#set "0."; displaying <- false)
        else
          if not (mem_string ~elt:'.' calc#get) then calc#insert s
    | '+'|'-'|'*'|'/' as c ->
        displaying <- true;
        begin match op with
          None ->
            x <- calc#get_float;
            op <- Some (List.assoc c ops)
        | Some f ->
            x <- f x (calc#get_float);
            op <- Some (List.assoc c ops);
            calc#set (Printf.sprintf "%g" x)
        end
    | '='|'\n'|'\r' ->
        displaying <- true;
        begin match op with
          None -> ()
        | Some f ->
            x <- f x (calc#get_float);
            op <- None;
            calc#set (Printf.sprintf "%g" x)
        end
    | 'q' -> closeTk (); exit 0
    | _ -> ()
end

(* Buttons for the calculator *)

let m =
  [|["7";"8";"9";"+"];
    ["4";"5";"6";"-"];
    ["1";"2";"3";"*"];
    ["0";".";"=";"/"]|]

(* The physical calculator. Inherits from the abstract one *)

class calculator ~parent = object
  inherit calc () as calc

  val label = Label.create ~anchor:`E ~relief:`Sunken ~padx:10 parent
  val frame = Frame.create parent

  initializer
    let buttons =
      Array.map ~f:
        (List.map ~f:
           (fun text ->
             Button.create ~text ~command:(fun () -> calc#command text) frame))
        m
    in
    Label.configure ~textvariable:variable label;
    calc#set "0";
    bind ~events:[`KeyPress] ~fields:[`Char]
      ~action:(fun ev -> calc#command ev.ev_Char)
      parent;
    for i = 0 to Array.length m - 1 do
      Grid.configure ~row:i buttons.(i)
    done;
    pack ~side:`Top ~fill:`X [label];
    pack ~side:`Bottom ~fill:`Both ~expand:true [frame];
end

(* Finally start everything *)

let top = openTk ()

let applet = new calculator ~parent:top

let _ = mainLoop ()
