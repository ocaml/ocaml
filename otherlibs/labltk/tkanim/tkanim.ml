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
open Widget
open Support
open Protocol
open Tkintf

external init : unit -> unit = "tkanim_init"

type gifFrame = {
  imagephoto : imagePhoto;
  frameWidth : int;
  frameHeight : int;
  left : int;
  top : int;
  delay : int
 }

type animatedGif = {
  frames : gifFrame list;
  animWidth : int;
  animHeight : int;
  loop : int
}

type imageType =
  | Still of Tk.options
  | Animated of animatedGif

let debug = ref false

let cTKtoCAMLgifFrame s =
  match splitlist s with
  | [photo; width; height; left; top; delay] ->
      {imagephoto = cTKtoCAMLimagePhoto photo;
       frameWidth = int_of_string width;
       frameHeight = int_of_string height;
       left = int_of_string left;
       top = int_of_string top;
       delay = int_of_string delay}
  | _ -> raise (Invalid_argument ("cTKtoCAMLgifFrame: " ^ s))

let cTKtoCAMLanimatedGif s =
  match splitlist s with
  | [width; height; frames; loop] ->
      {frames = List.map cTKtoCAMLgifFrame (splitlist frames);
       animWidth = int_of_string width;
       animHeight = int_of_string height;
       loop = int_of_string loop}
  | _ -> raise (Invalid_argument ("cTKtoCAMLgifFrame: " ^ s))

(* check Tkanim package is in the interpreter *)
let available () =
  let packages =
    splitlist (Protocol.tkEval [| TkToken "package";
                                  TkToken "names" |])
  in
  List.mem "Tkanim" packages

let create file =
  let s =
    Protocol.tkEval [| TkToken "animation";
                       TkToken "create";
                       TkToken file |]
  in
  let anmgif = cTKtoCAMLanimatedGif s in
  match anmgif.frames with
  | [] -> raise (TkError "Null frame in a gif ?")
  | [x] -> Still (ImagePhoto x.imagephoto)
  | _ -> Animated anmgif

let delete anim =
  List.iter (fun {imagephoto = i} -> Imagephoto.delete i) anim.frames

let width anm = anm.animWidth
let height anm = anm.animHeight
let images anm = List.map (fun x -> x.imagephoto) anm.frames

let image_existence_check img =
  (* I found there is a bug in Tk (even v8.0a2).                        *)
  (* We can copy from deleted images, Tk never says "it doesn't exist", *)
  (* But just do some operation. And sometimes it causes Seg-fault.     *)
  (* So, before using Imagephoto.copy, I should check the source image  *)
  (* really exists. *)
  try ignore (Imagephoto.height img) with
    TkError s -> prerr_endline ("tkanim: " ^ s); raise (TkError s)

let imagephoto_copy dst src opts =
  image_existence_check src;
  Imagephoto.copy dst src opts

let animate_gen w i anim =
  let length = List.length anim.frames in
  let frames = Array.of_list anim.frames in
  let current = ref 0 in
  let loop = ref anim.loop in
  let f = frames.(!current) in
    imagephoto_copy i f.imagephoto
      [ImgTo (f.left, f.top, f.left + f.frameWidth,
                             f.top + f.frameHeight)];
  let visible = ref true in
  let animated = ref false in
  let timer = ref None in
  (* Loop *)
  let display_current () =
    let f = frames.(!current) in
      imagephoto_copy i f.imagephoto
        [ImgTo (f.left, f.top,
                f.left + f.frameWidth, f.top + f.frameHeight)]
  in
  let rec tick () =
    if not (Winfo.exists w && Winfo.viewable w) then begin
      (* the widget is invisible. stop animation for efficiency *)
      if !debug then prerr_endline "Stopped (Visibility)";
      visible := false;
    end else
      begin
        display_current ();
        let t =
          Timer.add (if f.delay = 0 then 100 else f.delay * 10)
            (fun () ->
               incr current;
               if !current = length then begin
                 current := 0;
                 (* loop check *)
                 if !loop > 1 then begin
                   decr loop;
                   if !loop = 0 then begin
                     if !debug then prerr_endline "Loop end";
                     (* stop *)
                     loop := anim.loop;
                     timer := None
                   end
                 end
               end;
               tick ())
        in
          timer := Some t
      end
  in
  let start () =
    animated := true;
    tick ()
  in
  let stop () =
    match !timer with
    | Some t ->
        Timer.remove t;
        timer := None;
        animated := false
    | None -> ()
  in
  let next () =
    if !timer = None then begin
      incr current;
      if !current = length then current := 0;
      display_current ()
    end
  in
    (* We shouldn't delete the animation here. *)
(*
    bind w [[], Destroy]
      (BindSet ([], (fun _ -> Imagephoto.delete i)));
*)
    bind w [[], Visibility]
      (BindSet ([], (fun _ ->
        if not !visible then begin
          visible := true;
          if !animated then start ()
        end)));
    (function
     | false ->
         if !animated then stop () else start ()
     | true -> next ())

let animate label anim =
  (*  prerr_endline "animate"; *)
  let i = Imagephoto.create [Width (Pixels anim.animWidth);
                             Height (Pixels anim.animHeight)]
  in
    bind label [[], Destroy] (BindExtend ([], (fun _ ->
      Imagephoto.delete i)));
    Label.configure label [ImagePhoto i];
    animate_gen label i anim

let animate_canvas_item canvas tag anim =
(*  prerr_endline "animate"; *)
  let i = Imagephoto.create [Width (Pixels anim.animWidth);
                             Height (Pixels anim.animHeight)]
  in
    bind canvas [[], Destroy] (BindExtend ([], (fun _ ->
      Imagephoto.delete i)));
    Canvas.configure_image canvas tag [ImagePhoto i];
    animate_gen canvas i anim

let gifdata s =
  let tmp_dir = ref Filename.temp_dir_name in
  let mktemp =
    let cnter = ref 0
    and pid = Unix.getpid() in
      (function prefx ->
               incr cnter;
               (Filename.concat !tmp_dir
               (prefx ^ string_of_int pid ^ "." ^ string_of_int !cnter)))
  in
    let fname = mktemp "gifdata" in
    let oc = open_out_bin fname in
      try
        output_string oc s;
        close_out oc;
        let anim = create fname in
          Unix.unlink fname;
          anim
      with
        e -> begin Unix.unlink fname; raise e end

