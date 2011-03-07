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

open Tk;;

let découpe_image img nx ny =
  let l = Imagephoto.width img
  and h = Imagephoto.height img in
  let tx = l / nx and ty = h / ny in
  let pièces = ref [] in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      let pièce = Imagephoto.create ~width:tx ~height:ty () in
      Imagephoto.copy ~src:img
        ~src_area:(x * tx, y * ty, (x + 1) * tx, (y + 1) * ty) pièce;
      pièces := pièce :: !pièces
    done
  done;
  (tx, ty, List.tl !pièces);;

let remplir_taquin c nx ny tx ty pièces =
  let trou_x = ref (nx - 1)
  and trou_y = ref (ny - 1) in
  let trou =
    Canvas.create_rectangle
      ~x1:(!trou_x * tx) ~y1:(!trou_y * ty) ~x2:tx ~y2:ty c in
  let taquin = Array.make_matrix nx ny trou in
  let p = ref pièces in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      match !p with
      | [] -> ()
      | pièce :: reste ->
          taquin.(x).(y) <-
            Canvas.create_image
                ~x:(x * tx) ~y:(y * ty)
                ~image:pièce ~anchor:`Nw ~tags:["pièce"] c;
          p := reste
    done
  done;
  let déplacer x y =
    let pièce = taquin.(x).(y) in
    Canvas.coords_set c pièce
      ~xys:[!trou_x * tx, !trou_y * ty];
    Canvas.coords_set c trou
      ~xys:[x * tx, y * ty; tx, ty];
    taquin.(!trou_x).(!trou_y) <- pièce;
    taquin.(x).(y) <- trou;
    trou_x := x; trou_y := y in
  let jouer ei =
    let x = ei.ev_MouseX / tx and y = ei.ev_MouseY / ty in
    if x = !trou_x && (y = !trou_y - 1 || y = !trou_y + 1)
    || y = !trou_y && (x = !trou_x - 1 || x = !trou_x + 1)
    then déplacer x y in
  Canvas.bind ~events:[`ButtonPress]
                 ~fields:[`MouseX; `MouseY] ~action:jouer c (`Tag "pièce");;

let rec permutation = function
  | [] -> []
  | l  -> let n = Random.int (List.length l) in
          let (élément, reste) = partage l n in
          élément :: permutation reste

and partage l n =
  match l with
  | [] -> failwith "partage"
  | tête :: reste ->
      if n = 0 then (tête, reste) else
        let (élément, reste') = partage reste (n - 1) in
        (élément, tête :: reste');;

let create_filled_text parent lines =
  let lnum = List.length lines
  and lwidth =
    List.fold_right
     (fun line max ->
       let l = String.length line in
       if l > max then l else max)
     lines 1 in
  let txtw = Text.create ~width:lwidth ~height:lnum parent in
  List.iter
   (fun line ->
        Text.insert ~index:(`End, []) ~text:line txtw;
        Text.insert ~index:(`End, []) ~text:"\n" txtw)
   lines;
  txtw;;

let give_help parent lines () =
 let help_window = Toplevel.create parent in
 Wm.title_set help_window "Help";

 let help_frame = Frame.create help_window in

 let help_txtw = create_filled_text help_frame lines in

 let quit_help () = destroy help_window in
 let ok_button = Button.create ~text:"Ok" ~command:quit_help help_frame in

 pack ~side:`Bottom [help_txtw];
 pack ~side:`Bottom [ok_button ];
 pack [help_frame];;

let taquin nom_fichier nx ny =
  let fp = openTk () in
  Wm.title_set fp "Taquin";
  let img = Imagephoto.create ~file:nom_fichier () in
  let c =
    Canvas.create ~background:`Black
     ~width:(Imagephoto.width img)
     ~height:(Imagephoto.height img) fp in
  let (tx, ty, pièces) = découpe_image img nx ny in
  remplir_taquin c nx ny tx ty (permutation pièces);
  pack [c];

  let quit = Button.create ~text:"Quit" ~command:closeTk fp in
  let help_lines =
   ["Pour jouer, cliquer sur une des pièces";
    "entourant le trou";
    "";
    "To play, click on a part around the hole"] in
  let help =
    Button.create ~text:"Help" ~command:(give_help fp help_lines) fp in
  pack ~side:`Left ~fill:`X [quit] ;
  pack ~side:`Left ~fill:`X [help] ;
  mainLoop ();;

if !Sys.interactive then () else
begin taquin "Lambda2.back.gif" 4 4; exit 0 end;;
