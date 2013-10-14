(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: taquin.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Camltk;;

let découpe_image img nx ny =
  let l = Imagephoto.width img
  and h = Imagephoto.height img in
  let tx = l / nx and ty = h / ny in
  let pièces = ref [] in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      let pièce =
        Imagephoto.create [Width (Pixels tx); Height (Pixels ty)] in
      Imagephoto.copy pièce img
        [ImgFrom(x * tx, y * ty, (x + 1) * tx, (y + 1) * ty)];
      pièces := pièce :: !pièces
    done
  done;
  (tx, ty, List.tl !pièces)
;;

let remplir_taquin c nx ny tx ty pièces =
  let trou_x = ref (nx - 1)
  and trou_y = ref (ny - 1) in
  let trou =
    Canvas.create_rectangle c
      (Pixels (!trou_x * tx)) (Pixels (!trou_y * ty))
      (Pixels tx) (Pixels ty) [] in
  let taquin = Array.make_matrix nx ny trou in
  let p = ref pièces in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      match !p with
      | [] -> ()
      | pièce :: reste ->
          taquin.(x).(y) <-
            Canvas.create_image c
              (Pixels (x * tx)) (Pixels (y * ty))
              [ImagePhoto pièce; Anchor NW; Tags [Tag "pièce"]];
          p := reste
    done
  done;
  let déplacer x y =
    let pièce = taquin.(x).(y) in
    Canvas.coords_set c pièce
      [Pixels (!trou_x * tx); Pixels(!trou_y * ty)];
    Canvas.coords_set c trou
      [Pixels (x * tx); Pixels(y * ty); Pixels tx; Pixels ty];
    taquin.(!trou_x).(!trou_y) <- pièce;
    taquin.(x).(y) <- trou;
    trou_x := x; trou_y := y in
  let jouer ei =
    let x = ei.ev_MouseX / tx and y = ei.ev_MouseY / ty in
    if x = !trou_x && (y = !trou_y - 1 || y = !trou_y + 1)
    || y = !trou_y && (x = !trou_x - 1 || x = !trou_x + 1)
    then déplacer x y in
  Canvas.bind c (Tag "pièce") [[], ButtonPress]
                (BindSet ([Ev_MouseX; Ev_MouseY], jouer));;

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
      (élément, tête :: reste')
;;

let create_filled_text parent lines =
  let lnum = List.length lines
  and lwidth =
    List.fold_right
     (fun line max ->
       let l = String.length line in
       if l > max then l else max)
     lines 1 in
  let txtw = Text.create parent [TextWidth lwidth; TextHeight lnum] in
  List.iter
   (fun line ->
     Text.insert txtw (TextIndex (End, [])) line [];
     Text.insert txtw (TextIndex (End, [])) "\n" [])
   lines;
  txtw
;;

let give_help parent lines () =
 let help_window = Toplevel.create parent [] in
 Wm.title_set help_window "Help";

 let help_frame = Frame.create help_window [] in

 let help_txtw = create_filled_text help_frame lines in

 let quit_help () = destroy help_window in
 let ok_button = Button.create help_frame [Text "Ok"; Command quit_help] in

 pack [help_txtw; ok_button ] [Side Side_Bottom];
 pack [help_frame] []
;;

let taquin nom_fichier nx ny =
  let fp = openTk () in
  Wm.title_set fp "Taquin";
  let img = Imagephoto.create [File nom_fichier] in
  let c =
    Canvas.create fp
     [Width(Pixels(Imagephoto.width img));
      Height(Pixels(Imagephoto.height img))] in
  let (tx, ty, pièces) = découpe_image img nx ny in
  remplir_taquin c nx ny tx ty (permutation pièces);
  pack [c] [];

  let quit = Button.create fp [Text "Quit"; Command closeTk] in
  let help_lines =
   ["Pour jouer, cliquer sur une des pièces";
    "entourant le trou";
    "";
    "To play, click on a part around the hole"] in
  let help =
    Button.create fp [Text "Help"; Command (give_help fp help_lines)] in
  pack [quit; help] [Side Side_Left; Fill Fill_X];
  mainLoop ()
;;

if !Sys.interactive then () else begin taquin "joconde.gif" 3 5; exit 0 end;;
