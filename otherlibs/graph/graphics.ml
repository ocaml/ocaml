(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Graphic_failure of string

(* Initializations *)

let _ =
  Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

external raw_open_graph: string -> unit = "gr_open_graph"
external raw_close_graph: unit -> unit = "gr_close_graph"
external sigio_signal: unit -> int = "gr_sigio_signal"
external sigio_handler: int -> unit = "gr_sigio_handler"

let unix_open_graph arg =
  Sys.set_signal (sigio_signal()) (Sys.Signal_handle sigio_handler);
  raw_open_graph arg

let unix_close_graph () =
  Sys.set_signal (sigio_signal()) Sys.Signal_ignore;
  raw_close_graph ()

let (open_graph, close_graph) =
  match Sys.os_type with
  | "Unix" -> (unix_open_graph, unix_close_graph)
  | "Win32" -> (raw_open_graph, raw_close_graph)
  | "MacOS" -> (raw_open_graph, raw_close_graph)
  | _ -> invalid_arg ("Graphics: unknown OS type: " ^ Sys.os_type)

external clear_graph : unit -> unit = "gr_clear_graph"
external size_x : unit -> int = "gr_size_x"
external size_y : unit -> int = "gr_size_y"

(* Double-buffering *)

external display_mode : bool -> unit = "gr_display_mode"
external remember_mode : bool -> unit = "gr_remember_mode"
external synchronize : unit -> unit = "gr_synchronize"

let auto_synchronize = function
  | true -> display_mode true; remember_mode true; synchronize ()
  | false -> display_mode false; remember_mode true
;;


(* Colors *)

type color = int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

external set_color : color -> unit = "gr_set_color"

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF

let background = white
and foreground = black

(* Drawing *)

external plot : int -> int -> unit = "gr_plot"
external point_color : int -> int -> color = "gr_point_color"
external moveto : int -> int -> unit = "gr_moveto"
external current_point : unit -> int * int = "gr_current_point"
external lineto : int -> int -> unit = "gr_lineto"
external draw_arc : int -> int -> int -> int -> int -> int -> unit
               = "gr_draw_arc" "gr_draw_arc_nat"
let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360
let draw_circle x y r = draw_arc x y r r 0 360
external set_line_width : int -> unit = "gr_set_line_width"

external fill_rect : int -> int -> int -> int -> unit = "gr_fill_rect"
external fill_poly : (int * int) array -> unit = "gr_fill_poly"
external fill_arc : int -> int -> int -> int -> int -> int -> unit
               = "gr_fill_arc" "gr_fill_arc_nat"
let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360

(* Text *)

external draw_char : char -> unit = "gr_draw_char"
external draw_string : string -> unit = "gr_draw_string"
external set_font : string -> unit = "gr_set_font"
external set_text_size : int -> unit = "gr_set_text_size"
external text_size : string -> int * int = "gr_text_size"

(* Images *)

type image

let transp = -1

external make_image : color array array -> image = "gr_make_image"
external dump_image : image -> color array array = "gr_dump_image"
external draw_image : image -> int -> int -> unit = "gr_draw_image"
external create_image : int -> int -> image = "gr_create_image"
external blit_image : image -> int -> int -> unit = "gr_blit_image"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image

(* Events *)

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char }

type event =
    Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

external wait_next_event : event list -> status = "gr_wait_event"

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y)

let button_down () =
  let e = wait_next_event [Poll] in e.button

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed

(*** Sound *)

external sound : int -> int -> unit = "gr_sound"
