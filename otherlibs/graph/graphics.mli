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

(* Module [Graphics]: machine-independent graphics primitives *)

exception Graphic_failure of string
        (* Raised by the functions below when they encounter an error. *)

(*** Initializations *)

val open_graph: string -> unit
        (* Show the graphics window or switch the screen to graphic mode.
           The graphics window is cleared and the current point is set
           to (0, 0). The string argument is used to pass optional
           information on the desired graphics mode, the graphics window
           size, and so on. Its interpretation is implementation-dependent.
           If the empty string is given, a sensible default is selected. *)
val close_graph: unit -> unit
        (* Delete the graphics window or switch the screen back to
           text mode. *)
external clear_graph : unit -> unit = "gr_clear_graph"
        (* Erase the graphics window. *)
external size_x : unit -> int = "gr_size_x"
external size_y : unit -> int = "gr_size_y"
        (* Return the size of the graphics window. Coordinates of the screen
           pixels range over [0 .. size_x()-1] and [0 .. size_y()-1].
           Drawings outside of this rectangle are clipped, without causing
           an error. The origin (0,0) is at the lower left corner. *)

val window_id : unit -> string
        (* Return the unique identifier of the window allocated by the
           window manager and where Caml graphics are drawn.
           Under X Windows, the returned string is an unsigned 32 bits
           integer in decimal form. *)

(*** Colors *)

type color = int
        (* A color is specified by its R, G, B components. Each component
           is in the range [0..255]. The three components are packed in
           an [int]: [0xRRGGBB], where [RR] are the two hexadecimal digits for
           the red component, [GG] for the green component, [BB] for the
           blue component. *)

val rgb: int -> int -> int -> color
        (* [rgb r g b] returns the integer encoding the color with red
           component [r], green component [g], and blue component [b].
           [r], [g] and [b] are in the range [0..255]. *)

external set_color : color -> unit = "gr_set_color"
        (* Set the current drawing color. *)

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color
        (* Some predefined colors. *)

val background: color
val foreground: color
        (* Default background and foreground colors (usually, either black
           foreground on a white background or white foreground on a
           black background).
           [clear_graph] fills the screen with the [background] color.
           The initial drawing color is [foreground]. *)

(*** Point and line drawing *)

external plot : x:int -> y:int -> unit = "gr_plot"
        (* Plot the given point with the current drawing color. *)
external point_color : x:int -> y:int -> color = "gr_point_color"
        (* Return the color of the given point in the backing store
           (see "Double buffering" below). *)
external moveto : x:int -> y:int -> unit = "gr_moveto"
        (* Position the current point. *)
val rmoveto : dx:int -> dy:int -> unit
        (* [rmoveto dx dy] translates the current point by the given vector. *)
external current_x : unit -> int = "gr_current_x"
        (* Return the abscissa of the current point. *)
external current_y : unit -> int = "gr_current_y"
        (* Return the ordinate of the current point. *)
val current_point : unit -> int * int
        (* Return the position of the current point. *)
external lineto : x:int -> y:int -> unit = "gr_lineto"
        (* Draw a line with endpoints the current point and the given point,
           and move the current point to the given point. *)
val rlineto : dx:int -> dy:int -> unit
        (* Draws a line with endpoints the current point and the
           current point translated of the given vector,
           and move the current point to this point. *)
external draw_rect : x:int -> y:int -> w:int -> h:int -> unit = "gr_draw_rect"
        (* [draw_rect x y w h] draws the rectangle with lower left corner
           at [x,y], width [w] and height [h].
           The current point is unchanged. *)
external draw_arc :
        x:int -> y:int -> rx:int -> ry:int -> start:int -> stop:int -> unit
               = "gr_draw_arc" "gr_draw_arc_nat"
        (* [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
           [x,y], horizontal radius [rx], vertical radius [ry], from angle
           [a1] to angle [a2] (in degrees). The current point is unchanged. *)
val draw_ellipse : x:int -> y:int -> rx:int -> ry:int -> unit
        (* [draw_ellipse x y rx ry] draws an ellipse with center
           [x,y], horizontal radius [rx] and vertical radius [ry].
           The current point is unchanged.  *)
val draw_circle : x:int -> y:int -> r:int -> unit
        (* [draw_circle x y r] draws a circle with center [x,y] and
           radius [r]. The current point is unchanged. *)
external set_line_width : int -> unit = "gr_set_line_width"
        (* Set the width of points and lines drawn with the functions above.
           Under X Windows, [set_line_width 0] selects a width of 1 pixel
           and a faster, but less precise drawing algorithm than the one
           used when [set_line_width 1] is specified. *)

(*** Text drawing *)

external draw_char : char -> unit = "gr_draw_char"
external draw_string : string -> unit = "gr_draw_string"
        (* Draw a character or a character string with lower left corner
           at current position. After drawing, the current position is set
           to the lower right corner of the text drawn. *)
external set_font : string -> unit = "gr_set_font"
val set_text_size : int -> unit
        (* Set the font and character size used for drawing text.
           The interpretation of the arguments to [set_font] and
           [set_text_size] is implementation-dependent. *)
external text_size : string -> int * int = "gr_text_size"
        (* Return the dimensions of the given text, if it were drawn with
           the current font and size. *)

(*** Filling *)

external fill_rect : x:int -> y:int -> w:int -> h:int -> unit = "gr_fill_rect"
        (* [fill_rect x y w h] fills the rectangle with lower left corner
           at [x,y], width [w] and height [h], with the current color. *)
external fill_poly : (int * int) array -> unit = "gr_fill_poly"
        (* Fill the given polygon with the current color. The array
           contains the coordinates of the vertices of the polygon. *)
external fill_arc :
        x:int -> y:int -> rx:int -> ry:int -> start:int -> stop:int -> unit
               = "gr_fill_arc" "gr_fill_arc_nat"
        (* Fill an elliptical pie slice with the current color. The
           parameters are the same as for [draw_arc]. *)
val fill_ellipse : x:int -> y:int -> rx:int -> ry:int -> unit
        (* Fill an ellipse with the current color. The
           parameters are the same as for [draw_ellipse]. *)
val fill_circle : x:int -> y:int -> r:int -> unit
        (* Fill a circle with the current color. The
           parameters are the same as for [draw_circle]. *)

(*** Images *)

type image
        (* The abstract type for images, in internal representation.
           Externally, images are represented as matrices of colors. *)

val transp : color
        (* In matrices of colors, this color represent a ``transparent''
           point: when drawing the corresponding image, all pixels on the
           screen corresponding to a transparent pixel in the image will
           not be modified, while other points will be set to the color
           of the corresponding point in the image. This allows superimposing
           an image over an existing background. *)

external make_image : color array array -> image = "gr_make_image"
        (* Convert the given color matrix to an image.
           Each sub-array represents one horizontal line. All sub-arrays
           must have the same length; otherwise, exception [Graphic_failure]
           is raised. *)
external dump_image : image -> color array array = "gr_dump_image"
        (* Convert an image to a color matrix. *)
external draw_image : image -> x:int -> y:int -> unit = "gr_draw_image"
        (* Draw the given image with lower left corner at the given point. *)
val get_image : x:int -> y:int -> w:int -> h:int -> image
        (* Capture the contents of a rectangle on the screen as an image.
           The parameters are the same as for [fill_rect]. *)
external create_image : w:int -> h:int -> image = "gr_create_image"
        (* [create_image w h] returns a new image [w] pixels wide and [h]
           pixels tall, to be used in conjunction with [blit_image].
           The initial image contents are random, except that no point
           is transparent. *)
external blit_image : image -> x:int -> y:int -> unit = "gr_blit_image"
        (* [blit_image img x y] copies screen pixels into the image [img],
           modifying [img] in-place. The pixels copied are those inside the
           rectangle with lower left corner at [x,y], and width and height
           equal to those of the image. Pixels that were transparent in
           [img] are left unchanged. *)

(*** Mouse and keyboard events *)

type status =
  { mouse_x : int;              (* X coordinate of the mouse *)
    mouse_y : int;              (* Y coordinate of the mouse *)
    button : bool;              (* true if a mouse button is pressed *)
    keypressed : bool;          (* true if a key has been pressed *)
    key : char }                (* the character for the key pressed *)
        (* To report events. *)

type event =
    Button_down                 (* A mouse button is pressed *)
  | Button_up                   (* A mouse button is released *)
  | Key_pressed                 (* A key is pressed *)
  | Mouse_motion                (* The mouse is moved *)
  | Poll                        (* Don't wait; return immediately *)
        (* To specify events to wait for. *)

external wait_next_event : event list -> status = "gr_wait_event"
        (* Wait until one of the events specified in the given event list
           occurs, and return the status of the mouse and keyboard at
           that time. If [Poll] is given in the event list, return immediately
           with the current status. If the mouse cursor is outside of the
           graphics window, the [mouse_x] and [mouse_y] fields of the event are
           outside the range [0..size_x()-1, 0..size_y()-1]. Keypresses
           are queued, and dequeued one by one when the [Key_pressed]
           event is specified. *)

(*** Mouse and keyboard polling *)

val mouse_pos : unit -> int * int
        (* Return the position of the mouse cursor, relative to the
           graphics window. If the mouse cursor is outside of the graphics
           window, [mouse_pos()] returns a point outside of the range
           [0..size_x()-1, 0..size_y()-1]. *)
val button_down : unit -> bool
        (* Return [true] if the mouse button is pressed, [false] otherwise. *)
val read_key : unit -> char
        (* Wait for a key to be pressed, and return the corresponding
           character. Keypresses are queued. *)
val key_pressed : unit -> bool
        (* Return [true] if a keypress is available; that is, if [read_key]
           would not block. *)

(*** Sound *)

external sound : freq:int -> ms:int -> unit = "gr_sound"
        (* [sound freq dur] plays a sound at frequency [freq] (in hertz)
           for a duration [dur] (in milliseconds). *)

(*** Double buffering *)

val auto_synchronize : bool -> unit
        (* By default, drawing takes place both on the window displayed
           on screen, and in a memory area (the ``backing store'').
           The backing store image is used to re-paint the on-screen
           window when necessary.

           To avoid flicker during animations, it is possible to turn
           off on-screen drawing, perform a number of drawing operations
           in the backing store only, then refresh the on-screen window
           explicitly.

           [auto_synchronize false] turns on-screen drawing off.  All
           subsequent drawing commands are performed on the backing store
           only.

           [auto_synchronize true] refreshes the on-screen window from
           the backing store (as per [synchronize]), then turns on-screen
           drawing back on.  All subsequent drawing commands are performed
           both on screen and in the backing store.

           The default drawing mode corresponds to [auto_synchronize true]. *)

external synchronize : unit -> unit = "gr_synchronize"
        (* Synchronize the backing store and the on-screen window, by
           copying the contents of the backing store onto the graphics
           window. *)

external display_mode : bool -> unit = "gr_display_mode"
        (* Set display mode on or off. When turned on, drawings are done
           in the graphics window; when turned off, drawings do not affect
           the graphics window.  This occurs independently of
           drawing into the backing store (see the function [remember_mode]
           below). Default display mode is on. *)

external remember_mode : bool -> unit = "gr_remember_mode"
        (* Set remember mode on or off. When turned on, drawings are done
           in the backing store; when turned off, the backing store is
           unaffected by drawings.  This occurs independently of drawing
           onto the graphics window (see the function [display_mode] above).
           Default remember mode is on.  *)
