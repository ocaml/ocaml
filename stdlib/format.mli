(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Format]: pretty printing *)

(* This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box
   structure. *)

(* Rule of thumb for casual users:
   use simple boxes (as obtained by [open_box 0]);
   use simple break hints (as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that ouputs a space
   indicating a break hint);
   once a box is opened, display its material with basic printing
   functions (e. g. [print_int] and [print_string]);
   when the material for a box has been printed, call [close_box ()] to
   close the box;
   at the end of your routine, evaluate [print_newline ()] to close
   all remaining boxes and flush the pretty-printer. *) 

(* The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. Each box opened via
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly. *)

(* In case of interactive use, the system closes all opened boxes and
   flushes all pending text (as with the [print_newline] function)
   after each phrase. Each phrase is therefore executed in the initial
   state of the pretty-printer. *)

(*** Boxes *)
val open_hbox : unit -> unit
        (* [open_hbox ()] opens a new pretty-printing box.
           This box is ``horizontal'': the line is not split in this box
           (new lines may still occur inside boxes nested deeper). *)
val open_vbox : int -> unit
        (* [open_vbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``vertical'': every break hint inside this
           box leads to a new line.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
val open_hvbox : int -> unit
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal-vertical'': it behaves as an
           ``horizontal'' box if it fits on a single line,
           otherwise it behaves as a ``vertical'' box.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
val open_hovbox : int -> unit
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal or vertical'': break hints
           inside this box may lead to a new line, if there is no more room
           on the line to print the remainder of the box.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
val open_box : int -> unit
        (* [open_box d] opens a new pretty-printing box
           with offset [d]. 
           This box is the general purpose pretty-printing box.
           Material in this box is displayed ``horizontal or vertical'':
           break hints inside the box may lead to a new line, if there
           is no more room on the line to print the remainder of the box,
           or if a new line may lead to a new indentation
           (demonstrating the indentation of the box).
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
val close_box : unit -> unit
        (* Close the most recently opened pretty-printing box. *)

(*** Formatting functions *)
val print_string : string -> unit
        (* [print_string str] prints [str] in the current box. *)
val print_as : int -> string -> unit
        (* [print_as len str] prints [str] in the
           current box. The pretty-printer formats [str] as if
           it were of length [len]. *)
val print_int : int -> unit
        (* Print an integer in the current box. *)
val print_float : float -> unit
        (* Print a floating point number in the current box. *)
val print_char : char -> unit
        (* Print a character in the current box. *)
val print_bool : bool -> unit
        (* Print an boolean in the current box. *)

(*** Break hints *)
val print_break : int -> int -> unit
        (* Insert a break hint in a pretty-printing box.
           [print_break nspaces offset] indicates that the line may
           be split (a newline character is printed) at this point,
           if the contents of the current box does not fit on one line.
           If the line is split at that point, [offset] is added to
           the current indentation. If the line is not split,
           [nspaces] spaces are printed. *)
val print_cut : unit -> unit
        (* [print_cut ()] is equivalent to [print_break 0 0].
           This allows line splitting at the current point, without printing
           spaces or adding indentation. *)
val print_space : unit -> unit
        (* [print_space ()] is equivalent to [print_break 1 0].
           This either prints one space or splits the line at that point. *)
val force_newline : unit -> unit
        (* Force a newline in the current box. *)

val print_flush : unit -> unit
        (* Flush the pretty printer: all opened boxes are closed,
           and all pending text is displayed. *)
val print_newline : unit -> unit
        (* Equivalent to [print_flush] followed by a new line. *)

val print_if_newline : unit -> unit
        (* Execute the next formatting command if the preceding line
           has just been split. Otherwise, ignore the next formatting
           command. *)

(*** Tabulations *)
val open_tbox : unit -> unit
        (* Open a tabulation box. *)
val close_tbox : unit -> unit
        (* Close the most recently opened tabulation box. *)
val print_tbreak : int -> int -> unit
        (* Break hint in a tabulation box.
           [print_tbreak spaces offset] moves the insertion point to
           the next tabulation ([spaces] being added to this position).
           Nothing occurs if insertion point is already on a
           tabulation mark.
           If there is no next tabulation on the line, then a newline
           is printed and the insertion point moves to the first
           tabulation of the box.
           If a new line is printed, [offset] is added to the current
           indentation. *)
val set_tab : unit -> unit
        (* Set a tabulation mark at the current insertion point. *)
val print_tab : unit -> unit
        (* [print_tab ()] is equivalent to [print_tbreak (0,0)]. *)

(*** Margin *)
val set_margin : int -> unit
        (* [set_margin d] sets the value of the right margin
           to [d] (in characters): this value is used to detect line
           overflows that leads to split lines.
           Nothing happens if [d] is smaller than 2 or
           bigger than 999999999. *)
val get_margin : unit -> int
        (* Return the position of the right margin. *)

(*** Maximum indentation limit *)
val set_max_indent : int -> unit
        (* [set_max_indent d] sets the value of the maximum
           indentation limit to [d] (in characters):
           once this limit is reached, boxes are rejected to the left,
           if they do not fit on the current line.
           Nothing happens if [d] is smaller than 2 or
           bigger than 999999999. *)
val get_max_indent : unit -> int
        (* Return the value of the maximum indentation limit (in
           characters). *)

(*** Formatting depth: maximum number of boxes allowed before ellipsis *)
val set_max_boxes : int -> unit
        (* [set_max_boxes max] sets the maximum number
           of boxes simultaneously opened.
           Material inside boxes nested deeper is printed as an
           ellipsis (more precisely as the text returned by
           [get_ellipsis_text ()]).
           Nothing happens if [max] is not greater than 1. *)
val get_max_boxes : unit -> int
        (* Return the maximum number of boxes allowed before ellipsis. *)

(*** Ellipsis *)
val set_ellipsis_text : string -> unit
        (* Set the text of the ellipsis printed when too many boxes
           are opened (a single dot, [.], by default). *)
val get_ellipsis_text : unit -> string
        (* Return the text of the ellipsis. *)

(*** Redirecting formatter output *)
val set_formatter_out_channel : out_channel -> unit
        (* Redirect the pretty-printer output to the given channel. *)

val set_formatter_output_functions :
      (string -> int -> int -> unit) -> (unit -> unit) -> unit;;
        (* [set_formatter_output_functions out flush] redirects the
           pretty-printer output to the functions [out] and [flush].
           The [out] function performs the pretty-printer output.
           It is called with a string [s], a start position [p],
           and a number of characters [n]; it is supposed to output
           characters [p] to [p+n-1] of [s]. The [flush] function is
           called whenever the pretty-printer is flushed using
           [print_flush] or [print_newline]. *)
val get_formatter_output_functions :
        unit -> (string -> int -> int -> unit) * (unit -> unit);;
        (* Return the current output functions of the pretty-printer. *)

(*** Multiple formatted output *)
type formatter;;
        (* Abstract data type corresponding to a pretty-printer and
           all its machinery.
           Defining new pretty-printers permits the output of
           material in parallel on several channels.
           Parameters of the pretty-printer are local to the pretty-printer:
           margin, maximum indentation limit, maximum number of boxes
           simultaneously opened, ellipsis, and so on, are specific to
           each pretty-printer and may be fixed independantly. *)

val std_formatter : formatter;;
        (* The standard formatter used by the formatting functions
           above. *)

val make_formatter :
        (string -> int -> int -> unit) -> (unit -> unit) -> formatter;;
        (* Return a new formatter that writes according to the
           output functions given as argument. *)

val pp_open_hbox : formatter -> unit -> unit;;
val pp_open_vbox : formatter -> int -> unit;;
val pp_open_hvbox : formatter -> int -> unit;;
val pp_open_hovbox : formatter -> int -> unit;;
val pp_open_box : formatter -> int -> unit;;
val pp_close_box : formatter -> unit -> unit;;
val pp_print_string : formatter -> string -> unit;;
val pp_print_as : formatter -> int -> string -> unit;;
val pp_print_int : formatter -> int -> unit;;
val pp_print_float : formatter -> float -> unit;;
val pp_print_char : formatter -> char -> unit;;
val pp_print_bool : formatter -> bool -> unit;;
val pp_print_break : formatter -> int -> int -> unit;;
val pp_print_cut : formatter -> unit -> unit;;
val pp_print_space : formatter -> unit -> unit;;
val pp_force_newline : formatter -> unit -> unit;;
val pp_print_flush : formatter -> unit -> unit;;
val pp_print_newline : formatter -> unit -> unit;;
val pp_print_if_newline : formatter -> unit -> unit;;
val pp_open_tbox : formatter -> unit -> unit;;
val pp_close_tbox : formatter -> unit -> unit;;
val pp_print_tbreak : formatter -> int -> int -> unit;;
val pp_set_tab : formatter -> unit -> unit;;
val pp_print_tab : formatter -> unit -> unit;;
val pp_set_margin : formatter -> int -> unit;;
val pp_get_margin : formatter -> unit -> int;;
val pp_set_max_indent : formatter -> int -> unit;;
val pp_get_max_indent : formatter -> unit -> int;;
val pp_set_max_boxes : formatter -> int -> unit;;
val pp_get_max_boxes : formatter -> unit -> int;;
val pp_set_ellipsis_text : formatter -> string -> unit;;
val pp_get_ellipsis_text : formatter -> unit -> string;;
val pp_set_formatter_out_channel : formatter -> out_channel -> unit;;
val pp_set_formatter_output_functions : formatter ->
        (string -> int -> int -> unit) -> (unit -> unit) -> unit;;
val pp_get_formatter_output_functions :
        formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit);;
        (* The basic functions to use with formatters.
           These functions are the basic ones: usual functions
           operating on the standard formatter are defined via partial
           evaluation of these primitives. For instance,
           [print_string] is equal to [pp_print_string std_formatter]. *)
