(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printing.

   If you are new to this module, see the {{!examples} examples} below.

   This module implements a pretty-printing facility to format values
   within {{!boxes}'pretty-printing boxes'} and {{!tags}'semantic tags'}
   combined with a set of {{!fpp}printf-like functions}.
   The pretty-printer splits lines at specified {{!breaks}break hints},
   and indents lines according to the box structure.
   Similarly, {{!tags}semantic tags} can be used to decouple text
   presentation from its contents.

   This pretty-printing facility is implemented as an overlay on top of
   abstract {{!section:formatter}formatters} which provide basic output
   functions.
   Some formatters are predefined, notably:
   - {!std_formatter} outputs to {{!Stdlib.stdout}stdout}
   - {!err_formatter} outputs to {{!Stdlib.stderr}stderr}

   Most functions in the {!Format} module come in two variants: a short version
   that operates on the current domain's standard formatter as obtained using
   {!get_std_formatter} and the generic version prefixed by [pp_] that takes a
   formatter as its first argument. For the version that operates on the
   current domain's standard formatter, the call to {!get_std_formatter} is
   delayed until the last argument is received.

   More formatters can be created with {!formatter_of_out_channel},
   {!formatter_of_buffer}, {!formatter_of_symbolic_output_buffer} or using
   {{!section:formatter}custom formatters}.

   {b Warning}: Since {{!section:formatter}formatters} contain
   mutable state, it is not thread-safe to use the same formatter on multiple
   domains in parallel without synchronization.

   If multiple domains write to the same output channel using the
   predefined formatters (as obtained by {!get_std_formatter} or
   {!get_err_formatter}), the output from the domains will be interleaved with
   each other at points where the formatters are flushed, such as with
   {!print_flush}. This synchronization is not performed by formatters obtained
   from {!formatter_of_out_channel} (on the standard out channels or others).
*)

(** {1 Introduction}

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line splitting. The addition of
   pretty-printing annotations to your regular [printf] format strings gives
   you fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit pretty-printing box management and printing
   functions provided by this module. This style is more basic but more
   verbose than the concise [fprintf] format strings.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space ();
    print_int 1; close_box (); print_newline ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]@." "x =" 1], or even shorter
   [printf "@[x =@ %i@]@." 1].

   Rule of thumb for casual users of this library:
 - use simple pretty-printing boxes (as obtained by [open_box 0]);
 - use simple break hints as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint;
 - once a pretty-printing box is open, display its material with basic
   printing functions (e. g. [print_int] and [print_string]);
 - when the material for a pretty-printing box has been printed, call
   [close_box ()] to close the box;
 - at the end of pretty-printing, flush the pretty-printer to display all
   the remaining material, e.g. evaluate [print_newline ()].

   The behavior of pretty-printing commands is unspecified
   if there is no open pretty-printing box. Each box opened by
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, each phrase is executed in the initial state
   of the standard pretty-printer: after each phrase execution, the
   interactive system closes all open pretty-printing boxes, flushes all
   pending text, and resets the standard pretty-printer.

   Warning: mixing calls to pretty-printing functions of this module with
   calls to {!Stdlib} low level output functions is error prone.

   The pretty-printing functions output material that is delayed in the
   pretty-printer queue and stacks in order to compute proper line
   splitting. In contrast, basic I/O output functions write directly in
   their output device. As a consequence, the output of a basic I/O function
   may appear before the output of a pretty-printing function that has been
   called before. For instance,
   [
    Stdlib.print_string "<";
    Format.print_string "PRETTY";
    Stdlib.print_string ">";
    Format.print_string "TEXT";
   ]
   leads to output [<>PRETTYTEXT].
*)

(* A tutorial to the Format module is provided at {!Format_tutorial}. *)

(** {1 Formatters} *)

type formatter
(** Abstract data corresponding to a pretty-printer (also called a
    formatter) and all its machinery. See also {!section:formatter}. *)

(** {1:boxes Pretty-printing boxes} *)

(** The pretty-printing engine uses the concepts of pretty-printing box and
  break hint to drive indentation and line splitting behavior of the
  pretty-printer.

  Each different pretty-printing box kind introduces a specific line splitting
  policy:

  - within an {e horizontal} box, break hints never split the line (but the
    line may be split in a box nested deeper),
  - within a {e vertical} box, break hints always split the line,
  - within an {e horizontal/vertical} box, if the box fits on the current line
    then break hints never split the line, otherwise break hint always split
    the line,
  - within a {e compacting} box, a break hint never splits the line,
    unless there is no more room on the current line.

  Note that line splitting policy is box specific: the policy of a box does
  not rule the policy of inner boxes. For instance, if a vertical box is
  nested in an horizontal box, all break hints within the vertical box will
  split the line.

  Moreover, opening a box after the {{!maxindent}maximum indentation limit}
  splits the line whether or not the box would end up fitting on the line.

*)

val pp_open_box : formatter -> int -> unit
val open_box : int -> unit
(** [pp_open_box ppf d] opens a new compacting pretty-printing box with
    offset [d] in the formatter [ppf].

   Within this box, the pretty-printer prints as much as possible material on
   every line.

   A break hint splits the line if there is no more room on the line to
   print the remainder of the box.

   Within this box, the pretty-printer emphasizes the box structure:
   if a structural box does not fit fully on a simple line, a break
   hint also splits the line if the splitting ``moves to the left''
   (i.e. the new line gets an indentation smaller than the one of the current
   line).

   This box is the general purpose pretty-printing box.

   If the pretty-printer splits the line in the box, offset [d] is added to
   the current indentation.
*)


val pp_close_box : formatter -> unit -> unit
val close_box : unit -> unit
(** Closes the most recently open pretty-printing box. *)

val pp_open_hbox : formatter -> unit -> unit
val open_hbox : unit -> unit
(** [pp_open_hbox ppf ()] opens a new 'horizontal' pretty-printing box.

  This box prints material on a single line.

  Break hints in a horizontal box never split the line.
  (Line splitting may still occur inside boxes nested deeper).
*)

val pp_open_vbox : formatter -> int -> unit
val open_vbox : int -> unit
(** [pp_open_vbox ppf d] opens a new 'vertical' pretty-printing box
  with offset [d].

  This box prints material on as many lines as break hints in the box.

  Every break hint in a vertical box splits the line.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hvbox : formatter -> int -> unit
val open_hvbox : int -> unit
(** [pp_open_hvbox ppf d] opens a new 'horizontal/vertical' pretty-printing box
  with offset [d].

  This box behaves as an horizontal box if it fits on a single line,
  otherwise it behaves as a vertical box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hovbox : formatter -> int -> unit
val open_hovbox : int -> unit
(** [pp_open_hovbox ppf d] opens a new 'horizontal-or-vertical'
  pretty-printing box with offset [d].

  This box prints material as much as possible on every line.

  A break hint splits the line if there is no more room on the line to
  print the remainder of the box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

(** {1 Formatting functions} *)

val pp_print_string : formatter -> string -> unit
val print_string : string -> unit
(** [pp_print_string ppf s] prints [s] in the current pretty-printing box. *)

val pp_print_bytes : formatter -> bytes -> unit
val print_bytes : bytes -> unit
(** [pp_print_bytes ppf b] prints [b] in the current pretty-printing box.
    @since 4.13.0
*)

val pp_print_as : formatter -> int -> string -> unit
val print_as : int -> string -> unit
(** [pp_print_as ppf len s] prints [s] in the current pretty-printing box.
  The pretty-printer formats [s] as if it were of length [len].
*)

val pp_print_int : formatter -> int -> unit
val print_int : int -> unit
(** Print an integer in the current pretty-printing box. *)

val pp_print_float : formatter -> float -> unit
val print_float : float -> unit
(** Print a floating point number in the current pretty-printing box. *)

val pp_print_char : formatter -> char -> unit
val print_char : char -> unit
(** Print a character in the current pretty-printing box. *)

val pp_print_bool : formatter -> bool -> unit
val print_bool : bool -> unit
(** Print a boolean in the current pretty-printing box. *)

(** {1:breaks Break hints} *)

(** A 'break hint' tells the pretty-printer to output some space or split the
  line whichever way is more appropriate to the current pretty-printing box
  splitting rules.

  Break hints are used to separate printing items and are mandatory to let
  the pretty-printer correctly split lines and indent items.

  Simple break hints are:
  - the 'space': output a space or split the line if appropriate,
  - the 'cut': split the line if appropriate.

  Note: the notions of space and line splitting are abstract for the
  pretty-printing engine, since those notions can be completely redefined
  by the programmer.
  However, in the pretty-printer default setting, ``output a space'' simply
  means printing a space character (ASCII code 32) and ``split the line''
  means printing a newline character (ASCII code 10).
*)

val pp_print_space : formatter -> unit -> unit
val print_space : unit -> unit
(** [pp_print_space ppf ()] emits a 'space' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints one space.

  [pp_print_space ppf ()] is equivalent to [pp_print_break ppf 1 0].
*)

val pp_print_cut : formatter -> unit -> unit
val print_cut : unit -> unit
(** [pp_print_cut ppf ()] emits a 'cut' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints nothing.

  [pp_print_cut ppf ()] is equivalent to [pp_print_break ppf 0 0].
*)

val pp_print_break : formatter -> int -> int -> unit
val print_break : int -> int -> unit
(** [pp_print_break ppf nspaces offset] emits a 'full' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints [nspaces] spaces.

  If the pretty-printer splits the line, [offset] is added to
  the current indentation.
*)

val pp_print_custom_break :
  formatter ->
  fits:(string * int * string) ->
  breaks:(string * int * string) ->
  unit
(** [pp_print_custom_break ppf ~fits:(s1, n, s2) ~breaks:(s3, m, s4)] emits a
   custom break hint: the pretty-printer may split the line at this point.

   If it does not split the line, then the [s1] is emitted, then [n] spaces,
   then [s2].

   If it splits the line, then it emits the [s3] string, then an indent
   (according to the box rules), then an offset of [m] spaces, then the [s4]
   string.

   While [n] and [m] are handled by [formatter_out_functions.out_indent], the
   strings will be handled by [formatter_out_functions.out_string]. This allows
   for a custom formatter that handles indentation distinctly, for example,
   outputs [<br/>] tags or [&nbsp;] entities.

   The custom break is useful if you want to change which visible
   (non-whitespace) characters are printed in case of break or no break. For
   example, when printing a list [ [a; b; c] ], you might want to add a
   trailing semicolon when it is printed vertically:

   {[
[
  a;
  b;
  c;
]
   ]}

   You can do this as follows:
   {[
printf "@[<v 0>[@;<0 2>@[<v 0>a;@,b;@,c@]%t]@]@\n"
  (pp_print_custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))
   ]}

  @since 4.08.0
*)

val pp_force_newline : formatter -> unit -> unit
val force_newline : unit -> unit
(** Force a new line in the current pretty-printing box.

  The pretty-printer must split the line at this point,

  Not the normal way of pretty-printing, since imperative line splitting may
  interfere with current line counters and box size calculation.
  Using break hints within an enclosing vertical box is a better
  alternative.
*)

val pp_print_if_newline : formatter -> unit -> unit
val print_if_newline : unit -> unit
(** Execute the next formatting command if the preceding line
  has just been split. Otherwise, ignore the next formatting
  command.
*)

(** {1 Pretty-printing termination} *)

val pp_print_flush : formatter -> unit -> unit
val print_flush : unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.
  In addition, the pretty-printer low level output device is flushed to
  ensure that all pending text is really displayed.

  Note: never use [print_flush] in the normal course of a pretty-printing
  routine, since the pretty-printer uses a complex buffering machinery to
  properly indent the output; manually flushing those buffers at random
  would conflict with the pretty-printer strategy and result to poor
  rendering.

  Only consider using [print_flush] when displaying all pending material is
  mandatory (for instance in case of interactive use when you want the user
  to read some text) and when resetting the pretty-printer state will not
  disturb further pretty-printing.

  Warning: If the output device of the pretty-printer is an output channel,
  repeated calls to [print_flush] means repeated calls to {!Stdlib.flush}
  to flush the out channel; these explicit flush calls could foil the
  buffering strategy of output channels and could dramatically impact
  efficiency.
*)

val pp_print_newline : formatter -> unit -> unit
val print_newline : unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.

  Equivalent to {!print_flush} followed by a new line.
  See corresponding words of caution for {!print_flush}.

  Note: this is not the normal way to output a new line;
  the preferred method is using break hints within a vertical pretty-printing
  box.
*)

(** {1 Margin} *)

val pp_set_margin : formatter -> int -> unit
val set_margin : int -> unit
(** [pp_set_margin ppf d] sets the right margin to [d] (in characters):
  the pretty-printer splits lines that overflow the right margin according to
  the break hints given.
  Setting the margin to [d] means that the formatting engine aims at
  printing at most [d-1] characters per line.
  Nothing happens if [d] is smaller than 2.
  If [d] is too large, the right margin is set to the maximum
  admissible value (which is greater than [10 ^ 9]).
  If [d] is less than the current maximum indentation limit, the
  maximum indentation limit is decreased while trying to preserve
  a minimal ratio [max_indent/margin>=50%] and if possible
  the current difference [margin - max_indent].

  See also {!pp_set_geometry}.
*)

val pp_get_margin : formatter -> unit -> int
val get_margin : unit -> int
(** Returns the position of the right margin. *)

(** {1:maxindent Maximum indentation limit} *)

val pp_set_max_indent : formatter -> int -> unit
val set_max_indent : int -> unit
(** [pp_set_max_indent ppf d] sets the maximum indentation limit of lines
  to [d] (in characters):
  once this limit is reached, new pretty-printing boxes are rejected to the
  left, unless the enclosing box fully fits on the current line.
  As an illustration,
  {[ set_margin 10; set_max_indent 5; printf "@[123456@[7@]89A@]@." ]}
  yields
  {[
    123456
    789A
  ]}
  because the nested box ["@[7@]"] is opened after the maximum indentation
  limit ([7>5]) and its parent box does not fit on the current line.
  Either decreasing the length of the parent box to make it fit on a line:
  {[ printf "@[123456@[7@]89@]@." ]}
  or opening an intermediary box before the maximum indentation limit which
  fits on the current line
  {[ printf "@[123@[456@[7@]89@]A@]@." ]}
  avoids the rejection to the left of the inner boxes and print respectively
  ["123456789"] and ["123456789A"] .
  Note also that vertical boxes never fit on a line whereas horizontal boxes
  always fully fit on the current line.
  Opening a box may split a line whereas the contents may have fit.
  If this behavior is problematic, it can be curtailed by setting the maximum
  indentation limit to [margin - 1]. Note that setting the maximum indentation
  limit to [margin] is invalid.

  Nothing happens if [d] is smaller than 2.

  If [d] is too large, the limit is set to the maximum
  admissible value (which is greater than [10 ^ 9]).

  If [d] is greater or equal than the current margin, it is ignored,
  and the current maximum indentation limit is kept.

  See also {!pp_set_geometry}.
*)

val pp_get_max_indent : formatter -> unit -> int
val get_max_indent : unit -> int
(** Return the maximum indentation limit (in characters). *)

(** {1 Geometry }

Geometric functions can be used to manipulate simultaneously the
coupled variables, margin and maxixum indentation limit.

*)

type geometry = { max_indent:int; margin: int}

val check_geometry: geometry -> bool
(** Check if the formatter geometry is valid: [1 < max_indent < margin] *)

val pp_set_geometry : formatter -> max_indent:int -> margin:int -> unit
val set_geometry : max_indent:int -> margin:int -> unit
val pp_safe_set_geometry : formatter -> max_indent:int -> margin:int -> unit
val safe_set_geometry : max_indent:int -> margin:int -> unit
(**
   [pp_set_geometry ppf ~max_indent ~margin] sets both the margin
   and maximum indentation limit for [ppf].

   When [1 < max_indent < margin],
   [pp_set_geometry ppf ~max_indent ~margin]
   is equivalent to
   [pp_set_margin ppf margin; pp_set_max_indent ppf max_indent];
   and avoids the subtly incorrect
   [pp_set_max_indent ppf max_indent; pp_set_margin ppf margin];

   Outside of this domain, [pp_set_geometry] raises an invalid argument
   exception whereas [pp_safe_set_geometry] does nothing.

   @since 4.08.0
*)

(**
   [pp_update_geometry ppf (fun geo -> { geo with ... })] lets you
   update a formatter's geometry in a way that is robust to extension
   of the [geometry] record with new fields.

   Raises an invalid argument exception if the returned geometry
   does not satisfy {!check_geometry}.

   @since 4.11.0
*)
val pp_update_geometry : formatter -> (geometry -> geometry) -> unit
val update_geometry : (geometry -> geometry) -> unit

val pp_get_geometry: formatter -> unit -> geometry
val get_geometry: unit -> geometry
(** Return the current geometry of the formatter

    @since 4.08.0
*)



(** {1 Maximum formatting depth} *)

(** The maximum formatting depth is the maximum number of pretty-printing
  boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).
*)

val pp_set_max_boxes : formatter -> int -> unit
val set_max_boxes : int -> unit
(** [pp_set_max_boxes ppf max] sets the maximum number of pretty-printing
    boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).

  Nothing happens if [max] is smaller than 2.
*)

val pp_get_max_boxes : formatter -> unit -> int
val get_max_boxes : unit -> int
(** Returns the maximum number of pretty-printing boxes allowed before
  ellipsis.
*)

val pp_over_max_boxes : formatter -> unit -> bool
val over_max_boxes : unit -> bool
(** Tests if the maximum number of pretty-printing boxes allowed have already
  been opened.
*)

(** {1 Tabulation boxes} *)

(**

  A {e tabulation box} prints material on lines divided into cells of fixed
  length. A tabulation box provides a simple way to display vertical columns
  of left adjusted text.

  This box features command [set_tab] to define cell boundaries, and command
  [print_tab] to move from cell to cell and split the line when there is no
  more cells to print on the line.

  Note: printing within tabulation box is line directed, so arbitrary line
  splitting inside a tabulation box leads to poor rendering. Yet, controlled
  use of tabulation boxes allows simple printing of columns within
  module {!Format}.
*)

val pp_open_tbox : formatter -> unit -> unit
val open_tbox : unit -> unit
(** [open_tbox ()] opens a new tabulation box.

  This box prints lines separated into cells of fixed width.

  Inside a tabulation box, special {e tabulation markers} defines points of
  interest on the line (for instance to delimit cell boundaries).
  Function {!Format.set_tab} sets a tabulation marker at insertion point.

  A tabulation box features specific {e tabulation breaks} to move to next
  tabulation marker or split the line. Function {!Format.print_tbreak} prints
  a tabulation break.
*)

val pp_close_tbox : formatter -> unit -> unit
val close_tbox : unit -> unit
(** Closes the most recently opened tabulation box. *)

val pp_set_tab : formatter -> unit -> unit
val set_tab : unit -> unit
(** Sets a tabulation marker at current insertion point. *)

val pp_print_tab : formatter -> unit -> unit
val print_tab : unit -> unit
(** [print_tab ()] emits a 'next' tabulation break hint: if not already set on
  a tabulation marker, the insertion point moves to the first tabulation
  marker on the right, or the pretty-printer splits the line and insertion
  point moves to the leftmost tabulation marker.

  It is equivalent to [print_tbreak 0 0]. *)

val pp_print_tbreak : formatter -> int -> int -> unit
val print_tbreak : int -> int -> unit
(** [print_tbreak nspaces offset] emits a 'full' tabulation break hint.

  If not already set on a tabulation marker, the insertion point moves to the
  first tabulation marker on the right and the pretty-printer prints
  [nspaces] spaces.

  If there is no next tabulation marker on the right, the pretty-printer
  splits the line at this point, then insertion point moves to the leftmost
  tabulation marker of the box.

  If the pretty-printer splits the line, [offset] is added to
  the current indentation.
*)

(** {1 Ellipsis} *)

val pp_set_ellipsis_text : formatter -> string -> unit
val set_ellipsis_text : string -> unit
(** Set the text of the ellipsis printed when too many pretty-printing boxes
  are open (a single dot, [.], by default).
*)

val pp_get_ellipsis_text : formatter -> unit -> string
val get_ellipsis_text : unit -> string
(** Return the text of the ellipsis. *)

(** {1:tags Semantic tags} *)

type stag = ..
(** {i Semantic tags} (or simply {e tags}) are user's defined annotations
  to associate user's specific operations to printed entities.

  Common usage of semantic tags is text decoration to get specific font or
  text size rendering for a display device, or marking delimitation of
  entities (e.g. HTML or TeX elements or terminal escape sequences).
  More sophisticated usage of semantic tags could handle dynamic
  modification of the pretty-printer behavior to properly print the material
  within some specific tags.
  For instance, we can define an RGB tag like so:
{[
type stag += RGB of {r:int;g:int;b:int}
]}

  In order to properly delimit printed entities, a semantic tag must be
  opened before and closed after the entity. Semantic tags must be properly
  nested like parentheses using {!pp_open_stag} and {!pp_close_stag}.

  Tag specific operations occur any time a tag is opened or closed, At each
  occurrence, two kinds of operations are performed {e tag-marking} and
  {e tag-printing}:
- The tag-marking operation is the simpler tag specific operation: it simply
  writes a tag specific string into the output device of the
  formatter. Tag-marking does not interfere with line-splitting computation.
- The tag-printing operation is the more involved tag specific operation: it
  can print arbitrary material to the formatter. Tag-printing is tightly
  linked to the current pretty-printer operations.

  Roughly speaking, tag-marking is commonly used to get a better rendering of
  texts in the rendering device, while tag-printing allows fine tuning of
  printing routines to print the same entity differently according to the
  semantic tags (i.e. print additional material or even omit parts of the
  output).

  More precisely: when a semantic tag is opened or closed then both and
  successive 'tag-printing' and 'tag-marking' operations occur:
  - Tag-printing a semantic tag means calling the formatter specific function
  [print_open_stag] (resp. [print_close_stag]) with the name of the tag as
  argument: that tag-printing function can then print any regular material
  to the formatter (so that this material is enqueued as usual in the
  formatter queue for further line splitting computation).
  - Tag-marking a semantic tag means calling the formatter specific function
  [mark_open_stag] (resp. [mark_close_stag]) with the name of the tag as
  argument: that tag-marking function can then return the 'tag-opening
  marker' (resp. `tag-closing marker') for direct output into the output
  device of the formatter.

  Being written directly into the output device of the formatter, semantic
  tag marker strings are not considered as part of the printing material that
  drives line splitting (in other words, the length of the strings
  corresponding to tag markers is considered as zero for line splitting).

  Thus, semantic tag handling is in some sense transparent to pretty-printing
  and does not interfere with usual indentation. Hence, a single
  pretty-printing routine can output both simple 'verbatim' material or
  richer decorated output depending on the treatment of tags. By default,
  tags are not active, hence the output is not decorated with tag
  information. Once [set_tags] is set to [true], the pretty-printer engine
  honors tags and decorates the output accordingly.

  Default tag-marking functions behave the HTML way: {{!tag}string tags} are
  enclosed in "<" and ">" while other tags are ignored;
  hence, opening marker for tag string ["t"] is ["<t>"] and closing marker
  is ["</t>"].

  Default tag-printing functions just do nothing.

  Tag-marking and tag-printing functions are user definable and can
  be set by calling {!set_formatter_stag_functions}.

  Semantic tag operations may be set on or off with {!set_tags}.
  Tag-marking operations may be set on or off with {!set_mark_tags}.
  Tag-printing operations may be set on or off with {!set_print_tags}.

  @since 4.08.0
*)

type tag = string
type stag += String_tag of tag
(** [String_tag s] is a string tag [s]. String tags can be inserted either
    by explicitly using the constructor [String_tag] or by using the dedicated
    format syntax ["@{<s> ... @}"].

    @since 4.08.0
*)

val pp_open_stag : formatter -> stag -> unit
val open_stag : stag -> unit
(** [pp_open_stag ppf t] opens the semantic tag named [t].

  The [print_open_stag] tag-printing function of the formatter is called with
  [t] as argument; then the opening tag marker for [t], as given by
  [mark_open_stag t], is written into the output device of the formatter.

  @since 4.08.0
*)

val pp_close_stag : formatter -> unit -> unit
val close_stag : unit -> unit
(** [pp_close_stag ppf ()] closes the most recently opened semantic tag [t].

  The closing tag marker, as given by [mark_close_stag t], is written into the
  output device of the formatter; then the [print_close_stag] tag-printing
  function of the formatter is called with [t] as argument.

  @since 4.08.0
*)

val pp_set_tags : formatter -> bool -> unit
val set_tags : bool -> unit
(** [pp_set_tags ppf b] turns on or off the treatment of semantic tags
  (default is off).
*)

val pp_set_print_tags : formatter -> bool -> unit
val set_print_tags : bool -> unit
(** [pp_set_print_tags ppf b] turns on or off the tag-printing operations. *)

val pp_set_mark_tags : formatter -> bool -> unit
val set_mark_tags : bool -> unit
(** [pp_set_mark_tags ppf b] turns on or off the tag-marking operations. *)

val pp_get_print_tags : formatter -> unit -> bool
val get_print_tags : unit -> bool
(** Return the current status of tag-printing operations. *)

val pp_get_mark_tags : formatter -> unit -> bool
val get_mark_tags : unit -> bool
(** Return the current status of tag-marking operations. *)

(** {1 Redirecting the standard formatter output} *)
val pp_set_formatter_out_channel :
  formatter -> Stdlib.out_channel -> unit
val set_formatter_out_channel : Stdlib.out_channel -> unit
(** Redirect the standard pretty-printer output to the given channel.
  (All the output functions of the standard formatter are set to the
   default output functions printing to the given channel.)

  [set_formatter_out_channel] is equivalent to
  {!pp_set_formatter_out_channel} [std_formatter].
*)

val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
(** [pp_set_formatter_output_functions ppf out flush] redirects the
  standard pretty-printer output functions to the functions [out] and
  [flush].

  The [out] function performs all the pretty-printer string output.
  It is called with a string [s], a start position [p], and a number of
  characters [n]; it is supposed to output characters [p] to [p + n - 1] of
  [s].

  The [flush] function is called whenever the pretty-printer is flushed
  (via conversion [%!], or pretty-printing indications [@?] or [@.], or
  using low level functions [print_flush] or [print_newline]).
*)

val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)
val get_formatter_output_functions :
  unit -> (string -> int -> int -> unit) * (unit -> unit)
(** Return the current output functions of the standard pretty-printer. *)

(** {1:meaning Redefining formatter output} *)

(** The [Format] module is versatile enough to let you completely redefine
  the meaning of pretty-printing output: you may provide your own functions
  to define how to handle indentation, line splitting, and even printing of
  all the characters that have to be printed!
*)

(** {2 Redefining output functions} *)

type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
  out_indent : int -> unit;(** @since 4.06.0 *)
}
(** The set of output functions specific to a formatter:
- the [out_string] function performs all the pretty-printer string output.
  It is called with a string [s], a start position [p], and a number of
  characters [n]; it is supposed to output characters [p] to [p + n - 1] of
  [s].
- the [out_flush] function flushes the pretty-printer output device.
- [out_newline] is called to open a new line when the pretty-printer splits
  the line.
- the [out_spaces] function outputs spaces when a break hint leads to spaces
  instead of a line split. It is called with the number of spaces to output.
- the [out_indent] function performs new line indentation when the
  pretty-printer splits the line. It is called with the indentation value of
  the new line.

  By default:
- fields [out_string] and [out_flush] are output device specific;
  (e.g. {!Stdlib.output_string} and {!Stdlib.flush} for a
   {!Stdlib.out_channel} device, or [Buffer.add_substring] and
   {!Stdlib.ignore} for a [Buffer.t] output device),
- field [out_newline] is equivalent to [out_string "\n" 0 1];
- fields [out_spaces] and [out_indent] are equivalent to
  [out_string (String.make n ' ') 0 n].
  @since 4.01.0
*)

val pp_set_formatter_out_functions :
  formatter -> formatter_out_functions -> unit
val set_formatter_out_functions : formatter_out_functions -> unit
(** [pp_set_formatter_out_functions ppf out_funs]
  Set all the pretty-printer output functions of [ppf] to those of
  argument [out_funs],

  This way, you can change the meaning of indentation (which can be
  something else than just printing space characters) and the meaning of new
  lines opening (which can be connected to any other action needed by the
  application at hand).

  Reasonable defaults for functions [out_spaces] and [out_newline] are
  respectively [out_funs.out_string (String.make n ' ') 0 n] and
  [out_funs.out_string "\n" 0 1].
  @since 4.01.0
*)

val pp_get_formatter_out_functions :
  formatter -> unit -> formatter_out_functions
val get_formatter_out_functions : unit -> formatter_out_functions
(** Return the current output functions of the pretty-printer,
  including line splitting and indentation functions. Useful to record the
  current setting and restore it afterwards.
  @since 4.01.0
*)

(** {1:tagsmeaning Redefining semantic tag operations} *)

type formatter_stag_functions = {
  mark_open_stag : stag -> string;
  mark_close_stag : stag -> string;
  print_open_stag : stag -> unit;
  print_close_stag : stag -> unit;
}
(** The semantic tag handling functions specific to a formatter:
  [mark] versions are the 'tag-marking' functions that associate a string
  marker to a tag in order for the pretty-printing engine to write
  those markers as 0 length tokens in the output device of the formatter.
  [print] versions are the 'tag-printing' functions that can perform
  regular printing when a tag is closed or opened.

  @since 4.08.0
*)

val pp_set_formatter_stag_functions :
  formatter -> formatter_stag_functions -> unit
val set_formatter_stag_functions : formatter_stag_functions -> unit
(** [pp_set_formatter_stag_functions ppf tag_funs] changes the meaning of
  opening and closing semantic tag operations to use the functions in
  [tag_funs] when printing on [ppf].

  When opening a semantic tag with name [t], the string [t] is passed to the
  opening tag-marking function (the [mark_open_stag] field of the
  record [tag_funs]), that must return the opening tag marker for
  that name. When the next call to [close_stag ()] happens, the semantic tag
  name [t] is sent back to the closing tag-marking function (the
  [mark_close_stag] field of record [tag_funs]), that must return a
  closing tag marker for that name.

  The [print_] field of the record contains the tag-printing functions that
  are called at tag opening and tag closing time, to output regular material
  in the pretty-printer queue.

  @since 4.08.0
*)

val pp_get_formatter_stag_functions :
  formatter -> unit -> formatter_stag_functions
val get_formatter_stag_functions : unit -> formatter_stag_functions
(** Return the current semantic tag operation functions of the standard
    pretty-printer.

    @since 4.08.0 *)

(** {1:formatter Defining formatters}

  Defining new formatters permits unrelated output of material in
  parallel on several output devices.
  All the parameters of a formatter are local to the formatter:
  right margin, maximum indentation limit, maximum number of pretty-printing
  boxes simultaneously open, ellipsis, and so on, are specific to
  each formatter and may be fixed independently.

  For instance, given a {!Buffer.t} buffer [b], {!formatter_of_buffer} [b]
  returns a new formatter using buffer [b] as its output device.
  Similarly, given a {!Stdlib.out_channel} output channel [oc],
  {!formatter_of_out_channel} [oc] returns a new formatter using
  channel [oc] as its output device.

  Alternatively, given [out_funs], a complete set of output functions for a
  formatter, then {!formatter_of_out_functions} [out_funs] computes a new
  formatter using those functions for output.
*)

val formatter_of_out_channel : out_channel -> formatter
(** [formatter_of_out_channel oc] returns a new formatter writing
    to the corresponding output channel [oc].
*)

val synchronized_formatter_of_out_channel :
  out_channel -> formatter Domain.DLS.key
(** [synchronized_formatter_of_out_channel oc] returns the key to the
    domain-local state that holds the domain-local formatter for writing to the
    corresponding output channel [oc].

    When the formatter is used with multiple domains, the output from the
    domains will be interleaved with each other at points where the formatter
    is flushed, such as with {!print_flush}.
*)


val std_formatter : formatter
(** The initial domain's standard formatter to write to standard output.

  It is defined as {!formatter_of_out_channel} {!Stdlib.stdout}.
*)

val get_std_formatter : unit -> formatter
(** [get_std_formatter ()] returns the current domain's standard formatter used
    to write to standard output.
*)

val err_formatter : formatter
(** The initial domain's formatter to write to standard error.

  It is defined as {!formatter_of_out_channel} {!Stdlib.stderr}.
*)

val get_err_formatter : unit -> formatter
(* [get_err_formatter ()] returns the current domain's formatter used to write
   to standard error.
*)

val formatter_of_buffer : Buffer.t -> formatter
(** [formatter_of_buffer b] returns a new formatter writing to
  buffer [b]. At the end of pretty-printing, the formatter must be flushed
  using {!pp_print_flush} or {!pp_print_newline}, to print all the
  pending material into the buffer.
*)

val stdbuf : Buffer.t
(** The initial domain's string buffer in which [str_formatter] writes. *)

val get_stdbuf : unit -> Buffer.t
(** [get_stdbuf ()] returns the current domain's string buffer in which the
    current domain's string formatter writes. *)

val str_formatter : formatter
(** The initial domain's formatter to output to the {!stdbuf} string buffer.

  [str_formatter] is defined as {!formatter_of_buffer} {!stdbuf}.
*)

val get_str_formatter : unit -> formatter
(** The current domain's formatter to output to the current domains string
    buffer.
*)

val flush_str_formatter : unit -> string
(** Returns the material printed with [str_formatter] of the current domain,
    flushes the formatter and resets the corresponding buffer.
*)

val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
(** [make_formatter out flush] returns a new formatter that outputs with
  function [out], and flushes with function [flush].

  For instance,
  {[
    make_formatter
      (Stdlib.output oc)
      (fun () -> Stdlib.flush oc)
  ]}
  returns a formatter to the {!Stdlib.out_channel} [oc].
*)

val make_synchronized_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter Domain.DLS.key
(** [make_synchronized_formatter out flush] returns the key to the domain-local
    state that holds the domain-local formatter that outputs with function
    [out], and flushes with function [flush].

    When the formatter is used with multiple domains, the output from the
    domains will be interleaved with each other at points where the formatter
    is flushed, such as with {!print_flush}.
*)

val formatter_of_out_functions :
  formatter_out_functions -> formatter
(** [formatter_of_out_functions out_funs] returns a new formatter that writes
  with the set of output functions [out_funs].

  See definition of type {!formatter_out_functions} for the meaning of argument
  [out_funs].

  @since 4.06.0
*)



(** {2:symbolic Symbolic pretty-printing} *)

(**
  Symbolic pretty-printing is pretty-printing using a symbolic formatter,
  i.e. a formatter that outputs symbolic pretty-printing items.

  When using a symbolic formatter, all regular pretty-printing activities
  occur but output material is symbolic and stored in a buffer of output items.
  At the end of pretty-printing, flushing the output buffer allows
  post-processing of symbolic output before performing low level output
  operations.

  In practice, first define a symbolic output buffer [b] using:
  - [let sob = make_symbolic_output_buffer ()].
  Then define a symbolic formatter with:
  - [let ppf = formatter_of_symbolic_output_buffer sob]

  Use symbolic formatter [ppf] as usual, and retrieve symbolic items at end
  of pretty-printing by flushing symbolic output buffer [sob] with:
  - [flush_symbolic_output_buffer sob].
*)

type symbolic_output_item =
  | Output_flush (** symbolic flush command *)
  | Output_newline (** symbolic newline command *)
  | Output_string of string
  (** [Output_string s]: symbolic output for string [s]*)
  | Output_spaces of int
  (** [Output_spaces n]: symbolic command to output [n] spaces *)
  | Output_indent of int
  (** [Output_indent i]: symbolic indentation of size [i] *)
(** Items produced by symbolic pretty-printers
    @since 4.06.0
*)

type symbolic_output_buffer
(**
  The output buffer of a symbolic pretty-printer.

  @since 4.06.0
*)

val make_symbolic_output_buffer : unit -> symbolic_output_buffer
(** [make_symbolic_output_buffer ()] returns a fresh buffer for
  symbolic output.

  @since 4.06.0
*)

val clear_symbolic_output_buffer : symbolic_output_buffer -> unit
(** [clear_symbolic_output_buffer sob] resets buffer [sob].

  @since 4.06.0
*)

val get_symbolic_output_buffer :
  symbolic_output_buffer -> symbolic_output_item list
(** [get_symbolic_output_buffer sob] returns the contents of buffer [sob].

  @since 4.06.0
*)

val flush_symbolic_output_buffer :
  symbolic_output_buffer -> symbolic_output_item list
(** [flush_symbolic_output_buffer sob] returns the contents of buffer
  [sob] and resets buffer [sob].
  [flush_symbolic_output_buffer sob] is equivalent to
  [let items = get_symbolic_output_buffer sob in
   clear_symbolic_output_buffer sob; items]

  @since 4.06.0
*)

val add_symbolic_output_item :
  symbolic_output_buffer -> symbolic_output_item -> unit
(** [add_symbolic_output_item sob itm] adds item [itm] to buffer [sob].

  @since 4.06.0
*)

val formatter_of_symbolic_output_buffer : symbolic_output_buffer -> formatter
(** [formatter_of_symbolic_output_buffer sob] returns a symbolic formatter
  that outputs to [symbolic_output_buffer] [sob].

  @since 4.06.0
*)

(** {1 Convenience formatting functions.} *)

val pp_print_iter :
  ?pp_sep:(formatter -> unit -> unit) ->
  (('a -> unit) -> 'b -> unit) ->
  (formatter -> 'a -> unit) -> formatter -> 'b -> unit
(** [pp_print_iter ~pp_sep iter pp_v ppf v] formats on [ppf] the iterations of
  [iter] over a collection [v] of values using [pp_v]. Iterations are
  separated by [pp_sep] (defaults to {!pp_print_cut}).

  @since 5.1.0
*)

val pp_print_list:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a list -> unit)
(** [pp_print_list ?pp_sep pp_v ppf l] prints items of list [l],
  using [pp_v] to print each item, and calling [pp_sep]
  between items ([pp_sep] defaults to {!pp_print_cut}).
  Does nothing on empty lists.

  @since 4.02.0
*)

val pp_print_array:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a array -> unit)
(** [pp_print_array ?pp_sep pp_v ppf a] prints items of array [a],
  using [pp_v] to print each item, and calling [pp_sep]
  between items ([pp_sep] defaults to {!pp_print_cut}).
  Does nothing on empty arrays.

  If [a] is mutated after [pp_print_array] is called, the printed values
  may not be what is expected because [Format] can delay the printing.
  This can be avoided by flushing [ppf].

  @since 5.1.0
*)

val pp_print_seq:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a Seq.t -> unit)
(** [pp_print_seq ?pp_sep pp_v ppf s] prints items of sequence [s],
  using [pp_v] to print each item, and calling [pp_sep]
  between items ([pp_sep] defaults to {!pp_print_cut}.
  Does nothing on empty sequences.

  This function does not terminate on infinite sequences.

  @since 4.12
*)

val pp_print_text : formatter -> string -> unit
(** [pp_print_text ppf s] prints [s] with spaces and newlines respectively
  printed using {!pp_print_space} and {!pp_force_newline}.

  @since 4.02.0
*)

val pp_print_option :
  ?none:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a option -> unit)
(** [pp_print_option ?none pp_v ppf o] prints [o] on [ppf]
    using [pp_v] if [o] is [Some v] and [none] if it is [None]. [none]
    prints nothing by default.

    @since 4.08 *)

val pp_print_result :
  ok:(formatter -> 'a -> unit) -> error:(formatter -> 'e -> unit) ->
  formatter -> ('a, 'e) result -> unit
(** [pp_print_result ~ok ~error ppf r] prints [r] on [ppf] using
    [ok] if [r] is [Ok _] and [error] if [r] is [Error _].

    @since 4.08 *)

val pp_print_either :
  left:(formatter -> 'a -> unit) ->
  right:(formatter -> 'b -> unit) -> formatter -> ('a, 'b) Either.t -> unit
(** [pp_print_either ~left ~right ppf e] prints [e] on [ppf] using
    [left] if [e] is [Either.Left _] and [right] if [e] is [Either.Right _].

    @since 4.13 *)

(** {1:fpp Formatted pretty-printing} *)

(**
  Module [Format] provides a complete set of [printf] like functions for
  pretty-printing using format string specifications.

  Specific annotations may be added in the format strings to give
  pretty-printing commands to the pretty-printing engine.

  Those annotations are introduced in the format strings using the [@]
  character. For instance, [@ ] means a space break, [@,] means a cut,
  [@\[] opens a new box, and [@\]] closes the last open box.

*)

val fprintf : formatter -> ('a, formatter, unit) format -> 'a

(** [fprintf ff fmt arg1 ... argN] formats the arguments [arg1] to [argN]
  according to the format string [fmt], and outputs the resulting string on
  the formatter [ff].

  The format string [fmt] is a character string which contains three types of
  objects: plain characters and conversion specifications as specified in
  the {!Printf} module, and pretty-printing indications specific to the
  [Format] module.

  The pretty-printing indication characters are introduced by
  a [@] character, and their meanings are:
  - [@\[]: open a pretty-printing box. The type and offset of the
    box may be optionally specified with the following syntax:
    the [<] character, followed by an optional box type indication,
    then an optional integer offset, and the closing [>] character.
    Pretty-printing box type is one of [h], [v], [hv], [b], or [hov].
    '[h]' stands for an 'horizontal' pretty-printing box,
    '[v]' stands for a 'vertical' pretty-printing box,
    '[hv]' stands for an 'horizontal/vertical' pretty-printing box,
    '[b]' stands for an 'horizontal-or-vertical' pretty-printing box
    demonstrating indentation,
    '[hov]' stands a simple 'horizontal-or-vertical' pretty-printing box.
    For instance, [@\[<hov 2>] opens an 'horizontal-or-vertical'
    pretty-printing box with indentation 2 as obtained with [open_hovbox 2].
    For more details about pretty-printing boxes, see the various box opening
    functions [open_*box].
  - [@\]]: close the most recently opened pretty-printing box.
  - [@,]: output a 'cut' break hint, as with [print_cut ()].
  - [@ ]: output a 'space' break hint, as with [print_space ()].
  - [@;]: output a 'full' break hint as with [print_break]. The
    [nspaces] and [offset] parameters of the break hint may be
    optionally specified with the following syntax:
    the [<] character, followed by an integer [nspaces] value,
    then an integer [offset], and a closing [>] character.
    If no parameters are provided, the good break defaults to a
    'space' break hint.
  - [@.]: flush the pretty-printer and split the line, as with
    [print_newline ()].
  - [@<n>]: print the following item as if it were of length [n].
    Hence, [printf "@<0>%s" arg] prints [arg] as a zero length string.
    If [@<n>] is not followed by a conversion specification,
    then the following character of the format is printed as if
    it were of length [n].
  - [@\{]: open a semantic tag. The name of the tag may be optionally
    specified with the following syntax:
    the [<] character, followed by an optional string
    specification, and the closing [>] character. The string
    specification is any character string that does not contain the
    closing character ['>']. If omitted, the tag name defaults to the
    empty string.
    For more details about semantic tags, see the functions {!open_stag} and
    {!close_stag}.
  - [@\}]: close the most recently opened semantic tag.
  - [@?]: flush the pretty-printer as with [print_flush ()].
    This is equivalent to the conversion [%!].
  - [@\n]: force a newline, as with [force_newline ()], not the normal way
    of pretty-printing, you should prefer using break hints inside a vertical
    pretty-printing box.

  Note: To prevent the interpretation of a [@] character as a
  pretty-printing indication, escape it with a [%] character.
  Old quotation mode [@@] is deprecated since it is not compatible with
  formatted input interpretation of character ['@'].

  Example: [printf "@[%s@ %d@]@." "x =" 1] is equivalent to
  [open_box (); print_string "x ="; print_space ();
   print_int 1; close_box (); print_newline ()].
  It prints [x = 1] within a pretty-printing 'horizontal-or-vertical' box.

*)

val printf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [get_std_formatter ()].

    It is defined similarly to [fun fmt -> fprintf (get_std_formatter ()) fmt]
    but delays calling [get_std_formatter] until after the final argument
    required by the [format] is received. When used with multiple domains, the
    output from the domains will be interleaved with each other at points where
    the formatter is flushed, such as with {!print_flush}.
*)

val eprintf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [get_err_formatter ()].

    It is defined similarly to [fun fmt -> fprintf (get_err_formatter ()) fmt]
    but delays calling [get_err_formatter] until after the final argument
    required by the [format] is received. When used with multiple domains, the
    output from the domains will be interleaved with each other at points where
    the formatter is flushed, such as with {!print_flush}.
*)

val sprintf : ('a, unit, string) format -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  Note that the pretty-printer queue is flushed at the end of {e each
  call} to [sprintf].

  In case of multiple and related calls to [sprintf] to output
  material on a single string, you should consider using [fprintf]
  with the predefined formatter [str_formatter] and call
  [flush_str_formatter ()] to get the final result.

  Alternatively, you can use [Format.fprintf] with a formatter writing to a
  buffer of your own: flushing the formatter and the buffer at the end of
  pretty-printing returns the desired string.
*)

val asprintf : ('a, formatter, unit, string) format4 -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  The type of [asprintf] is general enough to interact nicely with [%a]
  conversions.

  @since 4.01.0
*)

val dprintf :
  ('a, formatter, unit, formatter -> unit) format4 -> 'a
(** Same as {!fprintf}, except the formatter is the last argument.
  [dprintf "..." a b c] is a function of type
  [formatter -> unit] which can be given to a format specifier [%t].

  This can be used as a replacement for {!asprintf} to delay
  formatting decisions. Using the string returned by {!asprintf} in a
  formatting context forces formatting decisions to be taken in
  isolation, and the final string may be created
  prematurely. {!dprintf} allows delay of formatting decisions until
  the final formatting context is known.
  For example:
{[
  let t = Format.dprintf "%i@ %i@ %i" 1 2 3 in
  ...
  Format.printf "@[<v>%t@]" t
]}

  @since 4.08.0
*)


val ifprintf : formatter -> ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but does not print anything.
  Useful to ignore some material when conditionally printing.

  @since 3.10.0
*)

(** Formatted Pretty-Printing with continuations. *)

val kfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [fprintf] above, but instead of returning immediately,
  passes the formatter to its first argument at the end of printing. *)

val kdprintf :
  ((formatter -> unit) -> 'a) ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as {!dprintf} above, but instead of returning immediately,
  passes the suspended printer to its first argument at the end of printing.

  @since 4.08.0
*)

val ikfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [kfprintf] above, but does not print anything.
  Useful to ignore some material when conditionally printing.

  @since 3.12.0
*)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
  passes it to the first argument. *)

val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [asprintf] above, but instead of returning the string,
  passes it to the first argument.

  @since 4.03
*)

(** {1:examples Examples}

  A few warmup examples to get an idea of how Format is used.

  We have a list [l] of pairs [(int * bool)], which the toplevel prints for us:

  {[# let l = List.init 20 (fun n -> n, n mod 2 = 0)
  val l : (int * bool) list =
  [(0, true); (1, false); (2, true); (3, false); (4, true); (5, false);
   (6, true); (7, false); (8, true); (9, false); (10, true); (11, false);
   (12, true); (13, false); (14, true); (15, false); (16, true); (17, false);
   (18, true); (19, false)]
 ]}

  If we want to print it ourself without the toplevel magic, we can try this:

  {[
  # let pp_pair out (x,y) = Format.fprintf out "(%d, %b)" x y
  val pp_pair : Format.formatter -> int * bool -> unit = <fun>
  # Format.printf "l: [@[<hov>%a@]]@."
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_pair) l
    l: [(0, true); (1, false); (2, true); (3, false); (4, true); (5, false);
        (6, true); (7, false); (8, true); (9, false); (10, true); (11, false);
        (12, true); (13, false); (14, true); (15, false); (16, true);
        (17, false); (18, true); (19, false)]

  ]}


  What this does, briefly, is:

    - [pp_pair] prints a pair [bool*int] surrounded in "(" ")". It takes
      a formatter (into which formatting happens), and the pair itself.
      When printing is done it returns [()].

    - [Format.printf "l = [@[<hov>%a@]]@." ... l] is like [printf], but
      with additional formatting instructions (denoted with "@"). The pair
      "@[<hov>" and "@]" is a "horizontal-or-vertical box".

    - "@." ends formatting with a newline. It is similar to "\n" but is also
      aware of the [Format.formatter]'s state. Do not use "\n" with [Format].

    - "%a" is a formatting instruction, like "%d" or "%s" for [printf].
      However, where "%d" prints an integer and "%s" prints a string,
      "%a" takes a printer (of type [Format.formatter -> 'a -> unit])
      and a value (of type ['a]) and applies the printer to the value.
      This is key to compositionality of printers.

    - We build a list printer using
      [Format.pp_print_list ~pp_sep:(...) pp_pair].
      [pp_print_list] takes an element printer and returns a list printer.
      The [?pp_sep] optional argument, if provided, is called in between
      each element to print a separator.

    - Here, for a separator, we use [(fun out () -> Format.fprintf out ";@ ")].
      It prints ";", and then "@ " which is a breaking space
      (either it prints " ", or it prints a newline if the box is about to
      overflow).
      This "@ " is responsible for the list printing splitting into several
      lines.

  If we omit "@ ", we get an ugly single-line print:

  {[# Format.printf "l: [@[<hov>%a@]]@."
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "; ") pp_pair) l
  l: [(0, true); (1, false); (2, true); (* ... *); (18, true); (19, false)]
- : unit = ()
    ]}

  Generally, it is good practice to define custom printers for important types
  in your program. If, for example, you were to define basic geometry
  types like so:

  {[
  type point = {
    x: float;
    y: float;
  }

  type rectangle = {
    ll: point; (* lower left *)
    ur: point; (* upper right *)
  }
  ]}

  For debugging purpose, or to display information in logs, or on the console,
  it would be convenient to define printers for these types.
  Here is an example of to do it.
  Note that "%.3f" is a [float] printer up to 3 digits of precision
  after the dot; "%f" would print as many digits as required, which is
  somewhat verbose; "%h" is an hexadecimal float printer.

  {[
  let pp_point out (p:point) =
    Format.fprintf out "{ @[x=%.3f;@ y=%.3f@] }" p.x p.y

  let pp_rectangle out (r:rectangle) =
    Format.fprintf out "{ @[ll=%a;@ ur=%a@] }"
      pp_point r.ll pp_point r.ur
  ]}

  In the [.mli] file, we could have:

  {[
    val pp_point : Format.formatter -> point -> unit

    val pp_rectangle : Format.formatter -> rectangle -> unit
  ]}

  These printers can now be used with "%a" inside other printers.

  {[ # Format.printf "some rectangle: %a@."
        (Format.pp_print_option pp_rectangle)
        (Some {ll={x=1.; y=2.}; ur={x=42.; y=500.12345}})
  some rectangle: { l={ x=1.000; y=2.000 }; ur={ x=42.000; y=500.123 } }

  # Format.printf "no rectangle: %a@."
        (Format.pp_option pp_rectangle)
        None
  no rectangle:
  ]}

  See how we combine [pp_print_option] (option printer) and our newly defined
  rectangle printer, like we did with [pp_print_list] earlier.

  For a more extensive tutorial, see
  {{: https://caml.inria.fr/resources/doc/guides/format.en.html}
    "Using the Format module"}.

  A final note: the [Format] module is a starting point.
  The OCaml ecosystem has libraries that makes formatting easier
  and more expressive, with more combinators, more concise names, etc.
  An example of such a library is {{: https://erratique.ch/software/fmt} Fmt}.

  Automatic deriving of pretty-printers from type definitions is also possible,
  using {{: ppx_deriving.show} https://github.com/ocaml-ppx/ppx_deriving}
  or similar ppx derivers.
*)
