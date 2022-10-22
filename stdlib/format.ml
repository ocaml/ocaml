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

(* A pretty-printing facility and definition of formatters for 'parallel'
   (i.e. unrelated or independent) pretty-printing on multiple out channels. *)

(*
   The pretty-printing engine internal data structures.
*)

let id x = x

(* A devoted type for sizes to avoid confusion
   between sizes and mere integers. *)
module Size : sig
  type t

  val to_int : t -> int
  val of_int : int -> t
  val zero : t
  val unknown : t
  val is_known : t -> bool
end  = struct
  type t = int

  let to_int = id
  let of_int = id
  let zero = 0
  let unknown = -1
  let is_known n = n >= 0
end



(* The pretty-printing boxes definition:
   a pretty-printing box is either
   - hbox: horizontal box (no line splitting)
   - vbox: vertical box (every break hint splits the line)
   - hvbox: horizontal/vertical box
     (the box behaves as an horizontal box if it fits on
      the current line, otherwise the box behaves as a vertical box)
   - hovbox: horizontal or vertical compacting box
     (the box is compacting material, printing as much material as possible
      on every lines)
   - box: horizontal or vertical compacting box with enhanced box structure
     (the box behaves as an horizontal or vertical box but break hints split
      the line if splitting would move to the left)
*)
type box_type = CamlinternalFormatBasics.block_type =
  | Pp_hbox | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits


(* The pretty-printing tokens definition:
   are either text to print or pretty printing
   elements that drive indentation and line splitting. *)
type pp_token =
  | Pp_text of string          (* normal text *)
  | Pp_break of {              (* complete break *)
      fits: string * int * string;   (* line is not split *)
      breaks: string * int * string; (* line is split *)
    }
  | Pp_tbreak of int * int     (* go to next tabulation *)
  | Pp_stab                    (* set a tabulation *)
  | Pp_begin of int * box_type (* beginning of a box *)
  | Pp_end                     (* end of a box *)
  | Pp_tbegin of tbox          (* beginning of a tabulation box *)
  | Pp_tend                    (* end of a tabulation box *)
  | Pp_newline                 (* to force a newline inside a box *)
  | Pp_if_newline              (* to do something only if this very
                                  line has been broken *)
  | Pp_open_tag of stag         (* opening a tag name *)
  | Pp_close_tag               (* closing the most recently open tag *)

and stag = ..

and tbox = Pp_tbox of int list ref  (* Tabulation box *)

type tag = string
type stag += String_tag of tag


(* The pretty-printer queue:
   pretty-printing material is not written in the output as soon as emitted;
   instead, the material is simply recorded in the pretty-printer queue,
   until the enclosing box has a known computed size and proper splitting
   decisions can be made.

   The pretty-printer queue contains formatting elements to be printed.
   Each formatting element is a tuple (size, token, length), where
   - length is the declared length of the token,
   - size is effective size of the token when it is printed
     (size is set when the size of the box is known, so that size of break
      hints are definitive). *)
type pp_queue_elem = {
  mutable size : Size.t;
  token : pp_token;
  length : int;
}


(* The pretty-printer queue definition. *)
type pp_queue = pp_queue_elem Queue.t

(* The pretty-printer scanning stack. *)

(* The pretty-printer scanning stack: scanning element definition. *)
type pp_scan_elem = {
  left_total : int; (* Value of pp_left_total when the element was enqueued. *)
  queue_elem : pp_queue_elem
}

(* The pretty-printer formatting stack:
   the formatting stack contains the description of all the currently active
   boxes; the pretty-printer formatting stack is used to split the lines
   while printing tokens. *)

(* The pretty-printer formatting stack: formatting stack element definition.
   Each stack element describes a pretty-printing box. *)
type pp_format_elem = { box_type : box_type; width : int }

(* The formatter definition.
   Each formatter value is a pretty-printer instance with all its
   machinery. *)
type formatter = {
  (* The pretty-printer scanning stack. *)
  pp_scan_stack : pp_scan_elem Stack.t;
  (* The pretty-printer formatting stack. *)
  pp_format_stack : pp_format_elem Stack.t;
  pp_tbox_stack : tbox Stack.t;
  (* The pretty-printer semantics tag stack. *)
  pp_tag_stack : stag Stack.t;
  pp_mark_stack : stag Stack.t;
  (* Value of right margin. *)
  mutable pp_margin : int;
  (* Minimal space left before margin, when opening a box. *)
  mutable pp_min_space_left : int;
  (* Maximum value of indentation:
     no box can be opened further. *)
  mutable pp_max_indent : int;
  (* Space remaining on the current line. *)
  mutable pp_space_left : int;
  (* Current value of indentation. *)
  mutable pp_current_indent : int;
  (* True when the line has been broken by the pretty-printer. *)
  mutable pp_is_new_line : bool;
  (* Total width of tokens already printed. *)
  mutable pp_left_total : int;
  (* Total width of tokens ever put in queue. *)
  mutable pp_right_total : int;
  (* Current number of open boxes. *)
  mutable pp_curr_depth : int;
  (* Maximum number of boxes which can be simultaneously open. *)
  mutable pp_max_boxes : int;
  (* Ellipsis string. *)
  mutable pp_ellipsis : string;
  (* Output function. *)
  mutable pp_out_string : string -> int -> int -> unit;
  (* Flushing function. *)
  mutable pp_out_flush : unit -> unit;
  (* Output of new lines. *)
  mutable pp_out_newline : unit -> unit;
  (* Output of break hints spaces. *)
  mutable pp_out_spaces : int -> unit;
  (* Output of indentation of new lines. *)
  mutable pp_out_indent : int -> unit;
  (* Are tags printed ? *)
  mutable pp_print_tags : bool;
  (* Are tags marked ? *)
  mutable pp_mark_tags : bool;
  (* Find opening and closing markers of tags. *)
  mutable pp_mark_open_tag : stag -> string;
  mutable pp_mark_close_tag : stag -> string;
  mutable pp_print_open_tag : stag -> unit;
  mutable pp_print_close_tag : stag -> unit;
  (* The pretty-printer queue. *)
  pp_queue : pp_queue;
}


(* The formatter specific tag handling functions. *)
type formatter_stag_functions = {
  mark_open_stag : stag -> string;
  mark_close_stag : stag -> string;
  print_open_stag : stag -> unit;
  print_close_stag : stag -> unit;
}


(* The formatter functions to output material. *)
type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
  out_indent : int -> unit;
}


(*

  Auxiliaries and basic functions.

*)

(* Enter a token in the pretty-printer queue. *)
let pp_enqueue state token =
  state.pp_right_total <- state.pp_right_total + token.length;
  Queue.add token state.pp_queue


let pp_clear_queue state =
  state.pp_left_total <- 1; state.pp_right_total <- 1;
  Queue.clear state.pp_queue


(* Pp_infinity: large value for default tokens size.

   Pp_infinity is documented as being greater than 1e10; to avoid
   confusion about the word 'greater', we choose pp_infinity greater
   than 1e10 + 1; for correct handling of tests in the algorithm,
   pp_infinity must be even one more than 1e10 + 1; let's stand on the
   safe side by choosing 1.e10+10.

   Pp_infinity could probably be 1073741823 that is 2^30 - 1, that is
   the minimal upper bound for integers; now that max_int is defined,
   this limit could also be defined as max_int - 1.

   However, before setting pp_infinity to something around max_int, we
   must carefully double-check all the integer arithmetic operations
   that involve pp_infinity, since any overflow would wreck havoc the
   pretty-printing algorithm's invariants. Given that this arithmetic
   correctness check is difficult and error prone and given that 1e10
   + 1 is in practice large enough, there is no need to attempt to set
   pp_infinity to the theoretically maximum limit. It is not worth the
   burden ! *)
let pp_infinity = 1000000010

(* Output functions for the formatter. *)
let pp_output_string state s = state.pp_out_string s 0 (String.length s)
and pp_output_newline state = state.pp_out_newline ()
and pp_output_spaces state n = state.pp_out_spaces n
and pp_output_indent state n = state.pp_out_indent n

(* Format a textual token *)
let format_pp_text state size text =
  state.pp_space_left <- state.pp_space_left - size;
  pp_output_string state text;
  state.pp_is_new_line <- false

(* Format a string by its length, if not empty *)
let format_string state s =
  if s <> "" then format_pp_text state (String.length s) s

(* To format a break, indenting a new line. *)
let break_new_line state (before, offset, after) width =
  format_string state before;
  pp_output_newline state;
  state.pp_is_new_line <- true;
  let indent = state.pp_margin - width + offset in
  (* Don't indent more than pp_max_indent. *)
  let real_indent = Int.min state.pp_max_indent indent in
  state.pp_current_indent <- real_indent;
  state.pp_space_left <- state.pp_margin - state.pp_current_indent;
  pp_output_indent state state.pp_current_indent;
  format_string state after


(* To force a line break inside a box: no offset is added. *)
let break_line state width = break_new_line state ("", 0, "") width

(* To format a break that fits on the current line. *)
let break_same_line state (before, width, after) =
  format_string state before;
  state.pp_space_left <- state.pp_space_left - width;
  pp_output_spaces state width;
  format_string state after


(* To indent no more than pp_max_indent, if one tries to open a box
   beyond pp_max_indent, then the box is rejected on the left
   by simulating a break. *)
let pp_force_break_line state =
  match Stack.top_opt state.pp_format_stack with
  | None -> pp_output_newline state
  | Some { box_type; width } ->
    if width > state.pp_space_left then
      match box_type with
      | Pp_fits | Pp_hbox -> ()
      | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box -> break_line state width


(* To skip a token, if the previous line has been broken. *)
let pp_skip_token state =
  match Queue.take_opt state.pp_queue with
  | None -> () (* print_if_newline must have been the last printing command *)
  | Some { size; length; _ } ->
    state.pp_left_total <- state.pp_left_total - length;
    state.pp_space_left <- state.pp_space_left + Size.to_int size


(*

  The main pretty printing functions.

*)

(* Formatting a token with a given size. *)
let format_pp_token state size = function

  | Pp_text s ->
    format_pp_text state size s

  | Pp_begin (off, ty) ->
    let insertion_point = state.pp_margin - state.pp_space_left in
    if insertion_point > state.pp_max_indent then
      (* can not open a box right there. *)
      begin pp_force_break_line state end;
    let width = state.pp_space_left - off in
    let box_type =
      match ty with
      | Pp_vbox -> Pp_vbox
      | Pp_hbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits ->
        if size > state.pp_space_left then ty else Pp_fits in
    Stack.push { box_type; width } state.pp_format_stack

  | Pp_end ->
    Stack.pop_opt state.pp_format_stack |> ignore

  | Pp_tbegin (Pp_tbox _ as tbox) ->
    Stack.push tbox state.pp_tbox_stack

  | Pp_tend ->
    Stack.pop_opt state.pp_tbox_stack |> ignore

  | Pp_stab ->
    begin match Stack.top_opt state.pp_tbox_stack with
    | None -> () (* No open tabulation box. *)
    | Some (Pp_tbox tabs) ->
      let rec add_tab n = function
        | [] -> [n]
        | x :: l as ls -> if n < x then n :: ls else x :: add_tab n l in
      tabs := add_tab (state.pp_margin - state.pp_space_left) !tabs
    end

  | Pp_tbreak (n, off) ->
    let insertion_point = state.pp_margin - state.pp_space_left in
    begin match Stack.top_opt state.pp_tbox_stack with
    | None -> () (* No open tabulation box. *)
    | Some (Pp_tbox tabs) ->
      let tab =
        match !tabs with
        | [] -> insertion_point
        | first :: _ ->
          let rec find = function
            | head :: tail ->
              if head >= insertion_point then head else find tail
            | [] -> first in
          find !tabs in
      let offset = tab - insertion_point in
      if offset >= 0
      then break_same_line state ("", offset + n, "")
      else break_new_line state ("", tab + off, "") state.pp_margin
    end

  | Pp_newline ->
    begin match Stack.top_opt state.pp_format_stack with
    | None -> pp_output_newline state (* No open box. *)
    | Some { width; _} -> break_line state width
    end

  | Pp_if_newline ->
    if state.pp_current_indent != state.pp_margin - state.pp_space_left
    then pp_skip_token state

  | Pp_break { fits; breaks } ->
    let before, off, _ = breaks in
    begin match Stack.top_opt state.pp_format_stack with
    | None -> () (* No open box. *)
    | Some { box_type; width } ->
      begin match box_type with
      | Pp_hovbox ->
        if size + String.length before > state.pp_space_left
        then break_new_line state breaks width
        else break_same_line state fits
      | Pp_box ->
        (* Have the line just been broken here ? *)
        if state.pp_is_new_line then break_same_line state fits else
        if size + String.length before > state.pp_space_left
          then break_new_line state breaks width else
        (* break the line here leads to new indentation ? *)
        if state.pp_current_indent > state.pp_margin - width + off
        then break_new_line state breaks width
        else break_same_line state fits
      | Pp_hvbox -> break_new_line state breaks width
      | Pp_fits -> break_same_line state fits
      | Pp_vbox -> break_new_line state breaks width
      | Pp_hbox -> break_same_line state fits
      end
    end

   | Pp_open_tag tag_name ->
     let marker = state.pp_mark_open_tag tag_name in
     pp_output_string state marker;
     Stack.push tag_name state.pp_mark_stack

   | Pp_close_tag ->
     begin match Stack.pop_opt state.pp_mark_stack with
     | None -> () (* No more tag to close. *)
     | Some tag_name ->
       let marker = state.pp_mark_close_tag tag_name in
       pp_output_string state marker
     end


(* Print if token size is known else printing is delayed.
   Printing is delayed when the text waiting in the queue requires
   more room to format than exists on the current line. *)
let rec advance_left state =
  match Queue.peek_opt state.pp_queue with
  | None -> () (* No tokens to print *)
  | Some { size; token; length } ->
    let pending_count = state.pp_right_total - state.pp_left_total in
    if Size.is_known size || pending_count >= state.pp_space_left then begin
      Queue.take state.pp_queue |> ignore; (* Not empty: we peek into it *)
      let size = if Size.is_known size then Size.to_int size else pp_infinity in
      format_pp_token state size token;
      state.pp_left_total <- length + state.pp_left_total;
      (advance_left [@tailcall]) state
    end


(* To enqueue a token : try to advance. *)
let enqueue_advance state tok = pp_enqueue state tok; advance_left state


(* To enqueue strings. *)
let enqueue_string_as state size s =
  enqueue_advance state { size; token = Pp_text s; length = Size.to_int size }


let enqueue_string state s =
  enqueue_string_as state (Size.of_int (String.length s)) s


(* Routines for scan stack
   determine size of boxes. *)

(* The scan_stack is never empty. *)
let initialize_scan_stack stack =
  Stack.clear stack;
  let queue_elem = { size = Size.unknown; token = Pp_text ""; length = 0 } in
  Stack.push { left_total = -1; queue_elem } stack

(* Setting the size of boxes on scan stack:
   if ty = true then size of break is set else size of box is set;
   in each case pp_scan_stack is popped.

   Note:
   Pattern matching on scan stack is exhaustive, since scan_stack is never
   empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes. *)
let set_size state ty =
  match Stack.top_opt state.pp_scan_stack with
  | None -> () (* scan_stack is never empty. *)
  | Some { left_total; queue_elem } ->
    let size = Size.to_int queue_elem.size in
    (* test if scan stack contains any data that is not obsolete. *)
    if left_total < state.pp_left_total then
      initialize_scan_stack state.pp_scan_stack
    else
      match queue_elem.token with
      | Pp_break _ | Pp_tbreak (_, _) ->
        if ty then begin
          queue_elem.size <- Size.of_int (state.pp_right_total + size);
          Stack.pop_opt state.pp_scan_stack |> ignore
        end
      | Pp_begin (_, _) ->
        if not ty then begin
          queue_elem.size <- Size.of_int (state.pp_right_total + size);
          Stack.pop_opt state.pp_scan_stack |> ignore
        end
      | Pp_text _ | Pp_stab | Pp_tbegin _ | Pp_tend | Pp_end
      | Pp_newline | Pp_if_newline | Pp_open_tag _ | Pp_close_tag ->
        () (* scan_push is only used for breaks and boxes. *)


(* Push a token on pretty-printer scanning stack.
   If b is true set_size is called. *)
let scan_push state b token =
  pp_enqueue state token;
  if b then set_size state true;
  let elem = { left_total = state.pp_right_total; queue_elem = token } in
  Stack.push elem state.pp_scan_stack


(* To open a new box :
   the user may set the depth bound pp_max_boxes
   any text nested deeper is printed as the ellipsis string. *)
let pp_open_box_gen state indent br_ty =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let elem = { size; token = Pp_begin (indent, br_ty); length = 0 } in
    scan_push state false elem else
  if state.pp_curr_depth = state.pp_max_boxes
  then enqueue_string state state.pp_ellipsis


(* The box which is always open. *)
let pp_open_sys_box state = pp_open_box_gen state 0 Pp_hovbox

(* Close a box, setting sizes of its sub boxes. *)
let pp_close_box state () =
  if state.pp_curr_depth > 1 then
  begin
    if state.pp_curr_depth < state.pp_max_boxes then
    begin
      pp_enqueue state { size = Size.zero; token = Pp_end; length = 0 };
      set_size state true; set_size state false
    end;
    state.pp_curr_depth <- state.pp_curr_depth - 1;
  end


(* Open a tag, pushing it on the tag stack. *)
let pp_open_stag state tag_name =
  if state.pp_print_tags then
  begin
    Stack.push tag_name state.pp_tag_stack;
    state.pp_print_open_tag tag_name
  end;
  if state.pp_mark_tags then
    let token = Pp_open_tag tag_name in
    pp_enqueue state { size = Size.zero; token; length = 0 }


(* Close a tag, popping it from the tag stack. *)
let pp_close_stag state () =
  if state.pp_mark_tags then
    pp_enqueue state { size = Size.zero; token = Pp_close_tag; length = 0 };
  if state.pp_print_tags then
    match Stack.pop_opt state.pp_tag_stack with
    | None -> () (* No more tag to close. *)
    | Some tag_name ->
      state.pp_print_close_tag tag_name

let pp_set_print_tags state b = state.pp_print_tags <- b
let pp_set_mark_tags state b = state.pp_mark_tags <- b
let pp_get_print_tags state () = state.pp_print_tags
let pp_get_mark_tags state () = state.pp_mark_tags
let pp_set_tags state b =
  pp_set_print_tags state b; pp_set_mark_tags state b


(* Handling tag handling functions: get/set functions. *)
let pp_get_formatter_stag_functions state () = {
  mark_open_stag = state.pp_mark_open_tag;
  mark_close_stag = state.pp_mark_close_tag;
  print_open_stag = state.pp_print_open_tag;
  print_close_stag = state.pp_print_close_tag;
}


let pp_set_formatter_stag_functions state {
     mark_open_stag = mot;
     mark_close_stag = mct;
     print_open_stag = pot;
     print_close_stag = pct;
  } =
  state.pp_mark_open_tag <- mot;
  state.pp_mark_close_tag <- mct;
  state.pp_print_open_tag <- pot;
  state.pp_print_close_tag <- pct


(* Initialize pretty-printer. *)
let pp_rinit state =
  pp_clear_queue state;
  initialize_scan_stack state.pp_scan_stack;
  Stack.clear state.pp_format_stack;
  Stack.clear state.pp_tbox_stack;
  Stack.clear state.pp_tag_stack;
  Stack.clear state.pp_mark_stack;
  state.pp_current_indent <- 0;
  state.pp_curr_depth <- 0;
  state.pp_space_left <- state.pp_margin;
  pp_open_sys_box state

let clear_tag_stack state =
  Stack.iter (fun _ -> pp_close_stag state ()) state.pp_tag_stack


(* Flushing pretty-printer queue. *)
let pp_flush_queue state b =
  clear_tag_stack state;
  while state.pp_curr_depth > 1 do
    pp_close_box state ()
  done;
  state.pp_right_total <- pp_infinity;
  advance_left state;
  if b then pp_output_newline state;
  pp_rinit state

(*

  Procedures to format values and use boxes.

*)

(* To format a string. *)
let pp_print_as_size state size s =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_string_as state size s


let pp_print_as state isize s =
  pp_print_as_size state (Size.of_int isize) s


let pp_print_string state s =
  pp_print_as state (String.length s) s

let pp_print_bytes state s =
  pp_print_as state (Bytes.length s) (Bytes.to_string s)

(* To format an integer. *)
let pp_print_int state i = pp_print_string state (Int.to_string i)

(* To format a float. *)
let pp_print_float state f = pp_print_string state (string_of_float f)

(* To format a boolean. *)
let pp_print_bool state b = pp_print_string state (string_of_bool b)

(* To format a char. *)
let pp_print_char state c =
  pp_print_as state 1 (String.make 1 c)


(* Opening boxes. *)
let pp_open_hbox state () = pp_open_box_gen state 0 Pp_hbox
and pp_open_vbox state indent = pp_open_box_gen state indent Pp_vbox

and pp_open_hvbox state indent = pp_open_box_gen state indent Pp_hvbox
and pp_open_hovbox state indent = pp_open_box_gen state indent Pp_hovbox
and pp_open_box state indent = pp_open_box_gen state indent Pp_box


(* Printing queued text.

   [pp_print_flush] prints all pending items in the pretty-printer queue and
   then flushes the low level output device of the formatter to actually
   display printing material.

   [pp_print_newline] behaves as [pp_print_flush] after printing an additional
   new line. *)
let pp_print_newline state () =
  pp_flush_queue state true; state.pp_out_flush ()
and pp_print_flush state () =
  pp_flush_queue state false; state.pp_out_flush ()


(* To get a newline when one does not want to close the current box. *)
let pp_force_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state { size = Size.zero; token = Pp_newline; length = 0 }


(* To format something, only in case the line has just been broken. *)
let pp_print_if_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state
      { size = Size.zero; token = Pp_if_newline; length = 0 }


(* Generalized break hint that allows printing strings before/after
   same-line offset (width) or new-line offset *)
let pp_print_custom_break state ~fits ~breaks =
  let before, width, after = fits in
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let token = Pp_break { fits; breaks } in
    let length = String.length before + width + String.length after in
    let elem = { size; token; length } in
    scan_push state true elem

(* Printing break hints:
   A break hint indicates where a box may be broken.
   If line is broken then offset is added to the indentation of the current
   box else (the value of) width blanks are printed. *)
let pp_print_break state width offset =
  pp_print_custom_break state
    ~fits:("", width, "") ~breaks:("", offset, "")


(* Print a space :
   a space is a break hint that prints a single space if the break does not
   split the line;
   a cut is a break hint that prints nothing if the break does not split the
   line. *)
let pp_print_space state () = pp_print_break state 1 0
and pp_print_cut state () = pp_print_break state 0 0


(* Tabulation boxes. *)
let pp_open_tbox state () =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.zero in
    let elem = { size; token = Pp_tbegin (Pp_tbox (ref [])); length = 0 } in
    enqueue_advance state elem


(* Close a tabulation box. *)
let pp_close_tbox state () =
  if state.pp_curr_depth > 1 then
  begin
   if state.pp_curr_depth < state.pp_max_boxes then
     let elem = { size = Size.zero; token = Pp_tend; length = 0 } in
     enqueue_advance state elem;
     state.pp_curr_depth <- state.pp_curr_depth - 1
  end


(* Print a tabulation break. *)
let pp_print_tbreak state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let elem = { size; token = Pp_tbreak (width, offset); length = width } in
    scan_push state true elem


let pp_print_tab state () = pp_print_tbreak state 0 0

let pp_set_tab state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem = { size = Size.zero; token = Pp_stab; length = 0 } in
    enqueue_advance state elem


(*

  Procedures to control the pretty-printers

*)

(* Set_max_boxes. *)
let pp_set_max_boxes state n = if n > 1 then state.pp_max_boxes <- n

(* To know the current maximum number of boxes allowed. *)
let pp_get_max_boxes state () = state.pp_max_boxes

let pp_over_max_boxes state () = state.pp_curr_depth = state.pp_max_boxes

(* Ellipsis. *)
let pp_set_ellipsis_text state s = state.pp_ellipsis <- s
and pp_get_ellipsis_text state () = state.pp_ellipsis


(* To set the margin of pretty-printer. *)
let pp_limit n =
  if n < pp_infinity then n else pred pp_infinity


(* Internal pretty-printer functions. *)
let pp_set_min_space_left state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_min_space_left <- n;
    state.pp_max_indent <- state.pp_margin - state.pp_min_space_left;
    pp_rinit state


(* Initially, we have :
   pp_max_indent = pp_margin - pp_min_space_left, and
   pp_space_left = pp_margin. *)
let pp_set_max_indent state n =
  if n > 1 then
    pp_set_min_space_left state (state.pp_margin - n)


let pp_get_max_indent state () = state.pp_max_indent

let pp_set_margin state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_margin <- n;
    let new_max_indent =
      (* Try to maintain max_indent to its actual value. *)
      if state.pp_max_indent <= state.pp_margin
      then state.pp_max_indent else
      (* If possible maintain pp_min_space_left to its actual value,
         if this leads to a too small max_indent, take half of the
         new margin, if it is greater than 1. *)
       Int.max (Int.max (state.pp_margin - state.pp_min_space_left)
                (state.pp_margin / 2)) 1 in
    (* Rebuild invariants. *)
    pp_set_max_indent state new_max_indent


(** Geometry functions and types *)
type geometry = { max_indent:int; margin: int}

let validate_geometry {margin; max_indent} =
  if max_indent < 2 then
    Error "max_indent < 2"
  else if margin <= max_indent then
    Error "margin <= max_indent"
  else Ok ()

let check_geometry geometry =
  match validate_geometry geometry with
  | Ok () -> true
  | Error _ -> false

let pp_get_margin state () = state.pp_margin

let pp_set_full_geometry state {margin; max_indent} =
  pp_set_margin state margin;
  pp_set_max_indent state max_indent;
  ()

let pp_set_geometry state ~max_indent ~margin =
  let geometry = { max_indent; margin } in
  match validate_geometry geometry with
  | Error msg ->
    raise (Invalid_argument ("Format.pp_set_geometry: " ^ msg))
  | Ok () ->
    pp_set_full_geometry state geometry

let pp_safe_set_geometry state ~max_indent ~margin =
  let geometry = { max_indent; margin } in
  match validate_geometry geometry with
  | Error _msg ->
     ()
  | Ok () ->
    pp_set_full_geometry state geometry

let pp_get_geometry state () =
  { margin = pp_get_margin state (); max_indent = pp_get_max_indent state () }

let pp_update_geometry state update =
  let geometry = pp_get_geometry state () in
  pp_set_full_geometry state (update geometry)

(* Setting a formatter basic output functions. *)
let pp_set_formatter_out_functions state {
      out_string = f;
      out_flush = g;
      out_newline = h;
      out_spaces = i;
      out_indent = j;
    } =
  state.pp_out_string <- f;
  state.pp_out_flush <- g;
  state.pp_out_newline <- h;
  state.pp_out_spaces <- i;
  state.pp_out_indent <- j

let pp_get_formatter_out_functions state () = {
  out_string = state.pp_out_string;
  out_flush = state.pp_out_flush;
  out_newline = state.pp_out_newline;
  out_spaces = state.pp_out_spaces;
  out_indent = state.pp_out_indent;
}


(* Setting a formatter basic string output and flush functions. *)
let pp_set_formatter_output_functions state f g =
  state.pp_out_string <- f; state.pp_out_flush <- g

let pp_get_formatter_output_functions state () =
  (state.pp_out_string, state.pp_out_flush)


(* The default function to output new lines. *)
let display_newline state () = state.pp_out_string "\n" 0  1

(* The default function to output spaces. *)
let blank_line = String.make 80 ' '
let rec display_blanks state n =
  if n > 0 then
  if n <= 80 then state.pp_out_string blank_line 0 n else
  begin
    state.pp_out_string blank_line 0 80;
    display_blanks state (n - 80)
  end


(* The default function to output indentation of new lines. *)
let display_indent = display_blanks

(* Setting a formatter basic output functions as printing to a given
   [Stdlib.out_channel] value. *)
let pp_set_formatter_out_channel state oc =
  state.pp_out_string <- output_substring oc;
  state.pp_out_flush <- (fun () -> flush oc);
  state.pp_out_newline <- display_newline state;
  state.pp_out_spaces <- display_blanks state;
  state.pp_out_indent <- display_indent state

(*

  Defining specific formatters

*)

let default_pp_mark_open_tag = function
  | String_tag s -> "<" ^ s ^ ">"
  | _ -> ""
let default_pp_mark_close_tag = function
  | String_tag s -> "</" ^ s ^ ">"
  | _ -> ""

let default_pp_print_open_tag = ignore
let default_pp_print_close_tag = ignore

(* Building a formatter given its basic output functions.
   Other fields get reasonable default values. *)
let pp_make_formatter f g h i j =
  (* The initial state of the formatter contains a dummy box. *)
  let pp_queue = Queue.create () in
  let sys_tok =
    { size = Size.unknown; token = Pp_begin (0, Pp_hovbox); length = 0 } in
  Queue.add sys_tok pp_queue;
  let scan_stack = Stack.create () in
  initialize_scan_stack scan_stack;
  Stack.push { left_total = 1; queue_elem = sys_tok } scan_stack;
  let pp_margin = 78
  and pp_min_space_left = 10 in
  {
    pp_scan_stack = scan_stack;
    pp_format_stack = Stack.create ();
    pp_tbox_stack = Stack.create ();
    pp_tag_stack = Stack.create ();
    pp_mark_stack = Stack.create ();
    pp_margin = pp_margin;
    pp_min_space_left = pp_min_space_left;
    pp_max_indent = pp_margin - pp_min_space_left;
    pp_space_left = pp_margin;
    pp_current_indent = 0;
    pp_is_new_line = true;
    pp_left_total = 1;
    pp_right_total = 1;
    pp_curr_depth = 1;
    pp_max_boxes = max_int;
    pp_ellipsis = ".";
    pp_out_string = f;
    pp_out_flush = g;
    pp_out_newline = h;
    pp_out_spaces = i;
    pp_out_indent = j;
    pp_print_tags = false;
    pp_mark_tags = false;
    pp_mark_open_tag = default_pp_mark_open_tag;
    pp_mark_close_tag = default_pp_mark_close_tag;
    pp_print_open_tag = default_pp_print_open_tag;
    pp_print_close_tag = default_pp_print_close_tag;
    pp_queue = pp_queue;
  }


(* Build a formatter out of its out functions. *)
let formatter_of_out_functions out_funs =
  pp_make_formatter
    out_funs.out_string
    out_funs.out_flush
    out_funs.out_newline
    out_funs.out_spaces
    out_funs.out_indent


(* Make a formatter with default functions to output spaces,
  indentation, and new lines. *)
let make_formatter output flush =
  let ppf = pp_make_formatter output flush ignore ignore ignore in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf.pp_out_indent <- display_indent ppf;
  ppf


(* Make a formatter writing to a given [Stdlib.out_channel] value. *)
let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> flush oc)


(* Make a formatter writing to a given [Buffer.t] value. *)
let formatter_of_buffer b =
  make_formatter (Buffer.add_substring b) ignore


(* Allocating buffer for pretty-printing purposes.
   Default buffer size is pp_buffer_size or 512.
*)
let pp_buffer_size = 512
let pp_make_buffer () = Buffer.create pp_buffer_size

(* The standard (shared) buffer. *)
let stdbuf = pp_make_buffer ()

(* Predefined formatters standard formatter to print
   to [Stdlib.stdout], [Stdlib.stderr], and {!stdbuf}. *)
let std_formatter = formatter_of_out_channel Stdlib.stdout
and err_formatter = formatter_of_out_channel Stdlib.stderr
and str_formatter = formatter_of_buffer stdbuf

(* Initialise domain local state *)
module DLS = Domain.DLS

let stdbuf_key = DLS.new_key pp_make_buffer
let _ = DLS.set stdbuf_key stdbuf

let str_formatter_key = DLS.new_key (fun () ->
  formatter_of_buffer (DLS.get stdbuf_key))
let _ = DLS.set str_formatter_key str_formatter

let buffered_out_string key str ofs len =
  Buffer.add_substring (Domain.DLS.get key) str ofs len

let buffered_out_flush oc key () =
  let buf = Domain.DLS.get key in
  let len = Buffer.length buf in
  let str = Buffer.contents buf in
  output_substring oc str 0 len ;
  Stdlib.flush oc;
  Buffer.clear buf

let std_buf_key = Domain.DLS.new_key (fun () -> Buffer.create pp_buffer_size)
let err_buf_key = Domain.DLS.new_key (fun () -> Buffer.create pp_buffer_size)

let std_formatter_key = DLS.new_key (fun () ->
  let ppf =
    pp_make_formatter (buffered_out_string std_buf_key)
      (buffered_out_flush Stdlib.stdout std_buf_key) ignore ignore ignore
  in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf.pp_out_indent <- display_indent ppf;
  Domain.at_exit (pp_print_flush ppf);
  ppf)
let _ = DLS.set std_formatter_key std_formatter

let err_formatter_key = DLS.new_key (fun () ->
  let ppf =
    pp_make_formatter (buffered_out_string err_buf_key)
      (buffered_out_flush Stdlib.stderr err_buf_key) ignore ignore ignore
  in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf.pp_out_indent <- display_indent ppf;
  Domain.at_exit (pp_print_flush ppf);
  ppf)
let _ = DLS.set err_formatter_key err_formatter

let get_std_formatter () = DLS.get std_formatter_key
let get_err_formatter () = DLS.get err_formatter_key
let get_str_formatter () = DLS.get str_formatter_key
let get_stdbuf () = DLS.get stdbuf_key

(* [flush_buffer_formatter buf ppf] flushes formatter [ppf],
   then returns the contents of buffer [buf] that is reset.
   Formatter [ppf] is supposed to print to buffer [buf], otherwise this
   function is not really useful. *)
let flush_buffer_formatter buf ppf =
  pp_flush_queue ppf false;
  let s = Buffer.contents buf in
  Buffer.reset buf;
  s

(* Flush [str_formatter] and get the contents of [stdbuf]. *)
let flush_str_formatter () =
  let stdbuf = DLS.get stdbuf_key in
  let str_formatter = DLS.get str_formatter_key in
  flush_buffer_formatter stdbuf str_formatter

let make_synchronized_formatter output flush =
  DLS.new_key (fun () ->
    let buf = Buffer.create pp_buffer_size in
    let output' = Buffer.add_substring buf in
    let flush' () =
      output (Buffer.contents buf) 0 (Buffer.length buf);
      Buffer.clear buf;
      flush ()
    in
    make_formatter output' flush')

let synchronized_formatter_of_out_channel oc =
  make_synchronized_formatter (output_substring oc) (fun () -> flush oc)

(*
  Symbolic pretty-printing
*)

(*
  Symbolic pretty-printing is pretty-printing with no low level output.

  When using a symbolic formatter, all regular pretty-printing activities
  occur but output material is symbolic and stored in a buffer of output
  items. At the end of pretty-printing, flushing the output buffer allows
  post-processing of symbolic output before low level output operations.
*)

type symbolic_output_item =
  | Output_flush
  | Output_newline
  | Output_string of string
  | Output_spaces of int
  | Output_indent of int

type symbolic_output_buffer = {
  mutable symbolic_output_contents : symbolic_output_item list;
}

let make_symbolic_output_buffer () =
  { symbolic_output_contents = [] }

let clear_symbolic_output_buffer sob =
  sob.symbolic_output_contents <- []

let get_symbolic_output_buffer sob =
  List.rev sob.symbolic_output_contents

let flush_symbolic_output_buffer sob =
  let items = get_symbolic_output_buffer sob in
  clear_symbolic_output_buffer sob;
  items

let add_symbolic_output_item sob item =
  sob.symbolic_output_contents <- item :: sob.symbolic_output_contents

let formatter_of_symbolic_output_buffer sob =
  let symbolic_flush sob () =
    add_symbolic_output_item sob Output_flush
  and symbolic_newline sob () =
    add_symbolic_output_item sob Output_newline
  and symbolic_string sob s i n =
    add_symbolic_output_item sob (Output_string (String.sub s i n))
  and symbolic_spaces sob n =
    add_symbolic_output_item sob (Output_spaces n)
  and symbolic_indent sob n =
    add_symbolic_output_item sob (Output_indent n) in

  let f = symbolic_string sob
  and g = symbolic_flush sob
  and h = symbolic_newline sob
  and i = symbolic_spaces sob
  and j = symbolic_indent sob in
  pp_make_formatter f g h i j

(*

  Basic functions on the 'standard' formatter
  (the formatter that prints to [Stdlib.stdout]).

*)

let open_hbox v = pp_open_hbox (DLS.get std_formatter_key) v
and open_vbox v = pp_open_vbox (DLS.get std_formatter_key) v
and open_hvbox v = pp_open_hvbox (DLS.get std_formatter_key) v
and open_hovbox v = pp_open_hovbox (DLS.get std_formatter_key) v
and open_box v = pp_open_box (DLS.get std_formatter_key) v
and close_box v = pp_close_box (DLS.get std_formatter_key) v
and open_stag v = pp_open_stag (DLS.get std_formatter_key) v
and close_stag v = pp_close_stag (DLS.get std_formatter_key) v
and print_as v w = pp_print_as (DLS.get std_formatter_key) v w
and print_string v = pp_print_string (DLS.get std_formatter_key) v
and print_bytes v = pp_print_bytes (DLS.get std_formatter_key) v
and print_int v = pp_print_int (DLS.get std_formatter_key) v
and print_float v = pp_print_float (DLS.get std_formatter_key) v
and print_char v = pp_print_char (DLS.get std_formatter_key) v
and print_bool v = pp_print_bool (DLS.get std_formatter_key) v
and print_break v w = pp_print_break (DLS.get std_formatter_key) v w
and print_cut v = pp_print_cut (DLS.get std_formatter_key) v
and print_space v = pp_print_space (DLS.get std_formatter_key) v
and force_newline v = pp_force_newline (DLS.get std_formatter_key) v
and print_flush v = pp_print_flush (DLS.get std_formatter_key) v
and print_newline v = pp_print_newline (DLS.get std_formatter_key) v
and print_if_newline v = pp_print_if_newline (DLS.get std_formatter_key) v

and open_tbox v = pp_open_tbox (DLS.get std_formatter_key) v
and close_tbox v = pp_close_tbox (DLS.get std_formatter_key) v
and print_tbreak v w = pp_print_tbreak (DLS.get std_formatter_key) v w

and set_tab v = pp_set_tab (DLS.get std_formatter_key) v
and print_tab v = pp_print_tab (DLS.get std_formatter_key) v

and set_margin v = pp_set_margin (DLS.get std_formatter_key) v
and get_margin v = pp_get_margin (DLS.get std_formatter_key) v

and set_max_indent v = pp_set_max_indent (DLS.get std_formatter_key) v
and get_max_indent v = pp_get_max_indent (DLS.get std_formatter_key) v

and set_geometry ~max_indent ~margin =
  pp_set_geometry (DLS.get std_formatter_key) ~max_indent ~margin
and safe_set_geometry ~max_indent ~margin =
  pp_safe_set_geometry (DLS.get std_formatter_key) ~max_indent ~margin
and get_geometry v = pp_get_geometry (DLS.get std_formatter_key) v
and update_geometry v = pp_update_geometry (DLS.get std_formatter_key) v

and set_max_boxes v = pp_set_max_boxes (DLS.get std_formatter_key) v
and get_max_boxes v = pp_get_max_boxes (DLS.get std_formatter_key) v
and over_max_boxes v = pp_over_max_boxes (DLS.get std_formatter_key) v

and set_ellipsis_text v = pp_set_ellipsis_text (DLS.get std_formatter_key) v
and get_ellipsis_text v = pp_get_ellipsis_text (DLS.get std_formatter_key) v

and set_formatter_out_channel v =
  pp_set_formatter_out_channel (DLS.get std_formatter_key) v

and set_formatter_out_functions v =
  pp_set_formatter_out_functions (DLS.get std_formatter_key) v
and get_formatter_out_functions v =
  pp_get_formatter_out_functions (DLS.get std_formatter_key) v

and set_formatter_output_functions v w =
  pp_set_formatter_output_functions (DLS.get std_formatter_key) v w
and get_formatter_output_functions v =
  pp_get_formatter_output_functions (DLS.get std_formatter_key) v

and set_formatter_stag_functions v =
  pp_set_formatter_stag_functions (DLS.get std_formatter_key) v
and get_formatter_stag_functions v =
  pp_get_formatter_stag_functions (DLS.get std_formatter_key) v
and set_print_tags v =
  pp_set_print_tags (DLS.get std_formatter_key) v
and get_print_tags v =
  pp_get_print_tags (DLS.get std_formatter_key) v
and set_mark_tags v =
  pp_set_mark_tags (DLS.get std_formatter_key) v
and get_mark_tags v =
  pp_get_mark_tags (DLS.get std_formatter_key) v
and set_tags v =
  pp_set_tags (DLS.get std_formatter_key) v


(* Convenience functions *)

let pp_print_iter ?(pp_sep = pp_print_cut) iter pp_v ppf v =
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_v ppf v
  in
  iter pp_v v

(* To format a list *)
let pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf v =
  pp_print_iter ~pp_sep List.iter pp_v ppf v

(* To format an array *)
let pp_print_array ?(pp_sep = pp_print_cut) pp_v ppf v =
  pp_print_iter ~pp_sep Array.iter pp_v ppf v

(* To format a sequence *)
let pp_print_seq ?(pp_sep = pp_print_cut) pp_v ppf seq =
  pp_print_iter ~pp_sep Seq.iter pp_v ppf seq

(* To format free-flowing text *)
let pp_print_text ppf s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    match s.[!right] with
      | '\n' ->
        flush ();
        pp_force_newline ppf ()
      | ' ' ->
        flush (); pp_print_space ppf ()
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> incr right
  done;
  if !left <> len then flush ()

let pp_print_option ?(none = fun _ () -> ()) pp_v ppf = function
| None -> none ppf ()
| Some v -> pp_v ppf v

let pp_print_result ~ok ~error ppf = function
| Ok v -> ok ppf v
| Error e -> error ppf e

let pp_print_either ~left ~right ppf = function
| Either.Left l -> left ppf l
| Either.Right r -> right ppf r

 (**************************************************************)

let compute_tag output tag_acc =
  let buf = Buffer.create 16 in
  let ppf = formatter_of_buffer buf in
  output ppf tag_acc;
  pp_print_flush ppf ();
  let len = Buffer.length buf in
  if len < 2 then Buffer.contents buf
  else Buffer.sub buf 1 (len - 2)

 (**************************************************************

  Defining continuations to be passed as arguments of
  CamlinternalFormat.make_printf.

  **************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat

(* Interpret a formatting entity on a formatter. *)
let output_formatting_lit ppf fmting_lit = match fmting_lit with
  | Close_box                 -> pp_close_box ppf ()
  | Close_tag                 -> pp_close_stag ppf ()
  | Break (_, width, offset)  -> pp_print_break ppf width offset
  | FFlush                    -> pp_print_flush ppf ()
  | Force_newline             -> pp_force_newline ppf ()
  | Flush_newline             -> pp_print_newline ppf ()
  | Magic_size (_, _)         -> ()
  | Escaped_at                -> pp_print_char ppf '@'
  | Escaped_percent           -> pp_print_char ppf '%'
  | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Differ from Printf.output_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec output_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    output_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    output_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (String.make 1 c);
  | Acc_formatting_lit (p, f) ->
    output_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    output_acc ppf p;
    pp_open_stag ppf (String_tag (compute_tag output_acc acc'))
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    output_acc ppf p;
    let (indent, bty) = open_box_of_string (compute_tag output_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
  | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Differ from Printf.bufput_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec strput_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (String.make 1 c);
  | Acc_delay (Acc_formatting_lit (p, Magic_size (_, size)), f) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (f ());
  | Acc_formatting_lit (p, f) ->
    strput_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    strput_acc ppf p;
    pp_open_stag ppf (String_tag (compute_tag strput_acc acc'))
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    strput_acc ppf p;
    let (indent, bty) = open_box_of_string (compute_tag strput_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> strput_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> strput_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> strput_acc ppf p; pp_print_string ppf (f ());
  | Acc_flush p              -> strput_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> strput_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(*

  Defining [fprintf] and various flavors of [fprintf].

*)

let kfprintf k ppf (Format (fmt, _)) =
  make_printf
    (fun acc -> output_acc ppf acc; k ppf)
    End_of_acc fmt

and ikfprintf k ppf (Format (fmt, _)) =
  make_iprintf k ppf fmt

let ifprintf _ppf (Format (fmt, _)) =
  make_iprintf ignore () fmt

let fprintf ppf = kfprintf ignore ppf

let printf (Format (fmt, _)) =
  make_printf
    (fun acc -> output_acc (DLS.get std_formatter_key) acc)
    End_of_acc fmt

let eprintf (Format (fmt, _)) =
  make_printf
    (fun acc -> output_acc (DLS.get err_formatter_key) acc)
    End_of_acc fmt

let kdprintf k (Format (fmt, _)) =
  make_printf
    (fun acc -> k (fun ppf -> output_acc ppf acc))
    End_of_acc fmt

let dprintf fmt = kdprintf (fun i -> i) fmt

let ksprintf k (Format (fmt, _)) =
  let b = pp_make_buffer () in
  let ppf = formatter_of_buffer b in
  let k acc =
    strput_acc ppf acc;
    k (flush_buffer_formatter b ppf) in
  make_printf k End_of_acc fmt


let sprintf fmt = ksprintf id fmt

let kasprintf k (Format (fmt, _)) =
  let b = pp_make_buffer () in
  let ppf = formatter_of_buffer b in
  let k acc =
    output_acc ppf acc;
    k (flush_buffer_formatter b ppf) in
  make_printf k End_of_acc fmt


let asprintf fmt = kasprintf id fmt

(* Flushing standard formatters at end of execution. *)

let flush_standard_formatters () =
  pp_print_flush (DLS.get std_formatter_key) ();
  pp_print_flush (DLS.get err_formatter_key) ()

let () = at_exit flush_standard_formatters

let () = Domain.before_first_spawn (fun () ->
  flush_standard_formatters ();
  let fs = pp_get_formatter_out_functions std_formatter () in
  pp_set_formatter_out_functions std_formatter
    {fs with out_string = buffered_out_string std_buf_key;
             out_flush = buffered_out_flush Stdlib.stdout std_buf_key};

  let fs = pp_get_formatter_out_functions err_formatter () in
  pp_set_formatter_out_functions err_formatter
    {fs with out_string = buffered_out_string err_buf_key;
             out_flush = buffered_out_flush Stdlib.stderr err_buf_key};
)
