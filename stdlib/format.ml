(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Tokens are one of the following : *)

type pp_token = 
| Pp_text of string            (* normal text *)
| Pp_break of int * int        (* complete break *)
| Pp_tbreak of int * int       (* go to next tab *)
| Pp_stab                      (* set a tabulation *)
| Pp_begin of int * block_type (* beginning of a block *)
| Pp_end                       (* end of a block *)
| Pp_tbegin of tblock          (* Beginning of a tabulation block *)
| Pp_tend                      (* end of a tabulation block *)
| Pp_newline                   (* to force a newline inside a block *)
| Pp_if_newline                (* to do something only if this very
                                  line has been broken *)

and block_type =
| Pp_hbox   (* Horizontal block no line breaking *)
| Pp_vbox   (* Vertical block each break leads to a new line *)
| Pp_hvbox  (* Horizontal-vertical block: same as vbox, except if this block
               is small enough to fit on a single line *)
| Pp_hovbox (* Horizontal or Vertical block: breaks lead to new line
               only when necessary to print the content of the block *)
| Pp_box    (* Horizontal or Indent block: breaks lead to new line
               only when necessary to print the content of the block, or
               when it leads to a new indentation of the current line *)
| Pp_fits   (* Internal usage: when a block fits on a single line *)

and tblock = Pp_tbox of int list ref  (* Tabulation box *)
;;

(* The Queue: contains all formatting elements.
   elements are tuples (size, token, length), where
   size is set when the size of the block is known
   len is the declared length of the token *)
type pp_queue_elem =
{mutable elem_size : int; token : pp_token; length : int};;

(* Scan stack
   each element is (left_total, queue element) where left_total
   is the value of pp_left_total when the element has been enqueued *)
type pp_scan_elem = Scan_elem of int * pp_queue_elem;;

(* Formatting Stack:
   used to break the lines while printing tokens.
   The formatting stack contains the description of
   the currently active blocks. *)
type pp_format_elem = Format_elem of block_type * int;;

(* General purpose queues, used in the formatter *)
type 'a queue_elem = | Nil | Cons of 'a queue_cell
and 'a queue_cell = {mutable head : 'a; mutable tail : 'a queue_elem};;

type 'a queue =
{mutable insert : 'a queue_elem;
 mutable body : 'a queue_elem};;

type formatter =
{mutable pp_scan_stack : pp_scan_elem list;
 mutable pp_format_stack : pp_format_elem list;
 mutable pp_tbox_stack : tblock list;
 (* Global variables: default initialization is
    set_margin 78
    set_min_space_left 0 *)
 (* Value of right margin *)
 mutable pp_margin : int;
 (* Minimal space left before margin, when opening a block *)
 mutable pp_min_space_left : int;
 (* Maximum value of indentation:
    no blocks can be opened further *)
 mutable pp_max_indent : int;
 (* Space remaining on the current line *)
 mutable pp_space_left : int;
 (* Current value of indentation *)
 mutable pp_current_indent : int;
 (* True when the line has been broken by the pretty-printer *)
 mutable pp_is_new_line : bool;
 (* Total width of tokens already printed *)
 mutable pp_left_total : int;
 (* Total width of tokens ever put in queue *)
 mutable pp_right_total : int;
 (* Current number of opened blocks *)
 mutable pp_curr_depth : int;
 (* Maximum number of blocks which can be simultaneously opened *)
 mutable pp_max_boxes : int;
 (* Ellipsis string *)
 mutable pp_ellipsis : string;
 (* Output function *)
 mutable pp_output_function : string -> int -> int -> unit;
 (* Flushing function *)
 mutable pp_flush_function : unit -> unit;
 (* The pretty-printer queue *)
 mutable pp_queue : pp_queue_elem queue
};;

(* Qeues *)
let make_queue () = {insert = Nil; body = Nil};;

let clear_queue q = q.insert <- Nil; q.body <- Nil;;

let add_queue x q =
 let c = Cons {head = x; tail = Nil} in
 match q with
 | {insert = Cons cell} -> q.insert <- c; cell.tail <- c
 (* Invariant: when insert is Nil body should be Nil *)
 | _ -> q.insert <- c; q.body <- c;;

exception Empty_queue;;

let peek_queue = function
 | {body = Cons {head = x}} -> x
 | _ -> raise Empty_queue;;

let take_queue = function
 | {body = Cons {head = x; tail = tl}} as q ->
    q.body <- tl;
    if tl = Nil then q.insert <- Nil; (* Maintain the invariant *)
    x
 | _ -> raise Empty_queue;;

(* Large value for default tokens size *)
(* Could be 1073741823 that is 2^30 - 1, that is the minimal upper bound
   of integers *)
let pp_infinity = 999999999;;

(* Output functions for the formatter *)
let pp_output_string state s = state.pp_output_function s 0 (String.length s)
and pp_output_newline state = state.pp_output_function "\n" 0 1;;

let pp_clear_queue state =
    state.pp_left_total <- 1; state.pp_right_total <- 1;
    clear_queue state.pp_queue;;

(* Enter a token in the pretty-printer queue *)
let pp_enqueue state ({length = len} as token) =
    state.pp_right_total <- state.pp_right_total + len;
    add_queue token state.pp_queue;;

(* To output spaces *)
let blank_line = String.make 80 ' ';;
let display_blanks state n =
    if n > 0 then
    if n <= 80 then state.pp_output_function blank_line 0 n
    else pp_output_string state (String.make n ' ');;

(* To format a break, indenting a new line *)
let break_new_line state offset width =
    pp_output_newline state;
    state.pp_is_new_line <- true;
    let indent = state.pp_margin - width + offset in
    (* Don't indent more than pp_max_indent *)
    let real_indent = min state.pp_max_indent indent in
    state.pp_current_indent <- real_indent;
    state.pp_space_left <- state.pp_margin - state.pp_current_indent;
    display_blanks state state.pp_current_indent;;

(* To force a line break inside a block: no offset is added *)
let break_line state width = break_new_line state 0 width;;

(* To format a break that fits on the current line *)
let break_same_line state width =
    state.pp_space_left <- state.pp_space_left - width;
    display_blanks state width;;

(* To indent no more than pp_max_indent, if one tries to open a block
   beyond pp_max_indent, then the block is rejected on the left
   by simulating a break. *)
let pp_force_break_line state =
    match state.pp_format_stack with
    | Format_elem (bl_ty, width) :: _ ->
        if width > state.pp_space_left then
         (match bl_ty with
          | Pp_fits -> () | Pp_hbox -> () | _ -> break_line state width)
    | _ -> pp_output_newline state;;

(* To skip a token, if the previous line has been broken *)
let pp_skip_token state =
    (* When calling pp_skip_token the queue cannot be empty *)
    match take_queue state.pp_queue with
    {elem_size = size; length = len} ->
       state.pp_left_total <- state.pp_left_total - len;
       state.pp_space_left <- state.pp_space_left + size;;

(* To format a token *)
let format_pp_token state size = function

  | Pp_text s ->
      state.pp_space_left <- state.pp_space_left - size;
      pp_output_string state s;
      state.pp_is_new_line <- false

  | Pp_begin (off, ty) ->
      let insertion_point = state.pp_margin - state.pp_space_left in
      if insertion_point > state.pp_max_indent then
         (* can't open a block right there *)
         begin pp_force_break_line state end;
      let offset = state.pp_space_left - off in
      let bl_type =
       begin match ty with
        | Pp_vbox -> Pp_vbox
        | _ -> if size > state.pp_space_left then ty else Pp_fits
       end in
       state.pp_format_stack <-
        Format_elem (bl_type, offset) :: state.pp_format_stack

  | Pp_end ->
      begin match state.pp_format_stack with
        | x :: (y :: l as ls) -> state.pp_format_stack <- ls
        | _ -> () (* No more block to close *)
      end

  | Pp_tbegin (Pp_tbox _ as tbox) ->
      state.pp_tbox_stack <- tbox :: state.pp_tbox_stack

  | Pp_tend ->
      begin match state.pp_tbox_stack with
        | x :: ls -> state.pp_tbox_stack <- ls
        | _ -> () (* No more tabulation block to close *)
      end

  | Pp_stab ->
     begin match state.pp_tbox_stack with
     | Pp_tbox tabs :: _ -> 
        let rec add_tab n = function
          | [] -> [n]
          | x :: l as ls -> if n < x then n :: ls else x :: add_tab n l in
        tabs := add_tab (state.pp_margin - state.pp_space_left) !tabs
     | _ -> () (* No opened tabulation block *)
     end

  | Pp_tbreak (n, off) ->
      let insertion_point = state.pp_margin - state.pp_space_left in
      begin match state.pp_tbox_stack with
      | Pp_tbox tabs :: _ -> 
         let rec find n = function
           | x :: l -> if x >= n then x else find n l
           | [] -> raise Not_found in
         let tab =
             match !tabs with
             | x :: l ->
                begin try find insertion_point !tabs with Not_found -> x end
             | _ -> insertion_point in
         let offset = tab - insertion_point in
         if offset >= 0 then break_same_line state (offset + n) else
          break_new_line state (tab + off) state.pp_margin
      | _ -> () (* No opened tabulation block *)
      end

  | Pp_newline ->
     begin match state.pp_format_stack with
     | Format_elem (_, width) :: _ -> break_line state width
     | _ -> pp_output_newline state
     end

  | Pp_if_newline ->
     if state.pp_current_indent != state.pp_margin - state.pp_space_left
     then pp_skip_token state

  | Pp_break (n, off) ->
     begin match state.pp_format_stack with
     | Format_elem (ty, width) :: _ ->
        begin match ty with
        | Pp_hovbox ->
           if size > state.pp_space_left 
           then break_new_line state off width
           else break_same_line state n
        | Pp_box ->
           (* Have the line just been broken here ? *)
           if state.pp_is_new_line then break_same_line state n else
           if size > state.pp_space_left
            then break_new_line state off width else
           (* break the line here leads to new indentation ? *)
           if state.pp_current_indent > state.pp_margin - width + off
           then break_new_line state off width else break_same_line state n
        | Pp_hvbox -> break_new_line state off width
        | Pp_fits -> break_same_line state n
        | Pp_vbox  -> break_new_line state off width
        | Pp_hbox  -> break_same_line state n
        end
     | _ -> () (* No opened block *)
     end;;

(* Print if token size is known or printing is delayed
   Size is known when not negative
   Printing is delayed when the text waiting in the queue requires
   more room to format than exists on the current line *)
let rec advance_left state =
    try
     match peek_queue state.pp_queue with
      {elem_size = size; token = tok; length = len} ->
       if not
        (size < 0 &&
         (state.pp_right_total - state.pp_left_total < state.pp_space_left))
        then begin
         let _ = take_queue state.pp_queue in
         format_pp_token state (if size < 0 then pp_infinity else size) tok;
         state.pp_left_total <- len + state.pp_left_total;
         advance_left state
        end
    with Empty_queue -> ();;

let enqueue_advance state tok = pp_enqueue state tok; advance_left state;;

(* To enqueue a string : try to advance *)
let enqueue_string_as state n s =
    enqueue_advance state {elem_size = n; token = Pp_text s; length = n};;

let enqueue_string state s = enqueue_string_as state (String.length s) s;;

(* Routines for scan stack
   determine sizes of blocks *)

(* The scan_stack is never empty *)
let scan_stack_bottom =
    [Scan_elem (-1, {elem_size = (-1); token = Pp_text ""; length = 0})];;

(* Set size of blocks on scan stack:
   if ty = true then size of break is set else size of block is set
   in each case pp_scan_stack is popped *)
let clear_scan_stack state = state.pp_scan_stack <- scan_stack_bottom;;

(* Pattern matching on scan stack is exhaustive,
   since scan_stack is never empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes *)
let set_size state ty =
    match state.pp_scan_stack with
    | Scan_elem (left_tot,
                 ({elem_size = size; token = tok} as queue_elem)) :: t ->
       (* test if scan stack contains any data that is not obsolete *)
       if left_tot < state.pp_left_total then clear_scan_stack state else
        begin match tok with
        | Pp_break (_, _) | Pp_tbreak (_, _) ->
           if ty then
            begin
             queue_elem.elem_size <- state.pp_right_total + size;
             state.pp_scan_stack <- t
            end
        | Pp_begin (_, _) ->
           if not ty then
            begin
             queue_elem.elem_size <- state.pp_right_total + size;
             state.pp_scan_stack <- t
            end
        | _ -> () (* scan_push is only used for breaks and boxes *)
        end
    | _ -> () (* scan_stack is never empty *);;

(* Push a token on scan stack. If b is true set_size is called *)
let scan_push state b tok =
    pp_enqueue state tok;
    if b then set_size state true;
    state.pp_scan_stack <-
     Scan_elem (state.pp_right_total, tok) :: state.pp_scan_stack;;

(*
  To open a new block :
  the user may set the depth bound pp_max_boxes
  any text nested deeper is printed as the character the ellipsis
*)
let pp_open_box_gen state indent br_ty =
    state.pp_curr_depth <- state.pp_curr_depth + 1;
    if state.pp_curr_depth < state.pp_max_boxes then
      (scan_push state false
        {elem_size = (- state.pp_right_total);
         token = Pp_begin (indent, br_ty); length = 0}) else
    if state.pp_curr_depth = state.pp_max_boxes
    then enqueue_string state state.pp_ellipsis;;

(* The box which is always opened *)
let pp_open_sys_box state =
    state.pp_curr_depth <- state.pp_curr_depth + 1;
    scan_push state false
     {elem_size = (- state.pp_right_total);
      token = Pp_begin (0, Pp_hovbox); length = 0};;

(* close a block, setting sizes of its subblocks *)
let pp_close_box state () =
    if state.pp_curr_depth > 1 then
     begin
      if state.pp_curr_depth < state.pp_max_boxes then
       begin
        pp_enqueue state {elem_size = 0; token = Pp_end; length = 0};
        set_size state true; set_size state false
       end;
      state.pp_curr_depth <- state.pp_curr_depth - 1;
     end;;

(* Initialize pretty-printer. *)
let pp_rinit state =
    pp_clear_queue state;
    clear_scan_stack state;
    state.pp_current_indent <- 0;
    state.pp_curr_depth <- 0;
    state.pp_space_left <- state.pp_margin;
    state.pp_format_stack <- [];
    state.pp_tbox_stack <- [];
    pp_open_sys_box state;;

(* Flushing pretty-printer queue. *)
let pp_flush state b =
    while state.pp_curr_depth > 1 do
     pp_close_box state ()
    done;
    state.pp_right_total <- pp_infinity; advance_left state;
    if b then pp_output_newline state;
    state.pp_flush_function ();
    pp_rinit state;;

(**************************************************************

  Procedures to format objects, and use boxes

 **************************************************************)

(* To format a string *)
let pp_print_as state n s =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_string_as state n s;;

let pp_print_string state s = pp_print_as state (String.length s) s;;

(* To format an integer *)
let pp_print_int state i = pp_print_string state (string_of_int i);;

(* To format a float *)
let pp_print_float state f = pp_print_string state (string_of_float f);;

(* To format a boolean *)
let pp_print_bool state b = pp_print_string state (string_of_bool b);;

(* To format a char *)
let pp_print_char state c =
  let s = String.create 1 in s.[0] <- c; pp_print_as state 1 s;;

(* Opening boxes *)
let pp_open_hbox state () = pp_open_box_gen state 0 Pp_hbox
and pp_open_vbox state indent = pp_open_box_gen state indent Pp_vbox

and pp_open_hvbox state indent = pp_open_box_gen state indent Pp_hvbox
and pp_open_hovbox state indent = pp_open_box_gen state indent Pp_hovbox
and pp_open_box state indent = pp_open_box_gen state indent Pp_box;;

(* Print a new line after printing all queued text
   (same for print_flush but without a newline)    *)
let pp_print_newline state () = pp_flush state true
and pp_print_flush state () = pp_flush state false;;

(* To get a newline when one does not want to close the current block *)
let pp_force_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state {elem_size = 0; token = Pp_newline; length = 0};;

(* To format something if the line has just been broken *)
let pp_print_if_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state {elem_size = 0; token = Pp_if_newline; length = 0};;

(* Breaks: indicate where a block may be broken.
   If line is broken then offset is added to the indentation of the current
   block else (the value of) width blanks are printed.
   To do (?) : add a maximum width and offset value *)
let pp_print_break state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then 
    scan_push state true
     {elem_size = (- state.pp_right_total); token = Pp_break (width, offset);
      length = width};;

let pp_print_space state () = pp_print_break state 1 0
and pp_print_cut state () = pp_print_break state 0 0;;

(* Tabulation boxes *)
let pp_open_tbox state () =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state
      {elem_size = 0;
       token = Pp_tbegin (Pp_tbox (ref [])); length = 0};;

(* Close a tabulation block *)
let pp_close_tbox state () =
  if state.pp_curr_depth > 1 then begin
   if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state {elem_size = 0; token = Pp_tend; length = 0};
   state.pp_curr_depth <- state.pp_curr_depth - 1 end;;

(* Print a tabulation break *)
let pp_print_tbreak state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    scan_push state true
     {elem_size = (- state.pp_right_total); token = Pp_tbreak (width, offset); 
      length = width};;

let pp_print_tab state () = pp_print_tbreak state 0 0;;

let pp_set_tab state () =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_advance state {elem_size = 0; token = Pp_stab; length=0};;

(**************************************************************

  Procedures to control the pretty-printer

 **************************************************************)

(* Fit max_boxes *)
let pp_set_max_boxes state n = if n > 1 then state.pp_max_boxes <- n;;

(* To know the current maximum number of boxes allowed *)
let pp_get_max_boxes state () = state.pp_max_boxes;;

let pp_over_max_boxes state () = state.pp_curr_depth = state.pp_max_boxes;;

(* Ellipsis *)
let pp_set_ellipsis_text state s = state.pp_ellipsis <- s
and pp_get_ellipsis_text state () = state.pp_ellipsis;;

(* To set the margin of pretty-formater *)
let pp_set_min_space_left state n =
  if n >= 1 && n < pp_infinity then
   begin
    state.pp_min_space_left <- n;
    state.pp_max_indent <- state.pp_margin - state.pp_min_space_left;
    pp_rinit state end;;

(* Initially we have :
  pp_max_indent = pp_margin - pp_min_space_left, and
  pp_space_left = pp_margin
*)
let pp_set_max_indent state n =
  pp_set_min_space_left state (state.pp_margin - n);;
let pp_get_max_indent state () = state.pp_max_indent;;

let pp_set_margin state n =
  if n >= 1 && n < pp_infinity then
   begin
    state.pp_margin <- n;
    let new_max_indent =
        (* Try to maintain max_indent to its actual value *)
        if state.pp_max_indent <= state.pp_margin
        then state.pp_max_indent else
        (* If possible maintain pp_min_space_left to its actual value,
           if this leads to a too small max_indent, take half of the
           new margin, if it is greater than 1 *)
         max (max (state.pp_margin - state.pp_min_space_left)
                  (state.pp_margin / 2)) 1 in
    (* Rebuild invariants *)
    pp_set_max_indent state new_max_indent end;;

let pp_get_margin state () = state.pp_margin;;

let pp_set_formatter_output_functions state f g =
  state.pp_output_function <- f; state.pp_flush_function <- g;;
let pp_set_formatter_out_channel state os = 
  state.pp_output_function <- output os;
  state.pp_flush_function <- (fun () -> flush os);;
let pp_get_formatter_output_functions state () = 
  (state.pp_output_function, state.pp_flush_function);;

let make_formatter f g = 
 (* The initial state of the formatter contains a dummy box *)
 let pp_q = make_queue () in
 let sys_tok =
     {elem_size = (- 1); token = Pp_begin (0, Pp_hovbox); length = 0} in
 add_queue sys_tok pp_q;
 let sys_scan_stack =
     (Scan_elem (1, sys_tok)) :: scan_stack_bottom in
 {pp_scan_stack = sys_scan_stack;
  pp_format_stack = [];
  pp_tbox_stack = [];
  pp_margin = 78;
  pp_min_space_left = 10;
  pp_max_indent = 78 - 10;
  pp_space_left = 78;
  pp_current_indent = 0;
  pp_is_new_line = true;
  pp_left_total = 1;
  pp_right_total = 1;
  pp_curr_depth = 1;
  pp_max_boxes = 35;
  pp_ellipsis = ".";
  pp_output_function = f;
  pp_flush_function = g;
  pp_queue = pp_q
 };;

let std_formatter =
    make_formatter (output stdout) (fun () -> flush stdout);;

let err_formatter =
    make_formatter (output stderr) (fun () -> flush stderr);;

let open_hbox = pp_open_hbox std_formatter
and open_vbox = pp_open_vbox std_formatter
and open_hvbox = pp_open_hvbox std_formatter
and open_hovbox = pp_open_hovbox std_formatter
and open_box = pp_open_box std_formatter
and close_box = pp_close_box std_formatter
and print_as = pp_print_as std_formatter
and print_string = pp_print_string std_formatter
and print_int = pp_print_int std_formatter
and print_float = pp_print_float std_formatter
and print_char = pp_print_char std_formatter
and print_bool = pp_print_bool std_formatter
and print_break = pp_print_break std_formatter
and print_cut = pp_print_cut std_formatter
and print_space = pp_print_space std_formatter
and force_newline = pp_force_newline std_formatter
and print_flush = pp_print_flush std_formatter
and print_newline = pp_print_newline std_formatter
and print_if_newline = pp_print_if_newline std_formatter
and open_tbox = pp_open_tbox std_formatter
and close_tbox = pp_close_tbox std_formatter
and print_tbreak = pp_print_tbreak std_formatter
and set_tab = pp_set_tab std_formatter
and print_tab = pp_print_tab std_formatter
and set_margin = pp_set_margin std_formatter
and get_margin = pp_get_margin std_formatter
and set_max_indent = pp_set_max_indent std_formatter
and get_max_indent = pp_get_max_indent std_formatter
and set_max_boxes = pp_set_max_boxes std_formatter
and get_max_boxes = pp_get_max_boxes std_formatter
and over_max_boxes = pp_over_max_boxes std_formatter
and set_ellipsis_text = pp_set_ellipsis_text std_formatter
and get_ellipsis_text = pp_get_ellipsis_text std_formatter
and set_formatter_out_channel =
    pp_set_formatter_out_channel std_formatter
and set_formatter_output_functions =
    pp_set_formatter_output_functions std_formatter
and get_formatter_output_functions =
    pp_get_formatter_output_functions std_formatter;;

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let fprintf ppf format =
  let format = (Obj.magic format : string) in
  let limit = String.length format in

  let print_as = ref None in

  let pp_print_as_char ppf c =
      match !print_as with
      | None -> pp_print_char ppf c
      | Some size ->
         pp_print_as ppf size (String.make 1 c);
         print_as := None
  and pp_print_as_string ppf s =
      match !print_as with
      | None -> pp_print_string ppf s
      | Some size ->
         pp_print_as ppf size s;
         print_as := None in

  let rec doprn i =
    if i >= limit then
      Obj.magic ()
    else
      match format.[i] with
      | '@' ->
          let j = succ i in
          if j >= limit then invalid_arg ("fprintf: unknown format") else
          begin match format.[j] with
          | '@' ->
              pp_print_char ppf '@';
              doprn (succ j)
          | '[' ->
              let j = do_pp_open ppf (i + 2) in
              doprn j
          | ']' ->
              pp_close_box ppf ();
              doprn (succ j)
          | ' ' ->
              pp_print_space ppf ();
              doprn (succ j)
          | ',' ->
              pp_print_cut ppf ();
              doprn (succ j)
          | '?' ->
              pp_print_flush ppf ();
              doprn (succ j)
          | '.' ->
              pp_print_newline ppf ();
              doprn (succ j)
          | '\n' ->
              pp_force_newline ppf ();
              doprn (succ j)
          | ';' ->
              let j = do_pp_break ppf (i + 2) in
              doprn j
          | '<' ->
              let size, j = get_int "fprintf: bad print format" (i + 2) in
              if format.[pred j] != '>'
               then invalid_arg "fprintf: bad print format"
               else print_as := Some size;
              doprn j
          | _ -> invalid_arg ("fprintf: unknown format") end
      | '%' ->
          let j = skip_args (succ i) in
          begin match format.[j] with
          | '%' ->
              pp_print_char ppf '%';
              doprn (succ j)
          | 's' ->
              Obj.magic(fun s ->
                if j <= i+1 then
                  pp_print_as_string ppf s
                else begin
                  let p =
                    try
                      int_of_string (String.sub format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 && String.length s < p then begin
                    pp_print_as_string ppf
                                  (String.make (p - String.length s) ' ');
                    pp_print_as_string ppf s
                  end else if p < 0 && String.length s < -p then begin
                    pp_print_as_string ppf s;
                    pp_print_as_string ppf
                                  (String.make (-p - String.length s) ' ')
                  end else
                    pp_print_as_string ppf s
                end;
                doprn (succ j))
          | 'c' ->
              Obj.magic(fun c ->
                pp_print_as_char ppf c;
                doprn (succ j))
          | 'd' | 'o' | 'x' | 'X' | 'u' ->
              Obj.magic(fun n ->
                pp_print_as_string ppf
                                (format_int (String.sub format i (j-i+1)) n);
                doprn (succ j))
          | 'f' | 'e' | 'E' | 'g' | 'G' ->
              Obj.magic(fun f ->
                pp_print_as_string ppf
                                (format_float (String.sub format i (j-i+1)) f);
                doprn (succ j))
          | 'b' ->
              Obj.magic(fun b ->
                pp_print_as_string ppf (string_of_bool b);
                doprn(succ j))
          | 'a' ->
              Obj.magic(fun printer arg ->
                printer ppf arg;
                doprn(succ j))
          | 't' ->
              Obj.magic(fun printer ->
                printer ppf;
                doprn(succ j))
          | c ->
              invalid_arg ("fprintf: unknown format")
          end
       | c -> pp_print_as_char ppf c; doprn (succ i)

  and skip_args j =
    match format.[j] with
    | '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  and get_int s i =
   if i >= limit then invalid_arg s else
   match format.[i] with
   | ' ' -> get_int s (i + 1)
   | c ->
      let rec get j =
       if j >= limit then invalid_arg s else
       match format.[j] with
       | '0' .. '9' | '-' -> get (succ j)
       | '>' | ' ' ->
         if j = i then 0, succ j else
          begin try int_of_string (String.sub format i (j - i)), succ j
          with Failure _ -> invalid_arg s end
       | c -> invalid_arg s in
       get i

  and get_box_kind j =
   if j >= limit then Pp_box, j else
   match format.[j] with
   | 'h' ->
      let j = succ j in
      if j >= limit then Pp_hbox, j else
      begin match format.[j] with
      | 'o' -> 
         let j = succ j in
         if j >= limit
          then invalid_arg "fprintf: bad box format" else
         begin match format.[j] with
         | 'v' -> Pp_hovbox, succ j
         | _ ->  invalid_arg "fprintf: bad box format" end
      | 'v' -> Pp_hvbox, succ j
      | c -> Pp_hbox, j
      end
   | 'b' -> Pp_box, succ j
   | 'v' -> Pp_vbox, succ j
   | _ -> Pp_box, j

  and do_pp_break ppf i =
   if i >= limit then begin pp_print_space ppf (); i end else
   match format.[i] with
   | '<' ->
     let nspaces, j = get_int "fprintf: bad break format" (succ i) in
     let offset, j = get_int "fprintf: bad break format" j in
     if format.[pred j] != '>' then invalid_arg "fprintf: bad break format"
     else pp_print_break ppf nspaces offset;
     j
   | c ->  pp_print_space ppf (); i

  and do_pp_open ppf i =
   if i >= limit then begin pp_open_box_gen ppf 0 Pp_box; i end else
   match format.[i] with
   | '<' ->
     let k, j = get_box_kind (succ i) in
     let size, j = get_int "fprintf: bad box format" j in
     pp_open_box_gen ppf size k;
     j
   | c ->  pp_open_box_gen ppf 0 Pp_box; i

  in doprn 0
;;

let printf f = fprintf std_formatter f;;
let eprintf f = fprintf err_formatter f;;

let _ = at_exit print_flush;;

