(* Tokens are one of the following : *)

type pp_token = 
     Pp_text of string            (* normal text *)
   | Pp_break of int * int        (* complete break *)
   | Pp_tbreak of int * int       (* go to next tab *)
   | Pp_stab                      (* set a tabulation *)
   | Pp_begin of int * block_type (* beginning of a block *)
   | Pp_end                       (* end of a block *)
   | Pp_tbegin of tblock          (* Beginning of a tabulation block *)
   | Pp_tend                      (* end of a tabulation block *)
   | Pp_newline                   (* to force a newline inside a block *)
   | Pp_if_newline                 (* to do something only if this very
                                      line has been broken *)

and block_type =
    Pp_hbox   (* Horizontal block no line breaking *)
  | Pp_vbox   (* Vertical block each break leads to a new line *)
  | Pp_hvbox  (* Horizontal-vertical block: same as vbox, except if this block
                 is small enough to fit on a single line *)
  | Pp_hovbox (* Horizontal or Vertical block: breaks lead to new line
                 only when necessary to print the content of the block *)
  | Pp_fits   (* Internal usage: when a block fits on a single line *)

and tblock = Pp_tbox of int list ref  (* Tabulation box *)

(* The Queue: contains all formatting elements.
   elements are tuples (size,token,length), where
   size is set when the size of the block is known
   len is the declared length of the token *)
type pp_queue_elem =
    {mutable elem_size : int; token : pp_token; length : int}

(* Scan stack
   each element is (left_total, queue element) where left_total
   is the value of pp_left_total when the element has been enqueued *)
type pp_scan_elem = Scan_elem of int * pp_queue_elem
let pp_scan_stack = ref ([] : pp_scan_elem list)

(* Formatting Stack:
   used to break the lines while printing tokens.
   The formatting stack contains the description of
   the currently active blocks. *)
type pp_format_elem = Format_elem of block_type * int
let pp_format_stack = ref ([]:pp_format_elem list)

let pp_tbox_stack = ref ([]:tblock list)

(* Large value for default tokens size *)
let pp_infinity = 9999

(* Global variables: default initialization is
   set_margin 78
   set_min_space_left 0 *)
(* value of right margin *)
let pp_margin = ref 78

(* Minimal space left before margin, when opening a block *)
let pp_min_space_left = ref 10
(* maximum value of indentation:
   no blocks can be opened further *)
let pp_max_indent = ref (!pp_margin - !pp_min_space_left)

let pp_space_left = ref !pp_margin(* space remaining on the current line *)
and pp_current_indent = ref 0	  (* current value of indentation *)
and pp_left_total = ref 1	  (* total width of tokens already printed *)
and pp_right_total = ref 1	  (* total width of tokens ever put in queue *)
and pp_curr_depth = ref 0	  (* current number of opened blocks *)
and pp_max_boxes = ref 35	  (* maximum number of blocks which can be
                                     opened at the same time *)
and pp_ellipsis = ref "."         (* ellipsis string *)
and pp_out_channel = ref stdout   (* out_channel of the pretty_printer *)

(* Output functions for the formatter *)
let pp_output s = output !pp_out_channel s
and pp_output_string s = output_string !pp_out_channel s
and pp_output_newline () = output_char !pp_out_channel '\n'

(* The pretty-printer queue *)
let pp_queue = (Queue.new () : pp_queue_elem Queue.t)

let pp_clear_queue () =
    pp_left_total := 1; pp_right_total := 1;
    Queue.clear pp_queue

(* Enter a token in the pretty-printer queue *)
let pp_enqueue ({length=len} as token) =
    pp_right_total := !pp_right_total + len;
    Queue.add token pp_queue

(* To output spaces *)
let blank_line = String.make 80 ' '
let display_blanks n =
    if n > 0 then
    if n <= 80 then pp_output blank_line 0 n
    else pp_output_string (String.make n ' ')

(* To format a break, indenting a new line *)
let break_new_line offset width =
    pp_output_newline ();
    let indent = !pp_margin - width + offset in
    (* Don't indent more than pp_max_indent *)
    let real_indent = min !pp_max_indent indent in
    pp_current_indent := real_indent;
    pp_space_left := !pp_margin - !pp_current_indent;
    display_blanks !pp_current_indent

(* To force a line break inside a block: no offset is added *)
let break_line width = break_new_line 0 width

(* To format a break that fits on the current line *)
let break_same_line width =
    pp_space_left := !pp_space_left - width;
    display_blanks width

(* To indent no more than pp_max_indent, if one tries to open a block
   beyond pp_max_indent, then the block is rejected on the left
   by simulating a break. *)
let pp_force_newline () =
    match !pp_format_stack with
     Format_elem (bl_ty, width) :: _ ->
        if width > !pp_space_left then
         (match bl_ty with
           Pp_fits -> () | Pp_hbox -> () | _ -> break_line width)
   | _ -> pp_output_newline()

(* To skip a token, if the previous line has been broken *)
let pp_skip_token () =
    (* When calling pp_skip_token the queue cannot be empty *)
    match Queue.take pp_queue with
    {elem_size = size; length = len} ->
       pp_left_total := !pp_left_total - len;
       pp_space_left := !pp_space_left + size

(* To format a token *)
let format_pp_token size = function

    Pp_text s -> pp_space_left := !pp_space_left - size; pp_output_string s

  | Pp_begin (off,ty) ->
      let insertion_point = !pp_margin - !pp_space_left in
      if insertion_point > !pp_max_indent then
         (* can't open a block right there ! *)
         pp_force_newline () else
         (* If block is rejected on the left current indentation will change *)
      if size > !pp_space_left & !pp_current_indent < insertion_point then
         pp_force_newline ();
      let offset = !pp_space_left - off in
      let bl_type =
       begin match ty with
          Pp_vbox -> Pp_vbox
        | _ -> if size > !pp_space_left then ty else Pp_fits
       end in
       pp_format_stack := Format_elem (bl_type, offset) :: !pp_format_stack

  | Pp_end ->
      begin match !pp_format_stack with
          x::(y::l as ls) -> pp_format_stack := ls
        | _ -> () (* No more block to close *)
      end

  | Pp_tbegin (Pp_tbox _ as tbox) -> pp_tbox_stack := tbox :: !pp_tbox_stack

  | Pp_tend ->
      begin match !pp_tbox_stack with
          x::ls -> pp_tbox_stack := ls
        | _ -> () (* No more tabulation block to close *)
      end

  | Pp_stab ->
     begin match !pp_tbox_stack with
       Pp_tbox tabs :: _ -> 
        let rec add_tab n = function
            [] -> [n]
          | x::l as ls -> if n < x then n :: ls else x::add_tab n l in
        tabs := add_tab (!pp_margin - !pp_space_left) !tabs
     | _ -> () (* No opened tabulation block *)
     end

  | Pp_tbreak (n,off) ->
      let insertion_point = !pp_margin - !pp_space_left in
      begin match !pp_tbox_stack with
         Pp_tbox tabs :: _ -> 
          let rec find n = function
              x :: l -> if x >= n then x else find n l
            | [] -> raise Not_found in
          let tab =
              match !tabs with
                x :: l ->
                 begin try find insertion_point !tabs with Not_found -> x end
              | _ -> insertion_point in
          let offset = tab - insertion_point in
          if offset >= 0 then break_same_line (offset + n) else
           break_new_line (tab + off) !pp_margin
       | _ -> () (* No opened tabulation block *)
      end

  | Pp_newline ->
     begin match !pp_format_stack with
       Format_elem (_,width) :: _ -> break_line width
     | _ -> pp_output_newline()
     end

  | Pp_if_newline ->
     if !pp_current_indent != !pp_margin - !pp_space_left
      then pp_skip_token ()

  | Pp_break (n,off) ->
     begin match !pp_format_stack with
       Format_elem (ty,width) :: _ ->
        begin match ty with
          Pp_hovbox ->
           if size > !pp_space_left then break_new_line off width else
           (* break the line here leads to new indentation ? *)
           if (!pp_current_indent > !pp_margin - width + off)
            then break_new_line off width else break_same_line n
        | Pp_hvbox -> break_new_line off width
        | Pp_fits -> break_same_line n
        | Pp_vbox  -> break_new_line off width
        | Pp_hbox  -> break_same_line n
        end
     | _ -> () (* No opened block *)
     end

(* Print if token size is known or printing is delayed
   Size is known when not negative
   Printing is delayed when the text waiting in the queue requires
   more room to format than List.exists on the current line *)
let rec advance_left () =
    try
     match Queue.peek pp_queue with
      {elem_size = size; token = tok; length = len} ->
       if not (size < 0 &
               (!pp_right_total - !pp_left_total <= !pp_space_left)) then
        begin
         Queue.take pp_queue;
         format_pp_token (if size < 0 then pp_infinity else size) tok;
         pp_left_total := len + !pp_left_total;
         advance_left ()
        end
    with Queue.Empty -> ()

let enqueue_advance tok = pp_enqueue tok; advance_left ()

(* To enqueue a string : try to advance *)
let enqueue_string_as n s =
    enqueue_advance {elem_size = n; token = Pp_text s; length = n}

let enqueue_string s = enqueue_string_as (String.length s) s

(* Routines for scan stack
   determine sizes of blocks *)
(* scan_stack is never empty *)
let empty_scan_stack =
    [Scan_elem (-1, {elem_size = (-1); token = Pp_text ""; length = 0})]
let clear_scan_stack () = pp_scan_stack := empty_scan_stack

(* Set size of blocks on scan stack:
   if ty = true then size of break is set else size of block is set
   in each case pp_scan_stack is popped *)
(* Pattern matching on scan stack is exhaustive,
   since scan_stack is never empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes *)
let set_size ty =
    match !pp_scan_stack with
      Scan_elem (left_tot,
                 ({elem_size = size; token = tok} as queue_elem)) :: t ->
       (* test if scan stack contains any data that is not obsolete *)
       if left_tot < !pp_left_total then clear_scan_stack () else
        begin match tok with
           Pp_break (_, _) | Pp_tbreak (_, _) ->
            if ty then
             begin
              queue_elem.elem_size <- !pp_right_total + size;
              pp_scan_stack := t
             end
         | Pp_begin (_, _) ->
            if not ty then
             begin
              queue_elem.elem_size <- !pp_right_total + size;
              pp_scan_stack := t
             end
         | _ -> () (* scan_push is only used for breaks and boxes *)
        end
    | _ -> () (* scan_stack is never empty *)

(* Push a token on scan stack. If b is true set_size is called *)
let scan_push b tok =
    pp_enqueue tok;
    if b then set_size true;
    pp_scan_stack := Scan_elem (!pp_right_total,tok) :: !pp_scan_stack

(*
  To open a new block :
  the user may set the depth bound pp_max_boxes
  any text nested deeper is printed as the character the ellipsis
*)
let pp_open_box (indent,br_ty) =
    incr pp_curr_depth;
    if !pp_curr_depth < !pp_max_boxes then
      (scan_push false
        {elem_size = (- !pp_right_total);
         token = Pp_begin (indent, br_ty); length = 0}) else
    if !pp_curr_depth = !pp_max_boxes then enqueue_string !pp_ellipsis

(* The box which is always opened *)
let pp_open_sys_box () =
    incr pp_curr_depth;
    scan_push false
     {elem_size = (- !pp_right_total);
      token = Pp_begin (0, Pp_hovbox); length = 0}

(* close a block, setting sizes of its subblocks *)
let close_box () =
    if !pp_curr_depth > 1 then
     begin
      if !pp_curr_depth < !pp_max_boxes then
       begin
        pp_enqueue {elem_size = 0; token = Pp_end; length = 0};
        set_size true; set_size false
       end;
      decr pp_curr_depth
     end

(* Initialize pretty-printer. *)
let pp_rinit () =
    pp_clear_queue ();
    clear_scan_stack();
    pp_current_indent := 0;
    pp_curr_depth := 0; pp_space_left := !pp_margin;
    pp_format_stack := [];
    pp_tbox_stack := [];
    pp_open_sys_box ()

(* Flushing pretty-printer queue. *)
let pp_flush b =
    while !pp_curr_depth > 1 do
     close_box ()
    done;
    pp_right_total := pp_infinity; advance_left ();
    if b then pp_output_newline ();
    flush !pp_out_channel;
    pp_rinit()

(**************************************************************

  Procedures to format objects, and use boxes

 **************************************************************)

(* To format a string *)
let print_as n s =
    if !pp_curr_depth < !pp_max_boxes then (enqueue_string_as n s)

let print_string s = print_as (String.length s) s

(* To format an integer *)
let print_int i = print_string (string_of_int i)

(* To format a float *)
let print_float f = print_string (string_of_float f)

(* To format a boolean *)
let print_bool b = print_string (string_of_bool b)

(* To format a char *)
let print_char c = print_string (String.make 1 c)

let open_hbox () = pp_open_box (0, Pp_hbox)
and open_vbox indent = pp_open_box (indent, Pp_vbox)

and open_hvbox indent = pp_open_box (indent, Pp_hvbox)
and open_hovbox indent = pp_open_box (indent, Pp_hovbox)

(* Print a new line after printing all queued text
   (same for print_flush but without a newline)    *)
let print_newline () = pp_flush true
and print_flush () = pp_flush false

(* To get a newline when one does not want to close the current block *)
let force_newline () =
    if !pp_curr_depth < !pp_max_boxes
    then enqueue_advance {elem_size = 0; token = Pp_newline; length = 0}

(* To format something if the line has just been broken *)
let print_if_newline () =
    if !pp_curr_depth < !pp_max_boxes
     then enqueue_advance {elem_size = 0; token = Pp_if_newline ;length = 0}

(* Breaks: indicate where a block may be broken.
   If line is broken then offset is added to the indentation of the current
    block else (the value of) width blanks are printed.
   To do (?) : add a maximum width and offset value *)
let print_break (width, offset) =
    if !pp_curr_depth < !pp_max_boxes then 
      scan_push true
       {elem_size = (- !pp_right_total); token = Pp_break (width,offset);
        length = width}

let print_space () = print_break (1,0)
and print_cut () = print_break (0,0)

let open_tbox () =
    incr pp_curr_depth;
    if !pp_curr_depth < !pp_max_boxes then
      enqueue_advance
        {elem_size = 0;
         token = Pp_tbegin (Pp_tbox (ref [])); length = 0}

(* Close a tabulation block *)
let close_tbox () =
    if !pp_curr_depth > 1 then begin
    if !pp_curr_depth < !pp_max_boxes then
     enqueue_advance {elem_size = 0; token = Pp_tend; length = 0};
    decr pp_curr_depth end

(* Print a tabulation break *)
let print_tbreak (width, offset) =
    if !pp_curr_depth < !pp_max_boxes then
      scan_push true
       {elem_size = (- !pp_right_total); token = Pp_tbreak (width,offset); 
        length = width}

let print_tab () = print_tbreak (0,0)

let set_tab () =
    if !pp_curr_depth < !pp_max_boxes
    then enqueue_advance {elem_size = 0; token = Pp_stab; length=0}

(**************************************************************

  Procedures to control the pretty-printer

 **************************************************************)

(* Fit max_boxes *)
let set_max_boxes n = if n > 1 then pp_max_boxes := n

(* To know the current maximum number of boxes allowed *)
let get_max_boxes () = !pp_max_boxes

(* Ellipsis *)
let set_ellipsis_text s = pp_ellipsis := s
and get_ellipsis_text () = !pp_ellipsis

(* To set the margin of pretty-formater *)
let set_margin n =
    if n >= 1 then
     begin
      pp_margin := n;
      pp_max_indent := !pp_margin - !pp_min_space_left;
      pp_rinit () end

let get_margin () = !pp_margin

let set_min_space_left n =
    if n >= 1 then
     begin
      pp_min_space_left := n;
      pp_max_indent := !pp_margin - !pp_min_space_left;
      pp_rinit () end

let set_max_indent n = set_min_space_left (!pp_margin - n)
let get_max_indent () = !pp_max_indent

let set_formatter_output os = pp_out_channel := os
let get_formatter_output () = !pp_out_channel

(* Initializing formatter *)
let _ = pp_rinit()
