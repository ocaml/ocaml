(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

type t = Warnings.loc =
  { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = { dummy_pos with pos_fname = name } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let rhs_interval m n = {
  loc_start = Parsing.rhs_start_pos m;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none

(******************************************************************************)
(* Input info *)

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(******************************************************************************)
(* Terminal info *)

let status = ref Terminfo.Uninitialised

let setup_terminal () =
  if !status = Terminfo.Uninitialised then
    status := Terminfo.setup stdout

(* The number of lines already printed after input.

   This is used by [highlight_terminfo] to identify the current position of the
   input in the terminal. This would not be possible without this information,
   since printing several warnings/errors adds text between the user input and
   the bottom of the terminal.
*)
let num_loc_lines = ref 0

(* This is used by the toplevel to reset [num_loc_lines] before each phrase *)
let reset () =
  num_loc_lines := 0

(* This is used by the toplevel *)
let echo_eof () =
  print_newline ();
  incr num_loc_lines

(* Code printing errors and warnings must be wrapped using this function, in
   order to update [num_loc_lines].

   [print_updating_num_loc_lines ppf f arg] is equivalent to calling [f ppf
   arg], and additionally updates [num_loc_lines]. *)
let print_updating_num_loc_lines ppf f arg =
  let open Format in
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_string str start len =
    let rec count i c =
      if i = start + len then c
      else if String.get str i = '\n' then count (succ i) (succ c)
      else count (succ i) c in
    num_loc_lines := !num_loc_lines + count start 0 ;
    out_functions.out_string str start len in
  pp_set_formatter_out_functions ppf
    { out_functions with out_string } ;
  f ppf arg ;
  pp_print_flush ppf ();
  pp_set_formatter_out_functions ppf out_functions

let setup_colors () =
  Misc.Color.setup !Clflags.color

(******************************************************************************)
(* Printing locations, e.g. 'File "foo.ml", line 3, characters 10-12' *)

let rewrite_absolute_path path =
  match Misc.get_build_path_prefix_map () with
  | None -> path
  | Some map -> Build_path_prefix_map.rewrite map path

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s =
    if not (is_relative s) then s
    else (rewrite_absolute_path (concat (Sys.getcwd ()) s))
  in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !Clflags.absname then absolute_path file else file

let print_filename ppf file =
  Format.pp_print_string ppf (show_filename file)

(* Best-effort printing of the text describing a location, of the form
   'File "foo.ml", line 3, characters 10-12'.

   Some of the information (filename, line number or characters numbers) in the
   location might be invalid; in which case we do not print it.
 *)
let print_loc ppf loc =
  setup_colors ();
  let file_valid = function
    | "_none_" ->
        (* This is a dummy placeholder, but we print it anyway to please editors
           that parse locations in error messages (e.g. Emacs). *)
        true
    | "" | "//toplevel//" -> false
    | _ -> true
  in
  let line_valid line = line > 0 in
  let chars_valid ~startchar ~endchar = startchar <> -1 && endchar <> -1 in

  let file =
    (* According to the comment in location.mli, if [pos_fname] is "", we must
       use [!input_name]. *)
    if loc.loc_start.pos_fname = "" then !input_name
    else loc.loc_start.pos_fname
  in
  let line = loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in

  let first = ref true in
  let capitalize s =
    if !first then (first := false; String.capitalize_ascii s)
    else s in
  let comma () =
    if !first then () else Format.fprintf ppf ", " in

  Format.fprintf ppf "@{<loc>";

  if file_valid file then
    Format.fprintf ppf "%s \"%a\"" (capitalize "file") print_filename file;

  (* Print "line 1" in the case of a dummy line number. This is to please the
     existing setup of editors that parse locations in error messages (e.g.
     Emacs). *)
  comma ();
  Format.fprintf ppf "%s %i" (capitalize "line")
    (if line_valid line then line else 1);

  if chars_valid ~startchar ~endchar then (
    comma ();
    Format.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  );

  Format.fprintf ppf "@}"

(* Print a comma-separated list of locations *)
let print_locs ppf locs =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
    print_loc ppf locs

(******************************************************************************)
(* Toplevel: highlighting and quoting locations *)

(* Highlight the locations using standout mode.

   If [locs] is empty, this function is a no-op.
*)
let highlight_terminfo lb ppf locs =
  Format.pp_print_flush ppf ();  (* avoid mixing Format and normal output *)
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if Bytes.get lb.lex_buffer i = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= Terminfo.num_lines stdout - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup stdout !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if List.exists (fun loc -> pos = loc.loc_start.pos_cnum) locs then
      Terminfo.standout stdout true;
    if List.exists (fun loc -> pos = loc.loc_end.pos_cnum) locs then
      Terminfo.standout stdout false;
    let c = Bytes.get lb.lex_buffer (pos + pos0) in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout stdout false;
  (* Position cursor back to original location *)
  Terminfo.resume stdout !num_loc_lines;
  flush stdout

let highlight_terminfo lb ppf locs =
  try highlight_terminfo lb ppf locs
  with Exit -> ()

(* Highlight the location by printing it again.

   There are two different styles for highlighting errors in "dumb" mode,
   depending if the error fits on a single line or spans across several lines.

   For single-line errors,

     foo the_error bar

   gets displayed as:

     foo the_error bar
         ^^^^^^^^^


   For multi-line errors,

     foo the_
     error bar

   gets displayed as:

     ....the_
     error....

   If [locs] is empty then this function is a no-op.
*)
let highlight_dumb lb ppf locs =
  let locs' = Misc.Stdlib.List.filter_map (fun loc ->
    let s, e = loc.loc_start.pos_cnum, loc.loc_end.pos_cnum in
    (* Ignore dummy locations *)
    if s = -1 || e = -1 then None
    else Some (s, e)
  ) locs
  in
  if locs' = [] then ()
  else begin
    (* Helper to check if a given position is to be highlighted *)
    let is_highlighted pos =
      List.exists (fun (s, e) -> s <= pos && pos < e) locs' in
    (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
    let pos0 = -lb.lex_abs_pos in
    (* Helper to read a char in the buffer *)
    let read_char pos = Bytes.get lb.lex_buffer (pos + pos0) in
    (* Do nothing if the buffer does not contain the whole phrase. *)
    if pos0 < 0 then raise Exit;
    let end_pos = lb.lex_buffer_len - pos0 - 1 in
    (* Leftmost starting position of all locations *)
    let leftmost_start, _ =
      List.hd @@ List.sort (fun (s, _) (s', _) -> compare s s') locs' in
    (* Rightmost ending position of all locations *)
    let _, rightmost_end =
      List.hd @@ List.sort (fun (_, e) (_, e') -> compare e' e) locs' in
    (* Determine line numbers and positions for the start and end points *)
    let line_start, line_end, line_start_pos, line_end_pos =
      let line_start = ref 0 and line_end = ref 0 in
      let line_start_pos = ref 0 and line_end_pos = ref 0 in
      let rec loop pos line line_bol_pos =
        if pos > end_pos then ()
        else if read_char pos = '\n' then
          let line_bol_pos' = pos + 1 in
          if line_bol_pos <= leftmost_start
          && leftmost_start < line_bol_pos' then begin
            line_start := line;
            line_start_pos := line_bol_pos
          end;
          if line_bol_pos <= rightmost_end
          && rightmost_end < line_bol_pos' then begin
            line_end := line;
            line_end_pos := pos
          end;
          loop (pos + 1) (line + 1) line_bol_pos'
        else
          loop (pos + 1) line line_bol_pos
      in
      loop 0 0 0;
      !line_start, !line_end, !line_start_pos, !line_end_pos
    in
    Format.fprintf ppf "@[<v>";
    (* Print the input, highlighting the locations.
       Indent by two spaces. *)
    Format.fprintf ppf "  @[<v>";
    if line_start = line_end then begin
      (* single-line error *)
      for pos = line_start_pos to line_end_pos - 1 do
        Format.pp_print_char ppf (read_char pos)
      done;
      Format.fprintf ppf "@,";
      for pos = line_start_pos to rightmost_end - 1 do
        if is_highlighted pos then Format.pp_print_char ppf '^'
        else Format.pp_print_char ppf ' '
      done
    end else begin
      (* multi-line error *)
      for pos = line_start_pos to line_end_pos - 1 do
        let c = read_char pos in
        if c = '\n' then Format.fprintf ppf "@,"
        else if is_highlighted pos then Format.pp_print_char ppf c
        else Format.pp_print_char ppf '.'
      done
    end;
    Format.fprintf ppf "@]@,@]"
  end

let highlight_dumb lb ppf locs =
  try highlight_dumb lb ppf locs with Exit -> ()

(******************************************************************************)
(* Reporting errors and warnings *)

type msg = (Format.formatter -> unit) loc

let msg ?(loc = none) fmt =
  Format.kdprintf (fun txt -> { loc; txt }) fmt

type report_kind =
  | Report_error
  | Report_warning of int
  | Report_warning_as_error of int

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
}

type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;

  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
  pp_submsgs : report_printer -> report ->
    Format.formatter -> msg list -> unit;
  pp_submsg : report_printer -> report ->
    Format.formatter -> msg -> unit;
  pp_submsg_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_submsg_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
}

let batch_mode_printer : report_printer =
  let pp_loc ppf loc = Format.fprintf ppf "%a:@ " print_loc loc in
  let pp_txt ppf txt = Format.fprintf ppf "@[%t@]" txt in
  let pp self ppf report =
    setup_colors ();
    Format.fprintf ppf "@[<v>%a%a: %a%a@]@."
      (self.pp_main_loc self report) report.main.loc
      (self.pp_report_kind self report) report.kind
      (self.pp_main_txt self report) report.main.txt
      (self.pp_submsgs self report) report.sub
  in
  let pp_report_kind _self _ ppf = function
    | Report_error -> Format.fprintf ppf "@{<error>Error@}"
    | Report_warning w -> Format.fprintf ppf "@{<warning>Warning@} %d" w
    | Report_warning_as_error w ->
        Format.fprintf ppf "@{<error>Error@} (warning %d)" w
  in
  let pp_main_loc _self _ ppf loc =
    pp_loc ppf loc
  in
  let pp_main_txt _self _ ppf txt =
    pp_txt ppf txt
  in
  let pp_submsgs self report ppf msgs =
    List.iter (fun msg ->
      Format.fprintf ppf "@,%a" (self.pp_submsg self report) msg
    ) msgs
  in
  let pp_submsg self report ppf { loc; txt } =
    Format.fprintf ppf "@[<hv 2>%a%a@]"
      (self.pp_submsg_loc self report) loc
      (self.pp_submsg_txt self report) txt
  in
  let pp_submsg_loc _self _ ppf loc =
    if not loc.loc_ghost then
      pp_loc ppf loc
  in
  let pp_submsg_txt _self _ ppf loc =
    pp_txt ppf loc
  in
  { pp; pp_report_kind; pp_main_loc; pp_main_txt;
    pp_submsgs; pp_submsg; pp_submsg_loc; pp_submsg_txt }

let is_dummy_loc loc =
  (* Fixme: this should be just [loc.loc_ghost] and the function should be
     inlined below. However, currently, the compiler emits in some places ghost
     locations with valid ranges that should still be printed. These locations
     should be made non-ghost -- in the meantime we just check if the ranges are
     valid. *)
  loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1

(* It only makes sense to highlight (i.e. quote or underline the corresponding
   source code) locations that originate from the toplevel. *)
let is_toplevel_loc loc =
  not (is_dummy_loc loc)
  && loc.loc_start.pos_fname = "//toplevel//"
  && loc.loc_end.pos_fname = "//toplevel//"

let dumb_toplevel_printer (lb: lexbuf): report_printer =
  let pp self ppf err =
    setup_colors ();
    (* Since we're printing in the toplevel, we have to keep [num_loc_lines]
       updated. *)
    print_updating_num_loc_lines ppf (batch_mode_printer.pp self) err
  in
  let pp_loc _ _ ppf loc =
    let highlight ppf loc =
      if is_toplevel_loc loc then highlight_dumb lb ppf [loc] in
    Format.fprintf ppf "@[<v>%a:@,%a@]" print_loc loc highlight loc
  in
  { batch_mode_printer with pp; pp_main_loc = pp_loc; pp_submsg_loc = pp_loc }

let terminfo_toplevel_printer (lb: lexbuf): report_printer =
  let pp self ppf err =
    setup_colors ();
    (* Highlight all toplevel locations of the report, instead of displaying
       the main location. Do it now instead of in [pp_main_loc], to avoid
       messing with Format boxes. *)
    let sub_locs = List.map (fun { loc; _ } -> loc) err.sub in
    let all_locs = err.main.loc :: sub_locs in
    let locs_highlighted = List.filter is_toplevel_loc all_locs in
    highlight_terminfo lb ppf locs_highlighted;
    (* Make sure we keep [num_loc_lines] updated. *)
    print_updating_num_loc_lines ppf (batch_mode_printer.pp self) err
  in
  let pp_main_loc _ _ _ _ = () in
  { batch_mode_printer with pp; pp_main_loc }

let best_toplevel_printer () =
  setup_terminal ();
  match !status, !input_lexbuf with
  | Terminfo.Good_term, Some lb ->
      terminfo_toplevel_printer lb
  | Terminfo.Bad_term, Some lb ->
      dumb_toplevel_printer lb
  | _, _ ->
      batch_mode_printer

(* Creates a printer for the current input *)
let default_report_printer () : report_printer =
  if !input_name = "//toplevel//" then
    best_toplevel_printer ()
  else
    batch_mode_printer

let report_printer = ref default_report_printer

let print_report ppf report =
  let printer = !report_printer () in
  printer.pp printer ppf report

(******************************************************************************)
(* Reporting errors *)

type error = report

let report_error ppf err =
  print_report ppf err

let mkerror loc sub txt =
  { kind = Report_error; main = { loc; txt }; sub }

let errorf ?(loc = none) ?(sub = []) =
  Format.kdprintf (mkerror loc sub)

let error ?(loc = none) ?(sub = []) msg_str =
  mkerror loc sub (fun ppf -> Format.pp_print_string ppf msg_str)

let error_of_printer ?(loc = none) ?(sub = []) pp x =
  mkerror loc sub (fun ppf -> pp ppf x)

let error_of_printer_file print x =
  error_of_printer ~loc:(in_file !input_name) print x

(******************************************************************************)
(* Reporting warnings: generating a report from a warning number using the
   information in [Warnings] + convenience functions. *)

let default_warning_reporter (loc: t) (w: Warnings.t): report option =
  match Warnings.report w with
  | `Inactive -> None
  | `Active { Warnings.number; message; is_error; sub_locs } ->
      let msg_of_str str = fun ppf -> Format.pp_print_string ppf str in
      let kind =
        if is_error then Report_warning_as_error number
        else Report_warning number in
      let main = { loc; txt = msg_of_str message } in
      let sub = List.map (fun (loc, sub_message) ->
        { loc; txt = msg_of_str sub_message }
      ) sub_locs in
      Some { kind; main; sub }

let warning_reporter = ref default_warning_reporter
let report_warning loc w = !warning_reporter loc w

let formatter_for_warnings = ref Format.err_formatter

let print_warning loc ppf w =
  match report_warning loc w with
  | None -> ()
  | Some report -> print_report ppf report

let prerr_warning loc w = print_warning loc !formatter_for_warnings w

let deprecated ?(def = none) ?(use = none) loc msg =
  prerr_warning loc (Warnings.Deprecated (msg, def, use))

(******************************************************************************)
(* Reporting errors on exceptions *)

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

exception Already_displayed_error = Warnings.Errors

let error_of_exn exn =
  match exn with
  | Already_displayed_error -> Some `Already_displayed
  | _ ->
     let rec loop = function
       | [] -> None
       | f :: rest ->
          match f exn with
          | Some error -> Some (`Ok error)
          | None -> loop rest
     in
     loop !error_of_exn

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name) "I/O error: %s" msg)

      | Misc.HookExnWrapper {error = e; hook_name;
                             hook_info={Misc.sourcefile}} ->
          let sub = match error_of_exn e with
            | None | Some `Already_displayed ->
                [msg "%s" (Printexc.to_string e)]
            | Some (`Ok err) ->
                (msg ~loc:err.main.loc "%t" err.main.txt) :: err.sub
          in
          Some
            (errorf ~loc:(in_file sourcefile) ~sub "In hook %S:" hook_name)
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let report_exception ppf exn =
  let rec loop n exn =
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> report_error ppf err
    | exception exn when n > 0 -> loop (n-1) exn
  in
  loop 5 exn

exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) =
  Format.kdprintf (fun txt -> raise (Error (mkerror loc sub txt)))
