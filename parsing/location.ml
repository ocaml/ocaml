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
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
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

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

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

let rewrite_absolute_path =
  let init = ref false in
  let map_cache = ref None in
  fun path ->
    if not !init then begin
      init := true;
      match Sys.getenv "BUILD_PATH_PREFIX_MAP" with
      | exception Not_found -> ()
      | encoded_map ->
        match Build_path_prefix_map.decode_map encoded_map with
          | Error err ->
              Misc.fatal_errorf
                "Invalid value for the environment variable \
                 BUILD_PATH_PREFIX_MAP: %s" err
          | Ok map -> map_cache := Some map
    end;
    match !map_cache with
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
  let line_valid line = line <> -1 in
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
  if line_valid line then (
    comma ();
    Format.fprintf ppf "%s %i" (capitalize "line") line
  );
  if chars_valid ~startchar ~endchar then (
    comma ();
    Format.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  );

  if !first then
    (* Nothing has been printed. This might happen if a preprocessor badly
       messes up the locations it produces.
       Print the position characters as a best effort. *)
    Format.fprintf ppf "Characters %i-%i"
      loc.loc_start.pos_cnum loc.loc_end.pos_cnum;

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
let highlight_terminfo ppf lb locs =
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
let highlight_dumb ~print_chars ppf lb locs =
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
    (* Print character location (useful for Emacs) *)
    if print_chars then
      Format.fprintf ppf "%a:@," print_locs locs;
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

let show_code_at_location ppf lb locs =
  try highlight_dumb ~print_chars:false ppf lb locs
  with Exit -> ()

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf locs =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf locs
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            try highlight_dumb ~print_chars:true ppf lb locs; true
            with Exit -> false
      end
  | Terminfo.Good_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf lb locs; true
          with Exit -> false
      end

let reset () =
  num_loc_lines := 0

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let default_printer ppf loc =
  setup_colors ();
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else Format.fprintf ppf "@{<loc>%a@}:@," print_loc loc
;;

let printer = ref default_printer
let print ppf loc = !printer ppf loc

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf =
  setup_colors ();
  Format.fprintf ppf "@{<error>%s@}" error_prefix;
;;

let print_compact ppf loc =
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else begin
    let (file, line, startchar) = get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "%a:%i" print_filename file line;
    if startchar >= 0 then Format.fprintf ppf ",%i--%i" startchar endchar
  end
;;

let print_error ppf loc =
  Format.fprintf ppf "%a%t:" print loc print_error_prefix

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs } ->
    setup_colors ();
    Format.fprintf ppf "@[<v>";
    print ppf loc;
    if is_error
    then
      Format.fprintf ppf "%t (%s %d): %s@," print_error_prefix
        (String.uncapitalize_ascii warning_prefix) number message
    else
      Format.fprintf ppf "@{<warning>%s@} %d: %s@," warning_prefix
        number message;
    List.iter
      (fun (loc, msg) ->
         if loc <> none then Format.fprintf ppf "  %a  %s@," print loc msg
      )
      sub_locs;
    Format.fprintf ppf "@]"

let warning_printer = ref default_warning_printer ;;

let print_warning loc ppf w =
  print_updating_num_loc_lines ppf (!warning_printer loc) w
;;

let formatter_for_warnings = ref Format.err_formatter;;
let prerr_warning loc w = print_warning loc !formatter_for_warnings w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none


type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

let pp_ksprintf ?before k fmt =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  begin match before with
    | None -> ()
    | Some f -> f ppf
  end;
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

(* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted *)
let print_phantom_error_prefix ppf =
  Format.pp_print_as ppf (String.length error_prefix + 2 (* ": " *)) ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  pp_ksprintf
    ~before:print_phantom_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

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

let rec default_error_reporter ppf ({loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" && loc.loc_start.pos_fname = "//toplevel//" then
      let rec collect_locs locs {loc; sub; _} =
        List.fold_left collect_locs (loc :: locs) sub
      in
      let locs = collect_locs [] err in
      highlight_locations ppf locs
    else
      false
  in
  if highlighted then
    Format.pp_print_string ppf if_highlight
  else begin
    Format.fprintf ppf "@[<v>%a %s" print_error loc msg;
    List.iter (Format.fprintf ppf "@,@[<2>%a@]" default_error_reporter) sub;
    Format.fprintf ppf "@]"
  end

let error_reporter = ref default_error_reporter

let report_error ppf err =
  print_updating_num_loc_lines ppf !error_reporter err
;;

let error_of_printer loc print x =
  errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name)
                "I/O error: %s" msg)

      | Misc.HookExnWrapper {error = e; hook_name;
                             hook_info={Misc.sourcefile}} ->
          let sub = match error_of_exn e with
            | None | Some `Already_displayed -> error (Printexc.to_string e)
            | Some (`Ok err) -> err
          in
          Some
            (errorf ~loc:(in_file sourcefile)
               "In hook %S:" hook_name
               ~sub:[sub])
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let report_exception ppf exn =
  let rec loop n exn =
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> Format.fprintf ppf "@[%a@]@." report_error err
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

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  pp_ksprintf
    ~before:print_phantom_error_prefix
    (fun msg -> raise (Error ({loc; msg; sub; if_highlight})))

let deprecated ?(def = none) ?(use = none) loc msg =
  prerr_warning loc (Warnings.Deprecated (msg, def, use))
