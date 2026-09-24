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
  { loc_start: position; loc_end: position; loc_ghost: bool }

let in_file = Warnings.ghost_loc_in_file

let none = in_file "_none_"
let is_none l = (l = none)

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
}

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
}

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

let rhs_interval m n = {
  loc_start = Parsing.rhs_start_pos m;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none

let map f x = { x with txt = f x.txt }

(******************************************************************************)
(* Input info *)

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)
let input_phrase_buffer = ref (None : Buffer.t option)

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

   We also use for {!is_first_report}, see below.
*)
let num_loc_lines = ref 0

(* We use [num_loc_lines] to determine if the report about to be
   printed is the first or a follow-up report of the current
   "batch" -- contiguous reports without user input in between, for
   example for the current toplevel phrase. We use this to print
   a blank line between messages of the same batch.
*)
let is_first_message () =
  !num_loc_lines = 0

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

(** {1 Printing setup }*)

let setup_tags () =
  Misc.Style.setup !Clflags.color

(******************************************************************************)
(* Printing locations, e.g. 'File "foo.ml", line 3, characters 10-12' *)

let rewrite_absolute_path path =
  match Misc.get_build_path_prefix_map () with
  | None -> path
  | Some map -> Build_path_prefix_map.rewrite map path

let rewrite_find_first_existing path =
  match Misc.get_build_path_prefix_map () with
  | None ->
      if Sys.file_exists path then Some path
      else None
  | Some prefix_map ->
    match Build_path_prefix_map.rewrite_all prefix_map path with
    | [] ->
      if Sys.file_exists path then Some path
      else None
    | matches ->
      Some (List.find Sys.file_exists matches)

let rewrite_find_all_existing_dirs path =
  let ok path = Sys.file_exists path && Sys.is_directory path in
  match Misc.get_build_path_prefix_map () with
  | None ->
      if ok path then [path]
      else []
  | Some prefix_map ->
    match Build_path_prefix_map.rewrite_all prefix_map path with
    | [] ->
        if ok path then [path]
        else []
    | matches ->
      match (List.filter ok matches) with
      | [] -> raise Not_found
      | results -> results

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if (is_relative s) then (concat (Sys.getcwd ()) s) else s in
  let s = rewrite_absolute_path s in
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

type summary = {
  file:string option;
  start_line: int option;
  end_line: int option;
  characters: (int * int) option
}

module Fmt = Format_doc
module Doc = struct

  (* This is used by the toplevel and the report printers below. *)
  let separate_new_message ppf () =
    if not (is_first_message ()) then begin
      Fmt.pp_print_newline ppf ();
      incr num_loc_lines
    end

  let filename ppf file =
    Fmt.pp_print_string ppf (show_filename file)



let summarize_loc loc =
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
  let startline = loc.loc_start.pos_lnum in
  let endline = loc.loc_end.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_end.pos_bol in
  let validate_line l = if line_valid l then Some l else None in
  {
    file = if file_valid file then Some file else None;
    start_line = validate_line startline;
    end_line =  validate_line endline;
    characters =
      if chars_valid ~startchar ~endchar then Some (startchar,endchar)
      else None
  }


(* Best-effort printing of the text describing a location, of the form
   'File "foo.ml", line 3, characters 10-12'.

   Some of the information (filename, line number or characters numbers) in the
   location might be invalid; in which case we do not print it.
 *)
let loc ppf loc =
  let summary = summarize_loc loc in
  setup_tags ();
  let first = ref true in
  let capitalize s =
    if !first then (first := false; String.capitalize_ascii s)
    else s in
  let comma () =
    if !first then () else Fmt.fprintf ppf ", " in
  Fmt.pp_open_stag ppf Misc.Style.Loc;
  Option.iter
    (fun f ->
       Fmt.fprintf ppf "%s \"%a\"" (capitalize "file") filename f
    )
    summary.file
  ;

  (* Print "line 1" in the case of a dummy line number. This is to please the
     existing setup of editors that parse locations in error messages (e.g.
     Emacs). *)
  comma ();
  let start_line = Option.value ~default:1 summary.start_line in
  let end_line = Option.value ~default:start_line summary.end_line in
  begin if start_line = end_line then
    Fmt.fprintf ppf "%s %i" (capitalize "line") start_line
  else
    Fmt.fprintf ppf "%s %i-%i" (capitalize "lines") start_line end_line
  end;
  Option.iter ( fun (startchar,endchar) ->
    comma ();
    Fmt.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  ) summary.characters;
    Fmt.fprintf ppf "@}"

  (* Print a comma-separated list of locations *)
  let locs ppf locs =
    Fmt.pp_print_list ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",@ ")
      loc ppf locs
  let quoted_filename ppf f = Misc.Style.as_inline_code filename ppf f

end

let print_filename = Fmt.compat Doc.filename
let print_loc = Fmt.compat Doc.loc
let print_locs = Fmt.compat Doc.locs
let separate_new_message' ppf =
  Fmt.compat Doc.separate_new_message ppf ()
let separate_new_message log =
  if not (is_first_message ()) then begin
    Log.separate log;
    incr num_loc_lines
  end


(******************************************************************************)
(* An interval set structure; additionally, it stores user-provided information
   at interval boundaries.

   The implementation provided here is naive and assumes the number of intervals
   to be small, but the interface would allow for a more efficient
   implementation if needed.

   Note: the structure only stores maximal intervals (that therefore do not
   overlap).
*)

module ISet : sig
  type 'a bound = 'a * int
  type 'a t
  (* bounds are included *)
  val of_intervals : ('a bound * 'a bound) list -> 'a t

  val mem : 'a t -> pos:int -> bool
  val find_bound_in : 'a t -> range:(int * int) -> 'a bound option

  val is_start : 'a t -> pos:int -> 'a option
  val is_end : 'a t -> pos:int -> 'a option

  val extrema : 'a t -> ('a bound * 'a bound) option
end
=
struct
  type 'a bound = 'a * int

  (* non overlapping intervals *)
  type 'a t = ('a bound * 'a bound) list

  let of_intervals intervals =
    let pos =
      List.map (fun ((a, x), (b, y)) ->
        if x > y then [] else [((a, x), `S); ((b, y), `E)]
      ) intervals
      |> List.flatten
      |> List.sort (fun ((_, x), k) ((_, y), k') ->
        (* Make `S come before `E so that consecutive intervals get merged
           together in the fold below *)
        let kn = function `S -> 0 | `E -> 1 in
        compare (x, kn k) (y, kn k'))
    in
    let nesting, acc =
      List.fold_left (fun (nesting, acc) (a, kind) ->
        match kind, nesting with
        | `S, `Outside -> `Inside (a, 0), acc
        | `S, `Inside (s, n) -> `Inside (s, n+1), acc
        | `E, `Outside -> assert false
        | `E, `Inside (s, 0) -> `Outside, ((s, a) :: acc)
        | `E, `Inside (s, n) -> `Inside (s, n-1), acc
      ) (`Outside, []) pos in
    assert (nesting = `Outside);
    List.rev acc

  let mem iset ~pos =
    List.exists (fun ((_, s), (_, e)) -> s <= pos && pos <= e) iset

  let find_bound_in iset ~range:(start, end_)  =
    List.find_map (fun ((a, x), (b, y)) ->
      if start <= x && x <= end_ then Some (a, x)
      else if start <= y && y <= end_ then Some (b, y)
      else None
    ) iset

  let is_start iset ~pos =
    List.find_map (fun ((a, x), _) ->
      if pos = x then Some a else None
    ) iset

  let is_end iset ~pos =
    List.find_map (fun (_, (b, y)) ->
      if pos = y then Some b else None
    ) iset

  let extrema iset =
    if iset = [] then None
    else Some (fst (List.hd iset), snd (List.hd (List.rev iset)))
end

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

   gets displayed as follows, where X is the line number:

     X | foo the_error bar
             ^^^^^^^^^


   For multi-line errors,

     foo the_
     error bar

   gets displayed as:

     X1 | ....the_
     X2 | error....

   An ellipsis hides the middle lines of the multi-line error if it has more
   than [max_lines] lines.

   If [locs] is empty then this function is a no-op.
*)

type input_line = {
  text : string;
  start_pos : int;
}

(* Takes a list of lines with possibly missing line numbers.

   If the line numbers that are present are consistent with the number of lines
   between them, then infer the intermediate line numbers.

   This is not always the case, typically if lexer line directives are
   involved... *)
let infer_line_numbers
    (lines: (int option * input_line) list):
  (int option * input_line) list
  =
  let (_, offset, consistent) =
    List.fold_left (fun (i, offset, consistent) (lnum, _) ->
      match lnum, offset with
      | None, _ -> (i+1, offset, consistent)
      | Some n, None -> (i+1, Some (n - i), consistent)
      | Some n, Some m -> (i+1, offset, consistent && n = m + i)
    ) (0, None, true) lines
  in
  match offset, consistent with
  | Some m, true ->
      List.mapi (fun i (_, line) -> (Some (m + i), line)) lines
  | _, _ ->
      lines

(* [get_lines] must return the lines to highlight, given starting and ending
   positions.

   See [lines_around_from_current_input] below for an instantiation of
   [get_lines] that reads from the current input.
*)
let highlight_quote ppf
    ~(get_lines: start_pos:position -> end_pos:position -> input_line list)
    ?(max_lines = 10)
    highlight_tag
    locs
  =
  let iset = ISet.of_intervals @@ List.filter_map (fun loc ->
    let s, e = loc.loc_start, loc.loc_end in
    if s.pos_cnum = -1 || e.pos_cnum = -1 then None
    else Some ((s, s.pos_cnum), (e, e.pos_cnum - 1))
  ) locs in
  match ISet.extrema iset with
  | None -> ()
  | Some ((leftmost, _), (rightmost, _)) ->
      let lines =
        get_lines ~start_pos:leftmost ~end_pos:rightmost
        |> List.map (fun ({ text; start_pos } as line) ->
          let end_pos = start_pos + String.length text - 1 in
          let line_nb =
            match ISet.find_bound_in iset ~range:(start_pos, end_pos) with
            | None -> None
            | Some (p, _) -> Some p.pos_lnum
          in
          (line_nb, line))
        |> infer_line_numbers
        |> List.map (fun (lnum, { text; start_pos }) ->
          (text,
           Option.fold ~some:Int.to_string ~none:"" lnum,
           start_pos))
      in
    Fmt.fprintf ppf "@[<v>";
    begin match lines with
    | [] | [("", _, _)] -> ()
    | [(line, line_nb, line_start_cnum)] ->
        (* Single-line error *)
        Fmt.fprintf ppf "%s | %s@," line_nb line;
        Fmt.fprintf ppf "%*s   " (String.length line_nb) "";
        (* Iterate up to [rightmost], which can be larger than the length of
           the line because we may point to a location after the end of the
           last token on the line, for instance:
           {[
             token
                       ^
             Did you forget ...
           ]} *)
        for i = 0 to rightmost.pos_cnum - line_start_cnum - 1 do
          let pos = line_start_cnum + i in
          if ISet.is_start iset ~pos <> None then
            Fmt.fprintf ppf "@{<%s>" highlight_tag;
          if ISet.mem iset ~pos then Fmt.pp_print_char ppf '^'
          else if i < String.length line then begin
            (* For alignment purposes, align using a tab for each tab in the
               source code *)
            if line.[i] = '\t' then Fmt.pp_print_char ppf '\t'
            else Fmt.pp_print_char ppf ' '
          end;
          if ISet.is_end iset ~pos <> None then
            Fmt.fprintf ppf "@}"
        done;
        Fmt.fprintf ppf "@}@,"
    | _ ->
        (* Multi-line error *)
        Fmt.pp_two_columns ~sep:"|" ~max_lines ppf
        @@ List.map (fun (line, line_nb, line_start_cnum) ->
          let line = String.mapi (fun i car ->
            if ISet.mem iset ~pos:(line_start_cnum + i) then car else '.'
          ) line in
          (line_nb, line)
        ) lines
    end;
    Fmt.fprintf ppf "@]"



let lines_around
    ~(start_pos: position) ~(end_pos: position)
    ~(seek: int -> unit)
    ~(read_char: unit -> char option):
  input_line list
  =
  seek start_pos.pos_bol;
  let lines = ref [] in
  let bol = ref start_pos.pos_bol in
  let cur = ref start_pos.pos_bol in
  let b = Buffer.create 80 in
  let add_line () =
    if !bol < !cur then begin
      let text = Buffer.contents b in
      Buffer.clear b;
      lines := { text; start_pos = !bol } :: !lines;
      bol := !cur
    end
  in
  let rec loop () =
    if !bol >= end_pos.pos_cnum then ()
    else begin
      match read_char () with
      | None ->
          (* end of input *)
          add_line ()
      | Some c ->
          incr cur;
          match c with
          | '\r' -> loop ()
          | '\n' -> add_line (); loop ()
          | _ -> Buffer.add_char b c; loop ()
    end
  in
  loop ();
  List.rev !lines

(* Attempt to get lines from the lexing buffer. *)
let lines_around_from_lexbuf
    ~(start_pos: position) ~(end_pos: position)
    (lb: lexbuf):
  input_line list
  =
  (* Converts a global position to one that is relative to the lexing buffer *)
  let rel n = n - lb.lex_abs_pos in
  if rel start_pos.pos_bol < 0 then begin
    (* Do nothing if the buffer does not contain the input (because it has been
       refilled while lexing it) *)
    []
  end else begin
    let pos = ref 0 in (* relative position *)
    let seek n = pos := rel n in
    let read_char () =
      if !pos >= lb.lex_buffer_len then (* end of buffer *) None
      else
        let c = Bytes.get lb.lex_buffer !pos in
        incr pos; Some c
    in
    lines_around ~start_pos ~end_pos ~seek ~read_char
  end

(* Attempt to get lines from the phrase buffer *)
let lines_around_from_phrasebuf
    ~(start_pos: position) ~(end_pos: position)
    (pb: Buffer.t):
  input_line list
  =
  let pos = ref 0 in
  let seek n = pos := n in
  let read_char () =
    if !pos >= Buffer.length pb then None
    else begin
      let c = Buffer.nth pb !pos in
      incr pos; Some c
    end
  in
  lines_around ~start_pos ~end_pos ~seek ~read_char

(* A [get_lines] function for [highlight_quote] that reads from the current
   input. *)
let lines_around_from_current_input ~start_pos ~end_pos =
  match !input_lexbuf, !input_phrase_buffer, !input_name with
  | _, Some pb, "//toplevel//" ->
      lines_around_from_phrasebuf pb ~start_pos ~end_pos
  | Some lb, _, _ ->
      lines_around_from_lexbuf lb ~start_pos ~end_pos
  | None, _, _ ->
      []

(******************************************************************************)
(* Reporting errors and warnings *)

type msg = Fmt.t loc

let msg ?(loc = none) fmt =
  Fmt.kdoc_printf (fun txt -> { loc; txt }) fmt

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
  footnote: Fmt.t option;
}

module Error_diagnostic = struct
  type lc = t
  open Diagnostic
  module Error = Compiler_diagnostic.Error
  module Vl = Compiler_diagnostic.V
  let v1 = Compiler_diagnostic.v1

  module Kind = New_sum(Vl)(struct
      let name="error_kind"
      let description = "Kind of reports (error,warning,alert)"
      let update = v1 end)
      ()

  let report_error = Kind.new_constr0 v1 "Report_error"
  let report_alert = Kind.new_constr v1  "Report_alert"  String
  let report_alert_as_error = Kind.new_constr v1 "Report_alert_as_error" String
  let report_warning = Kind.new_constr v1 "Report_warning" String
  let report_warning_as_error =
    Kind.new_constr v1 "Report_warning_as_error" String
  let () = Kind.seal v1


  let loc_summary ~strict loc =
    let line_valid line = if line > 0 then Some line else None in
    let chars_valid ~startchar ~endchar =
      if startchar <> -1 && endchar <> -1 then Some (startchar, endchar)
      else None
    in
    let file =
      (* According to the comment in location.mli, if [pos_fname] is "", we must
         use [!input_name]. *)
      if loc.loc_start.Lexing.pos_fname = "" then !input_name
      else loc.loc_start.pos_fname
    in
    let file = match file with
      | "_none_"->
          if strict then None
          (* This is a dummy placeholder, but we print it anyway to please
             editors that parse locations in error messages (e.g. Emacs). *)
          else (Some file)
      | "" | "//toplevel//" -> None
      | _ -> Some file
    in

    let characters = chars_valid
        ~startchar:(loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
        ~endchar:(loc.loc_end.pos_cnum - loc.loc_end.pos_bol)
    in
    {
      file;
      start_line = line_valid loc.loc_start.pos_lnum;
      end_line = line_valid loc.loc_end.pos_lnum;
      characters;
    }


  type _ extension +=
    | Error_kind: report_kind extension
    | Error: report extension
    | Location: lc extension
    | Msg: Format_doc.t loc extension

  let pull v = function
    | Report_error -> Kind.app v report_error ()
    | Report_warning w -> Kind.app v report_warning w
    | Report_warning_as_error w -> Kind.app v report_warning_as_error w
    | Report_alert w -> Kind.app v report_alert w
    | Report_alert_as_error w -> Kind.app v report_alert_as_error w


  module Loc = struct
    include New_record(Vl)(struct
        let name = "loc"
        let description = "Source location information"
        let update = v1
      end)()
    let file = new_field_opt v1 "file" String
    let start_line = new_field_opt v1 "start_line" Int
    let end_line = new_field_opt v1 "stop_line" Int
    let characters = new_field_opt v1 "characters" (Pair(Int,Int))
    let () = seal v1
    let ctyp =
      let pull v l =
        let l = loc_summary ~strict:true l in
        make v [
          characters ^=? l.characters;
          file ^=? l.file;
          start_line ^=? l.start_line;
          end_line ^=? l.end_line
        ]
      in
      Custom {
        id = Location; pull; default = Record scheme
      }
  end

  let doc = Compiler_diagnostic.doc

  module Msg = New_record(Vl)(struct
      let name="error_msg"
      let description = "Error message bundled with an optional location"
      let update=v1
    end)()
  let msg = Msg.new_field v1 "msg" doc
  let msg_loc = Msg.new_field_opt v1 "loc" Loc.ctyp
  let () = Msg.seal v1
  let msg_typ =
    let pull v m =
      Msg.(make v [ msg ^= m.txt; msg_loc ^= m.loc ]) in
    Custom { id = Msg; pull; default = Record Msg.scheme }

  let kind = Error.new_field v1 "kind"
      (Custom { id = Error_kind; pull; default = Sum Kind.scheme })

  let main = Error.new_field v1 "main" msg_typ
  let sub = Error.new_field_opt v1 "sub" (Diagnostic.List msg_typ)
  let footnote = Error.new_field_opt v1 "footnote" doc

  let () = Error.seal v1

  let pull v (report:report) =
    let open Error in
    make v
    [
      kind ^= report.kind;
      main ^= report.main;
      sub ^= report.sub;
      footnote ^=? report.footnote
    ]
  let report_typ = Custom { id = Error; pull; default = Error.raw_type }
  let key = Compiler_diagnostic.new_field_opt v1 "error" report_typ
  let warnings =
    Compiler_diagnostic.new_field_opt v1 "warnings" (List report_typ)
  let alerts = Compiler_diagnostic.new_field_opt v1 "alerts" (List report_typ)
  let () = Compiler_diagnostic.seal v1

end

type 'a printer = Format.formatter -> 'a -> unit
type report_printer = {
  pp_report_kind : report_kind printer;
  pp_main_loc: (report_kind * t) printer;
  pp_sub_loc : (report_kind * t) printer;
  pp_msg : Format_doc.t printer;
  pp_highlight_locs: report printer;
}

let is_dummy_loc loc =
  (* Fixme: this should be just [loc.loc_ghost] and the function should be
     inlined below. However, currently, the compiler emits in some places ghost
     locations with valid ranges that should still be printed. These locations
     should be made non-ghost -- in the meantime we just check if the ranges are
     valid. *)
  loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1

(* It only makes sense to highlight (i.e. quote or underline the corresponding
   source code) locations that originate from the current input.

   As of now, this should only happen in the following cases:

   - if dummy locs or ghost locs leak out of the compiler or a buggy ppx;

   - more generally, if some code uses the compiler-libs API and feeds it
   locations that do not match the current values of [!Location.input_name],
   [!Location.input_lexbuf];

   - when calling the compiler on a .ml file that contains lexer line directives
   indicating an other file. This should happen relatively rarely in practice --
   in particular this is not what happens when using -pp or -ppx or a ppx
   driver.
*)
let is_quotable_loc loc =
  not (is_dummy_loc loc)
  && loc.loc_start.pos_fname = !input_name
  && loc.loc_end.pos_fname = !input_name

let error_style () =
  match !Clflags.error_style with
  | Some setting -> setting
  | None -> Misc.Error_style.default_setting

let pp_report reporter ppf report =
  let pp_submsg kind ppf { loc; txt } =
    if is_dummy_loc loc then Format.fprintf ppf "@[%a@]" reporter.pp_msg txt
    else
      Format.fprintf ppf "@[%a@[%a@]@]"
        reporter.pp_sub_loc (kind,loc)
        reporter.pp_msg txt
  in
  let pp_submsgs kind ppf msgs =
    List.iter (fun msg ->
      Format.fprintf ppf "@,%a" (pp_submsg kind) msg
    ) msgs
  in
  let pp_footnote ppf = function
    | None -> ()
    | Some msg -> Format.fprintf ppf "@,%a" reporter.pp_msg msg
  in
  let error_format ppf report =
    Format.fprintf ppf "@[<v>%a%a%a: %a@[%a@]%a%a%a@]"
      Format.pp_open_tbox ()
      reporter.pp_main_loc (report.kind, report.main.loc)
      reporter.pp_report_kind report.kind
      Format.pp_set_tab ()
      reporter.pp_msg report.main.txt
      (pp_submsgs report.kind) report.sub
      pp_footnote report.footnote
      Format.pp_close_tbox ()
  in
  let warning_format ppf report =
    Format.fprintf ppf "@[<v>%a@[<b 2>%a: %a@]%a%a@]"
      reporter.pp_main_loc (report.kind, report.main.loc)
      reporter.pp_report_kind report.kind
      reporter.pp_msg report.main.txt
      (pp_submsgs report.kind) report.sub
      pp_footnote report.footnote
  in
    (* Make sure we keep [num_loc_lines] updated.
       The tabulation box is here to give submessage the option
       to be aligned with the main message box
    *)
  separate_new_message' ppf;
  print_updating_num_loc_lines ppf (fun ppf () ->
      match report.kind with
      | Report_error -> error_format ppf report
      | _ -> warning_format ppf report
    ) ();
  incr num_loc_lines


let batch_mode_printer : report_printer =
  let pp_loc ~indent ppf (kind,loc) =
    let tag = match kind with
      | Report_warning_as_error _
      | Report_alert_as_error _
      | Report_error -> "error"
      | Report_warning _
      | Report_alert _ -> "warning"
    in
    match error_style () with
    | Misc.Error_style.Contextual when is_quotable_loc loc ->
       let highlight ppf loc =
         highlight_quote ppf
          ~get_lines:lines_around_from_current_input
          tag [loc]
        in
        Format.fprintf ppf "@[<v>@[%a@]:@,%a@]%s" print_loc loc
        (Fmt.compat highlight) loc
        (if indent then "  " else "")
    | Misc.Error_style.(Short|Contextual) ->
        Format.fprintf ppf "%a:" print_loc loc;
        if indent then Format.pp_print_break ppf 1 2
        else Format.pp_print_space ppf ()
  in
  let pp_report_kind ppf = function
    | Report_error ->
        Format.fprintf ppf "%aError@}"
          Format.pp_open_stag Misc.Style.Error
    | Report_warning w ->
        Format.fprintf ppf "%aWarning@} %s"
          Format.pp_open_stag Misc.Style.Warning
          w
    | Report_warning_as_error w ->
        Format.fprintf ppf "%aError@} (warning %s)"
          Format.pp_open_stag Misc.Style.Error
          w
    | Report_alert w ->
        Format.fprintf ppf "%aAlert@} %s"
          Format.pp_open_stag Misc.Style.Warning
          w
    | Report_alert_as_error w ->
        Format.fprintf ppf "%aError@} (alert %s)"
          Format.pp_open_stag Misc.Style.Error
          w
  in
  let pp_msg ppf txt = Format.fprintf ppf "%a" Fmt.Doc.format txt in
  let pp_highlight_locs _ _  = () in
  let pp_main_loc = pp_loc ~indent:false in
  let pp_sub_loc ppf (_,loc as kloc) =
    if not loc.loc_ghost then pp_loc ~indent:true ppf kloc
  in
  { pp_report_kind; pp_msg; pp_sub_loc; pp_main_loc; pp_highlight_locs }

let make_quotable_locs main sub =
    let sub_locs = List.map (fun { loc; _ } -> loc) sub in
    let all_locs = main.loc :: sub_locs in
    List.filter is_quotable_loc all_locs

let terminfo_toplevel_printer (lb: lexbuf): report_printer =
  let pp_highlight_locs ppf report =
    (* Highlight all toplevel locations of the report, instead of displaying
       the main location. Do it now instead of in [pp_main_loc], to avoid
       messing with Format boxes. *)
       highlight_terminfo lb ppf (make_quotable_locs report.main report.sub)
  in
  let pp_main_loc _ _ = () in
  let pp_sub_loc ppf (_,loc) =
    if not loc.loc_ghost then
      Format.fprintf ppf "%a:@ " print_loc loc in
  { batch_mode_printer with pp_main_loc; pp_sub_loc; pp_highlight_locs }

let best_toplevel_printer () =
  setup_terminal ();
  match !status, !input_lexbuf with
  | Terminfo.Good_term, Some lb ->
      terminfo_toplevel_printer lb
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
  pp_report printer ppf report

let log_report log report = log.Log.%[Error_diagnostic.key] <- report

(******************************************************************************)
(* Reporting errors *)

type error = report
type delayed_msg = unit -> Fmt.t option


let mkerror loc sub footnote txt =
  { kind = Report_error; main = { loc; txt }; sub; footnote=footnote () }

let errorf ?(loc = none) ?(sub = []) ?(footnote=Fun.const None) =
  Fmt.kdoc_printf (mkerror loc sub footnote)

let multiple_errors ?loc ?footnote = function
  | [error] -> error
  | errors ->
      let sub = List.concat_map (fun err -> err.main :: err.sub) errors in
      errorf ?loc ?footnote ~sub "Multiple errors were encountered@,"

let aligned_error_hint
    ?(loc = none) ?(sub = []) ?(footnote=Fun.const None) fmt =
  Fmt.kdoc_printf (fun main hint ->
      match hint with
      | None -> mkerror loc sub footnote main
      | Some hint ->
          let main, hint = Misc.align_error_hint ~main ~hint in
          mkerror loc (mknoloc hint :: sub) footnote main
  ) fmt

let error ?(loc = none) ?(sub = []) ?(footnote=Fun.const None) msg_str =
  mkerror loc sub footnote Fmt.Doc.(string msg_str empty)

let error_of_printer ?(loc = none) ?(sub = []) ?(footnote=Fun.const None) pp x =
  mkerror loc sub footnote (Fmt.doc_printf "%a" pp x)

let error_of_printer_file print x =
  error_of_printer ~loc:(in_file !input_name) print x

(******************************************************************************)
(* Reporting warnings: generating a report from a warning number using the
   information in [Warnings] + convenience functions. *)

let default_warning_alert_reporter report mk (loc: t) w : report option =
  match report w with
  | `Inactive -> None
  | `Active { Warnings.id; message; is_error; sub_locs } ->
      let kind = mk is_error id in
      let main = { loc; txt = message } in
      let sub = List.map (fun (loc, sub_message) ->
        { loc; txt = sub_message }
      ) sub_locs in
      Some { kind; main; sub; footnote=None }

let default_warning_reporter =
  default_warning_alert_reporter
    Warnings.report
    (fun is_error id ->
       if is_error then Report_warning_as_error id
       else Report_warning id
    )

let warning_reporter = ref default_warning_reporter
let report_warning loc w = !warning_reporter loc w

let error_extension: type a. a Diagnostic.extension -> a printer option =
  function
  | Error_diagnostic.Error -> Some print_report
  | Error_diagnostic.Msg -> None
  | _ -> None

let () =
  Diagnostic_backends.add_extension { extension = error_extension }


let formatter_for_warnings = ref Format.err_formatter

let create_log device =
  let default_backend = Diagnostic_backends.fmt in
  let log =
    Clflags.create_log ~default_backend
      Compiler_diagnostic.V.history Compiler_diagnostic.scheme device
  in
  if !formatter_for_warnings != Format.err_formatter then
    Log.redirect log Error_diagnostic.warnings
      (Log.Device.make formatter_for_warnings);
  log

let current_log = ref (create_log @@ Log.Device.make formatter_for_warnings)


let temporary_log () = Log.tmp Compiler_diagnostic.scheme

let log_on_device ?prev device =
  let log = create_log device in
  current_log := log;
  Option.iter (fun prev -> Log.replay ~source:prev ~dest:log) prev;
  log

let log_warning loc log w =
  Option.iter
    (Log.cons log Error_diagnostic.warnings)
    (report_warning loc w)

let prerr_warning loc w = log_warning loc !current_log w

let default_alert_reporter =
  default_warning_alert_reporter
    Warnings.report_alert
    (fun is_error id ->
       if is_error then Report_alert_as_error id
       else Report_alert id
    )

let alert_reporter = ref default_alert_reporter
let report_alert loc w = !alert_reporter loc w

let log_alert loc log w =
  match report_alert loc w with
  | None -> ()
  | Some report -> Log.( log.%[Error_diagnostic.alerts] <- [report] )

let prerr_alert loc w = log_alert loc !current_log w

let alert ?(def = none) ?(use = none) ~kind loc message =
  prerr_alert loc {Warnings.kind; message; def; use}

let deprecated ?def ?use loc message =
  alert ?def ?use ~kind:"deprecated" loc message

module Style = Misc.Style

let auto_include_alert lib =
  let message = Fmt.asprintf "\
    OCaml's lib directory layout changed in 5.0. The %a subdirectory has been \
    automatically added to the search path, but you should add %a to the \
    command-line to silence this alert (e.g. by adding %a to the list of \
    libraries in your dune file, or adding %a to your %a file for \
    ocamlbuild, or using %a for ocamlfind)."
      Style.inline_code lib
      Style.inline_code ("-I +" ^lib)
      Style.inline_code lib
      Style.inline_code ("use_"^lib)
      Style.inline_code "_tags"
      Style.inline_code ("-package " ^ lib) in
  let alert =
    {Warnings.kind="ocaml_deprecated_auto_include"; use=none; def=none;
     message = Format.asprintf "@[@\n%a@]" Format.pp_print_text message}
  in
  prerr_alert (in_file !input_name) alert

let deprecated_script_alert program =
  let message = Fmt.asprintf "\
    Running %a where the first argument is an implicit basename with no \
    extension (e.g. %a) is deprecated. Either rename the script \
    (%a) or qualify the basename (%a)"
      Style.inline_code program
      Style.inline_code (program ^ " script-file")
      Style.inline_code (program ^ " script-file.ml")
      Style.inline_code (program ^ " ./script-file")
  in
  let alert =
    {Warnings.kind="ocaml_deprecated_cli"; use=none; def=none;
     message = Format.asprintf "@[@\n%a@]" Format.pp_print_text message}
  in
  prerr_alert none alert

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
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let log_exception log exn =
  let rec loop n exn =
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> log.Log.%[Error_diagnostic.key] <- err
    | exception exn when n > 0 -> loop (n-1) exn
  in
  loop 5 exn

let report_exception ppf exn =
  let dev = Log.Device.make (ref ppf) in
  let log = log_on_device dev in
  log_exception log exn;
  Log.close log

exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(footnote=Fun.const None) =
  Fmt.kdoc_printf (fun txt -> raise (Error (mkerror loc sub footnote txt)))
