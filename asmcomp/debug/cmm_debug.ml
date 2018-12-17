(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Int = Numbers.Int

type t = {
  startup_cmm_file : string;
  ppf : Format.formatter;
  mutable current_pos : Lexing.position;
  mutable start_positions_by_placeholder_line : Lexing.position Int.Map.t;
  mutable end_positions_by_placeholder_line : Lexing.position Int.Map.t;
}

let track_output_position_of_formatter t =
  let ppf = t.ppf in
  Format.pp_set_margin ppf 80;
  let { Format. out_string; out_flush; out_newline; out_spaces; out_indent; } =
    Format.pp_get_formatter_out_functions ppf ()
  in
  let track_newline () =
    let old_pos = t.current_pos in
    let new_pos : Lexing.position =
      { old_pos with
        pos_lnum = old_pos.pos_lnum + 1;
        pos_bol = old_pos.pos_cnum + 1;
        pos_cnum = old_pos.pos_cnum + 1;
      }
    in
    t.current_pos <- new_pos
  in
  let out_string str start_pos num_chars =
    for index = start_pos to start_pos + num_chars - 1 do
      if String.get str index = '\n' then begin
        track_newline ()
      end else begin
        let old_pos = t.current_pos in
        let new_pos : Lexing.position =
          { old_pos with
            pos_cnum = old_pos.pos_cnum + 1;
          }
        in
        t.current_pos <- new_pos
      end
    done;
    out_string str start_pos num_chars
  in
  let out_functions : Format.formatter_out_functions =
    { out_string;
      out_flush;
      (* The default [out_newline] calls [out_string] with a newline character,
         so we don't need to override [out_newline].  A similar situation
         exists for [out_spaces] and [out_indent]. *)
      out_newline;
      out_spaces;
      out_indent;
    }
  in
  Format.pp_set_formatter_out_functions ppf out_functions

let intercept_cmm_location_tags_on_formatter t =
  let ppf = t.ppf in
  let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
  let mark_open_stag stag =
    match Printcmm.parse_placeholder_line_start_tag stag with
    | None -> stag_functions.mark_open_stag stag
    | Some line ->
      t.start_positions_by_placeholder_line
        <- Int.Map.add line t.current_pos t.start_positions_by_placeholder_line;
      ""
  in
  let mark_close_stag stag =
    match Printcmm.parse_placeholder_line_end_tag stag with
    | None -> stag_functions.mark_close_stag stag
    | Some line ->
      t.end_positions_by_placeholder_line
        <- Int.Map.add line t.current_pos t.end_positions_by_placeholder_line;
      ""
  in
  let stag_functions =
    { stag_functions with
      mark_open_stag;
      mark_close_stag;
    }
  in
  Format.pp_set_tags ppf true;
  Format.pp_set_mark_tags ppf true;
  Format.pp_set_formatter_stag_functions ppf stag_functions

let create ~startup_cmm_file ~startup_cmm_chan =
  let ppf = Format.formatter_of_out_channel startup_cmm_chan in
  let starting_pos : Lexing.position =
    { pos_fname = startup_cmm_file;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
  in
  let t =
    { startup_cmm_file;
      ppf;
      current_pos = starting_pos;
      start_positions_by_placeholder_line = Int.Map.empty;
      end_positions_by_placeholder_line = Int.Map.empty;
    }
  in
  track_output_position_of_formatter t;
  intercept_cmm_location_tags_on_formatter t;
  t

let rewrite_code_range t pos =
  let placeholder_line = Debuginfo.Code_range.line pos in
  match
    Int.Map.find placeholder_line t.start_positions_by_placeholder_line
  with
  | exception Not_found ->
    Misc.fatal_errorf "No start position for placeholder line %d"
      placeholder_line
  | start_pos ->
    match
      Int.Map.find placeholder_line t.end_positions_by_placeholder_line
    with
    | exception Not_found ->
      Misc.fatal_errorf "No end position for placeholder line %d"
        placeholder_line
    | end_pos ->
      let line = start_pos.pos_lnum in
      let char_start = start_pos.pos_cnum - start_pos.pos_bol in
      let char_end = end_pos.pos_cnum - start_pos.pos_cnum in
      Debuginfo.Code_range.create ~file:t.startup_cmm_file
        ~line ~char_start ~char_end

let write_cmm_to_channel_and_fix_up_debuginfo t phrase =
  t.start_positions_by_placeholder_line <- Int.Map.empty;
  t.end_positions_by_placeholder_line <- Int.Map.empty;
  Printcmm.phrase' ~no_debuginfo:() t.ppf phrase;
  Format.pp_print_newline t.ppf ();
  Format.pp_print_flush t.ppf ();
  Cmm.map_debuginfo_phrase phrase ~f:(fun dbg ->
      match Debuginfo.position dbg with
      | None -> dbg
      | Some pos ->
        let pos = rewrite_code_range t pos in
        Debuginfo.with_position dbg pos)
    ~f_function:(fun fun_dbg ->
      let pos =
        rewrite_code_range t (Debuginfo.Function.position fun_dbg)
      in
      Debuginfo.Function.with_position fun_dbg pos)
