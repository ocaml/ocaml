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
  let empty_position : Lexing.position =
    { pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 1;
    }
  in
  let current_pos = ref empty_position in
  let { Format. out_string; out_flush; out_newline; out_spaces; out_indent; } =
    Format.pp_get_formatter_out_functions ppf ()
  in
  let out_string str start_pos num_chars =
    let substr = String.sub str start_pos num_chars in
    assert (not (String.contains substr '\n'));
    let old_pos = !current_pos in
    let new_pos : Lexing.position =
      { old_pos with
        pos_cnum = old_pos.pos_cnum + num_chars;
      }
    in
    current_pos := new_pos;
    out_string str start_pos num_chars
  in
  let out_newline () =
    let old_pos = !current_pos in
    let new_pos : Lexing.position =
      { old_pos with
        pos_lnum = old_pos.pos_lnum + 1;
        pos_bol = old_pos.pos_cnum + 1;
        pos_cnum = old_pos.pos_cnum + 1;
      }
    in
    current_pos := new_pos;
    out_newline ()
  in
  let out_spaces num_spaces =
    let old_pos = !current_pos in
    let new_pos : Lexing.position =
      { old_pos with
        pos_cnum = old_pos.pos_cnum + num_spaces;
      }
    in
    current_pos := new_pos;
    out_spaces num_spaces
  in
  let out_indent num_spaces =
    let old_pos = !current_pos in
    let new_pos : Lexing.position =
      { old_pos with
        pos_cnum = old_pos.pos_cnum + num_spaces;
      }
    in
    current_pos := new_pos;
    out_indent num_spaces
  in
  let out_functions : Format.formatter_out_functions =
    { out_string;
      out_flush;
      out_newline;
      out_spaces;
      out_indent;
    }
  in
  Format.pp_set_formatter_out_functions ppf out_functions

let intercept_cmm_location_tags_on_formatter t =
  let ppf = t.ppf in
  let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
  let print_open_stag stag =
    match Printcmm.parse_placeholder_line_start_tag stag with
    | None -> stag_functions.print_open_stag stag
    | Some line ->
      t.start_positions_by_placeholder_line
        <- Int.Map.add line t.current_pos t.start_positions_by_placeholder_line
  in
  let print_close_stag stag =
    match Printcmm.parse_placeholder_line_end_tag stag with
    | None -> stag_functions.print_close_stag stag
    | Some line ->
      t.end_positions_by_placeholder_line
        <- Int.Map.add line t.current_pos t.end_positions_by_placeholder_line
  in
  let stag_functions =
    { stag_functions with
      print_open_stag;
      print_close_stag;
    }
  in
  Format.pp_set_formatter_stag_functions ppf stag_functions

let create ~startup_cmm_file ~startup_cmm_chan =
  let ppf = Format.formatter_of_out_channel startup_cmm_chan in
  let t =
    { startup_cmm_file;
      ppf;
      current_pos = Lexing.dummy_pos;
      start_positions_by_placeholder_line = Int.Map.empty;
      end_positions_by_placeholder_line = Int.Map.empty;
    }
  in
  track_output_position_of_formatter t;
  intercept_cmm_location_tags_on_formatter t;
  t

let write_cmm_to_channel_and_fix_up_debuginfo t phrase =
  Printcmm.phrase' ~no_debuginfo:() t.ppf phrase;
  Format.pp_print_flush t.ppf ();
  Cmm.map_debuginfo_phrase phrase ~f:(fun dbg ->
    match Debuginfo.position dbg with
    | None -> dbg
    | Some pos ->
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
          let pos =
            Debuginfo.Code_range.create ~file:t.startup_cmm_file
              ~line ~char_start ~char_end
          in
          Debuginfo.with_position dbg pos)
