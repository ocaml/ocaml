(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Instruct
open Format
open Debugcom
open Checkpoints
open Events
open Symbols
open Frames
open Source
open Show_source
open Breakpoints
open Parameters

(* Display information about the current event. *)
let show_current_event ppf =
  if !Parameters.time then begin
    fprintf ppf "Time: %Li" (current_time ());
    (match current_pc () with
     | Some pc ->
         fprintf ppf " - pc: %i:%i" pc.frag pc.pos
     | _ -> ());
  end;
  update_current_event ();
  reset_frame ();
  match current_report ()  with
  | None ->
      if !Parameters.time then fprintf ppf "@.";
      fprintf ppf "Beginning of program.@.";
      show_no_point ()
  | Some {rep_type = (Event | Breakpoint); rep_program_pointer = pc} ->
        let ev = (get_current_event ()).ev_ev in
        if !Parameters.time then fprintf ppf " - module %s@." ev.ev_module;
        (match breakpoints_at_pc pc with
         | [] ->
             ()
         | [breakpoint] ->
             fprintf ppf "Breakpoint: %i@." breakpoint
         | breakpoints ->
             fprintf ppf "Breakpoints: %a@."
             (fun ppf l ->
               List.iter
                (function x -> fprintf ppf "%i " x) l)
             (List.sort compare breakpoints));
        show_point ev true
  | Some {rep_type = Exited} ->
      if !Parameters.time then fprintf ppf "@.";
      fprintf ppf "Program exit.@.";
      show_no_point ()
  | Some {rep_type = Uncaught_exc} ->
      if !Parameters.time then fprintf ppf "@.";
      fprintf ppf
        "Program end.@.\
         @[Uncaught exception:@ %a@]@."
      Printval.print_exception (Debugcom.Remote_value.accu ());
      show_no_point ()
  | Some {rep_type = Code_loaded frag} ->
      let mds = String.concat ", " (Symbols.modules_in_code_fragment frag) in
      fprintf ppf "@.Module(s) %s loaded.@." mds;
      show_no_point ()
  | Some {rep_type = Trap_barrier}
  | Some {rep_type = Debug_info _}
  | Some {rep_type = Code_unloaded _} ->
      (* Not visible outside *)
      (* of module `time_travel'. *)
      if !Parameters.time then fprintf ppf "@.";
      Misc.fatal_error "Show_information.show_current_event"

(* Display short information about one frame. *)

let show_one_frame framenum ppf ev =
  let pos = Events.get_pos ev.ev_ev in
  let cnum =
    try
      let buffer = get_buffer pos ev.ev_ev.ev_module in
      snd (start_and_cnum buffer pos)
    with _ -> pos.Lexing.pos_cnum in
  if !machine_readable then
    fprintf ppf "#%i  Pc: %i:%i  %s char %i@."
           framenum ev.ev_frag ev.ev_ev.ev_pos ev.ev_ev.ev_module
           cnum
  else
    fprintf ppf "#%i %s %s:%i:%i@."
           framenum ev.ev_ev.ev_module
           pos.Lexing.pos_fname pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(* Display information about the current frame. *)
(* --- `select frame' must have succeeded before calling this function. *)
let show_current_frame ppf selected =
  match !selected_event with
  | None ->
      fprintf ppf "@.No frame selected.@."
  | Some sel_ev ->
      show_one_frame !current_frame ppf sel_ev;
      begin match breakpoints_at_pc
                    {frag=sel_ev.ev_frag; pos = sel_ev.ev_ev.ev_pos} with
      | [] -> ()
      | [breakpoint] ->
          fprintf ppf "Breakpoint: %i@." breakpoint
      | breakpoints ->
          fprintf ppf "Breakpoints: %a@."
          (fun ppf l ->
            List.iter (function x -> fprintf ppf "%i " x) l)
          (List.sort compare breakpoints);
      end;
      show_point sel_ev.ev_ev selected
