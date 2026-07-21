(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Frederic Bour,             *)
(*                           Xavier Van de Woestyne                       *)
(*                                                                        *)
(*   Copyright 2025 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module RawTypeHash = Types.TransientTypeHash

let ref_recoverable_handlers : (exn -> bool) list ref = ref []
let ref_errors : (exn list ref * unit RawTypeHash.t) option ref = ref None
let ref_monitor_errors =
  (* A ref of ref implements scoped errors.
     The outer ref points to the current error context.
     Each recursive call (e. g.: on nested Pexp_sequence) can
     install a fresh inner ref, giving it an isolated error state. *)
  ref (ref false)

let monitor_errors () =
  if !(!ref_monitor_errors) then
    (* if there was an error, we start from a clean slate, otherwise we
       keep the same context so the ancestor knows about it as well. *)
    ref_monitor_errors := ref false;
  !ref_monitor_errors

let log_or_raise exn =
  !ref_monitor_errors := true;
  match !ref_errors with
  | Some (l, _) -> l := exn :: !l
  | None -> raise exn

let log_and_raise exn =
  log_or_raise exn;
  raise exn

let catch_errors caught f =
  let e = !ref_errors in
  ref_errors := Some (caught, RawTypeHash.create 3);
  Misc.try_finally f ~always:(fun () ->
      ref_errors := e)

let uncatch_errors f =
  let e = !ref_errors in
  ref_errors := None;
  Misc.try_finally f ~always:(fun () -> ref_errors := e)

let register_recoverable handler =
  ref_recoverable_handlers := handler :: !ref_recoverable_handlers

let is_recoverable exn =
  List.exists (fun handler -> handler exn) !ref_recoverable_handlers

let erroneous_type_check te =
  let te = Types.Transient_expr.coerce te in
  match !ref_errors with
  | Some (_, h) -> RawTypeHash.mem h te
  | _ -> false

let erroneous_type_register te =
  let te = Types.Transient_expr.coerce te in
  match !ref_errors with
  | Some (_, h) -> RawTypeHash.replace h te ()
  | None -> ()

module Error_set = Set.Make (struct
    type t = Location.error

    let compare_position (a: Lexing.position) (b: Lexing.position) =
      (* If the errors are not different, a positive number is
         returned, mainly to sort the set in order of appearance and
         to deduplicate certain errors.

         Do not take the [fname] in account. *)
      let ln = Int.compare a.pos_lnum b.pos_lnum
      and bol = Int.compare a.pos_bol b.pos_bol
      and cn = Int.compare a.pos_cnum b.pos_cnum
      in
      if Int.equal ln 0 && Int.equal bol 0 && Int.equal cn 0 then 0
      else 1


    let compare (a : t) (b : t) =
      (* If the errors are not different, a positive number is
         returned, mainly to sort the set in order of appearance and
         to deduplicate certain errors. *)
      let a_start = a.main.loc.loc_start
      and b_start = b.main.loc.loc_start in
      let a_fname = a_start.pos_fname
      and b_fname = b_start.pos_fname in
      if String.equal a_fname b_fname then
        if Int.equal (compare_position a_start b_start) 0
        then
          compare_position a.main.loc.loc_end b.main.loc.loc_end
        else 1
      else
        1
  end)
