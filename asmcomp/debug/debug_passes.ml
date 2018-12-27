(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module ARV = Available_ranges_all_vars
module L = Linearize
module LB = Lexical_block_ranges
module SLDL = Simple_location_description_lang
module V = Backend_var

type result =
  { fundecl : L.fundecl;
    available_ranges_vars : Available_ranges_all_vars.t;
    lexical_block_ranges : Lexical_block_ranges.t;
    external_calls_generated_during_emit
      : Emitaux.external_call_generated_during_emit list;
  }

let passes_for_fundecl (fundecl : L.fundecl) =
  let available_ranges_vars, fundecl =
    Profile.record "debug_available_ranges_vars" (fun fundecl ->
        Available_ranges_vars.create fundecl)
      ~accumulate:true
      fundecl
  in
  let available_ranges_phantom_vars, fundecl =
    Profile.record "debug_available_ranges_phantom_vars" (fun fundecl ->
        Available_ranges_phantom_vars.create fundecl)
      ~accumulate:true
      fundecl
  in
  let lexical_block_ranges, fundecl =
    Profile.record "debug_lexical_block_ranges" (fun fundecl ->
        Lexical_block_ranges.create fundecl)
      ~accumulate:true
      fundecl
  in
  let available_ranges_vars, available_ranges_phantom_vars,
        lexical_block_ranges, fundecl =
    let label_env, fundecl =
      Profile.record "debug_coalesce_labels" (fun () ->
          Coalesce_labels.fundecl fundecl)
        ~accumulate:true
        ()
    in
    let available_ranges_vars =
      Profile.record "debug_rewrite_labels_vars" (fun () ->
          Available_ranges_vars.rewrite_labels available_ranges_vars
            ~env:label_env)
        ~accumulate:true
        ()
    in
    let available_ranges_phantom_vars =
      Profile.record "debug_rewrite_labels_phantom_vars" (fun () ->
          Available_ranges_phantom_vars.rewrite_labels
            available_ranges_phantom_vars
            ~env:label_env)
        ~accumulate:true
        ()
    in
    let lexical_block_ranges =
      Profile.record "debug_rewrite_labels_lexical_blocks" (fun () ->
          Lexical_block_ranges.rewrite_labels lexical_block_ranges
            ~env:label_env)
        ~accumulate:true
        ()
    in
    available_ranges_vars, available_ranges_phantom_vars,
      lexical_block_ranges, fundecl
  in
  let available_ranges_vars =
    Available_ranges_all_vars.create ~available_ranges_vars
      ~available_ranges_phantom_vars
  in
  available_ranges_vars, lexical_block_ranges, fundecl

let passes_for_fundecl_and_emit ~emit ~end_of_function_label
      (fundecl : L.fundecl) =
  let available_ranges_vars, lexical_block_ranges, fundecl =
    passes_for_fundecl fundecl
  in
  let external_calls_generated_during_emit =
    emit fundecl ~end_of_function_label
  in
  { fundecl;
    available_ranges_vars;
    lexical_block_ranges;
    external_calls_generated_during_emit;
  }
