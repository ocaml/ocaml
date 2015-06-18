(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let middle_end ppf ~sourcefile ~prefixname ~backend ~exported_fields lam =
  Timings.(start (Flambda_middle_end sourcefile));
  let dump_and_check s flam =
    if !Clflags.dump_flambda
    then Format.fprintf ppf "%s:@ %a@." s Printflambda.flambda flam;
    try Flambdacheck.check flam
    with e ->
      Format.fprintf ppf "%a@."
        Printflambda.flambda flam;
      raise e
  in
  let flam =
    (* CR mshinwell for pchambart: I'm not sure this first sentence is
       accurate.  I suppose we mean "the only constants" or something? *)
    (* Strings are the only expressions that can't be duplicated without
       changing the semantics.  So we lift them to the toplevel to avoid
       having to handle special cases later.
       There is no runtime cost to this transformation: strings are
       constants and will not appear in closures. *)
    lam
    |> Lift_strings.lift_strings_to_toplevel
    |> Closure_conversion.lambda_to_flambda ~backend ~exported_fields
    |> Lift_code.lift_apply_construction_to_variables
    |> Lift_code.lift_block_construction_to_variables
  in
  dump_and_check "flambdagen" flam;
  let rec loop rounds flam =
    if rounds <= 0 then flam
    else
      flam
      |> Lift_code.lift_lets
      |> Remove_unused_closure_vars.remove_unused_closure_variables
      |> Inlining.inline ~never_inline:false ~backend
      |> Lift_code.lift_lets
      |> Remove_unused_closure_vars.remove_unused_closure_variables
      |> Remove_unused_arguments.separate_unused_arguments_in_closures
      |> Lift_code.lift_set_of_closures
      |> Remove_unused_globals.remove_unused_globals
      |> Inlining.inline ~never_inline:true ~backend
      |> Remove_unused_closure_vars.remove_unused_closure_variables
      |> Ref_to_variables.eliminate_ref
      |> loop (rounds - 1)
  in
  let flam = loop !Clflags.simplify_rounds flam in
  dump_and_check "flambdasimplify" flam;
  Inlining_stats.save_then_forget_decisions ~output_prefix:prefixname;
  Timings.(stop (Flambda_middle_end sourcefile));
  flam
