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

let verbose = try ignore (Sys.getenv "FLAMBDA_VERBOSE"); true with _ -> false

let middle_end ppf ~sourcefile ~prefixname ~backend
    ~size
    ~module_ident
    ~module_initializer =
  let pass_number = ref 0 in
  let round_number = ref 0 in
  let check flam =
    try Flambda_invariants.check_exn flam
    with exn ->
      Misc.fatal_errorf "After Flambda pass %d, round %d:@.%s:@.%a"
        !pass_number !round_number (Printexc.to_string exn)
        Flambda.print_program flam
  in
  let dump_and_check s flam =
    if !Clflags.dump_flambda
    then Format.fprintf ppf "%s:@ %a@." s Flambda.print_program flam;
    check flam
  in
  let (+-+) flam (name, pass) =
    incr pass_number;
    if verbose then begin
      Format.fprintf ppf "@.PASS: %s@." name;
      if !Clflags.full_flambda_invariant_check then begin
        Format.fprintf ppf "Before pass %d, round %d:@ %a@." !pass_number
          !round_number Flambda.print_program flam;
        Format.eprintf "\n@?"
      end;
    end;
    let timing_pass = (Timings.Flambda_pass (name, sourcefile)) in
    Timings.restart timing_pass;
    let flam = pass flam in
    Timings.accumulate timing_pass;
    if !Clflags.full_flambda_invariant_check then
      Timings.accumulate_time (Flambda_pass ("check", sourcefile)) check flam;
    flam
  in
  Timings.(start (Flambda_middle_end sourcefile));
  let flam =
    module_initializer
    |> Eliminate_const_block.run
    |> Closure_conversion.lambda_to_flambda ~backend ~module_ident ~size
  in
  dump_and_check "After closure conversion" flam;
  let rec loop flam =
    pass_number := 0;
    incr round_number;
    if !round_number > !Clflags.simplify_rounds then flam
    else
      flam
      (* Beware: [Lift_constants] must be run before any pass that might
         duplicate strings. *)
      +-+ ("lift_lets 1", Lift_code.lift_lets)
      +-+ ("Lift_constants", Lift_constants.lift_constants ~backend)
      +-+ ("Share_constants", Share_constants.share_constants)
      +-+ ("Lift_let_to_initialize_symbol",
           Lift_let_to_initialize_symbol.lift ~backend)
      +-+ ("lift_lets 2", Lift_code.lift_lets)
      +-+ ("Remove_unused_closure_vars 1",
           Remove_unused_closure_vars.remove_unused_closure_variables)
      +-+ ("Inline_and_simplify",
           Inline_and_simplify.run ~never_inline:false ~backend
             ~prefixname)
      +-+ ("lift_lets 3", Lift_code.lift_lets)
      +-+ ("Remove_unused_closure_vars 2",
           Remove_unused_closure_vars.remove_unused_closure_variables)
      (* +- "Remove_unused_arguments" *)
(*
      ++ Remove_unused_arguments.separate_unused_arguments_in_closures
        ?force:None
*)
      +-+ ("Inline_and_simplify noinline 1",
           Inline_and_simplify.run ~never_inline:true ~backend
             ~prefixname)
      +-+ ("Remove_unused_closure_vars",
           Remove_unused_closure_vars.remove_unused_closure_variables)
      +-+ ("Ref_to_variables",
           Ref_to_variables.eliminate_ref)
      +-+ ("Inline_and_simplify noinline 2",
           Inline_and_simplify.run ~never_inline:true ~backend
            ~prefixname)
      +-+ ("Initialize_symbol_to_let_symbol",
           Initialize_symbol_to_let_symbol.run)
      +-+ ("Remove_unused_globals",
           Remove_unused_globals.remove_unused_globals)
      |> loop
  in
  (* Check that there aren't any unused "always inline" attributes. *)
  Flambda_iterators.iter_apply_on_program flam ~f:(fun apply ->
      match apply.inline with
      | Default_inline | Never_inline -> ()
      | Always_inline ->
        (* CR-someday mshinwell: consider a different error message if
           this triggers as a result of the propagation of a user's
           attribute into the second part of an over application
           (inline_and_simplify.ml line 710). *)
        Location.prerr_warning (Debuginfo.to_location apply.dbg)
          (Warnings.Inlining_impossible "[@inlined] attribute was not \
            used on this function application (the optimizer did not \
            know what function was being applied)"));
  let flam = loop flam in
  dump_and_check "End of middle end" flam;
  Timings.(stop (Flambda_middle_end sourcefile));
  flam
