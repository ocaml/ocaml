(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let _dump_function_sizes flam ~backend =
  let module Backend = (val backend : Backend_intf.S) in
  let than = max_int in
  Flambda_iterators.iter_on_set_of_closures_of_program flam
    ~f:(fun ~constant:_ (set_of_closures : Flambda.set_of_closures) ->
      Variable.Map.iter (fun fun_var
            (function_decl : Flambda.function_declaration) ->
          let closure_id = Closure_id.wrap fun_var in
          let symbol = Backend.closure_symbol closure_id in
          match Inlining_cost.lambda_smaller' function_decl.body ~than with
          | Some size -> Format.eprintf "%a %d\n" Symbol.print symbol size
          | None -> assert false)
        set_of_closures.function_decls.funs)

let middle_end ppf ~source_provenance ~prefixname ~backend
    ~size
    ~filename
    ~module_ident
    ~module_initializer =
  let pass_number = ref 0 in
  let round_number = ref 0 in
  let check flam =
    if !Clflags.flambda_invariant_checks then begin
      try Flambda_invariants.check_exn flam
      with exn ->
        Misc.fatal_errorf "After Flambda pass %d, round %d:@.%s:@.%a"
          !pass_number !round_number (Printexc.to_string exn)
          Flambda.print_program flam
    end
  in
  let (+-+) flam (name, pass) =
    incr pass_number;
    if !Clflags.dump_flambda_verbose then begin
      Format.fprintf ppf "@.PASS: %s@." name;
      Format.fprintf ppf "Before pass %d, round %d:@ %a@." !pass_number
        !round_number Flambda.print_program flam;
      Format.eprintf "\n@?"
    end;
    let timing_pass = (Timings.Flambda_pass (name, source_provenance)) in
    let flam = Timings.accumulate_time timing_pass pass flam in
    if !Clflags.flambda_invariant_checks then begin
      Timings.accumulate_time (Flambda_pass ("check", source_provenance))
        check flam
    end;
    flam
  in
  Timings.accumulate_time
    (Flambda_pass ("middle_end", source_provenance)) (fun () ->
    let flam =
      let timing_pass =
        Timings.Flambda_pass ("closure_conversion", source_provenance)
      in
      Timings.accumulate_time timing_pass (fun () ->
          module_initializer
          |> Closure_conversion.lambda_to_flambda ~backend ~module_ident
                ~size ~filename)
        ()
    in
    if !Clflags.dump_rawflambda
    then
      Format.fprintf ppf "After closure conversion:@ %a@."
        Flambda.print_program flam;
    check flam;
    let fast_mode flam =
      pass_number := 0;
      let round = 0 in
      flam
      +-+ ("lift_lets 1", Lift_code.lift_lets)
      +-+ ("Lift_constants", Lift_constants.lift_constants ~backend)
      +-+ ("Share_constants", Share_constants.share_constants)
      +-+ ("Lift_let_to_initialize_symbol",
           Lift_let_to_initialize_symbol.lift ~backend)
      +-+ ("Inline_and_simplify",
           Inline_and_simplify.run ~never_inline:false ~backend
             ~prefixname ~round)
      +-+ ("Ref_to_variables",
           Ref_to_variables.eliminate_ref)
      +-+ ("Remove_unused_closure_vars 2",
           Remove_unused_closure_vars.remove_unused_closure_variables
             ~remove_direct_call_surrogates:false)
      +-+ ("Initialize_symbol_to_let_symbol",
           Initialize_symbol_to_let_symbol.run)
    in
    let rec loop flam =
      pass_number := 0;
      let round = !round_number in
      incr round_number;
      if !round_number > (Clflags.rounds ()) then flam
      else
        flam
        (* Beware: [Lift_constants] must be run before any pass that might
           duplicate strings. *)
        +-+ ("lift_lets 1", Lift_code.lift_lets)
        +-+ ("Lift_constants", Lift_constants.lift_constants ~backend)
        +-+ ("Share_constants", Share_constants.share_constants)
        +-+ ("Remove_unused_program_constructs",
             Remove_unused_program_constructs.remove_unused_program_constructs)
        +-+ ("Lift_let_to_initialize_symbol",
             Lift_let_to_initialize_symbol.lift ~backend)
        +-+ ("lift_lets 2", Lift_code.lift_lets)
        +-+ ("Remove_unused_closure_vars 1",
             Remove_unused_closure_vars.remove_unused_closure_variables
              ~remove_direct_call_surrogates:false)
        +-+ ("Inline_and_simplify",
             Inline_and_simplify.run ~never_inline:false ~backend
               ~prefixname ~round)
        +-+ ("Remove_unused_closure_vars 2",
             Remove_unused_closure_vars.remove_unused_closure_variables
              ~remove_direct_call_surrogates:false)
        +-+ ("lift_lets 3", Lift_code.lift_lets)
        +-+ ("Ref_to_variables",
             Ref_to_variables.eliminate_ref)
        +-+ ("Inline_and_simplify noinline",
             Inline_and_simplify.run ~never_inline:true ~backend
              ~prefixname ~round)
        +-+ ("Remove_unused_closure_vars 3",
             Remove_unused_closure_vars.remove_unused_closure_variables
              ~remove_direct_call_surrogates:false)
        +-+ ("Initialize_symbol_to_let_symbol",
             Initialize_symbol_to_let_symbol.run)
        |> loop
    in
    let back_end flam =
      flam
      +-+ ("Remove_unused_closure_vars",
           Remove_unused_closure_vars.remove_unused_closure_variables
             ~remove_direct_call_surrogates:true)
      +-+ ("Lift_constants", Lift_constants.lift_constants ~backend)
      +-+ ("Share_constants", Share_constants.share_constants)
      +-+ ("Remove_unused_program_constructs",
        Remove_unused_program_constructs.remove_unused_program_constructs)
    in
    let flam =
      if !Clflags.classic_inlining then
        fast_mode flam
      else
        loop flam
    in
    let flam = back_end flam in
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
              know what function was being applied)")
        | Unroll _ ->
          Location.prerr_warning (Debuginfo.to_location apply.dbg)
            (Warnings.Inlining_impossible "[@unroll] attribute was not \
              used on this function application (the optimizer did not \
              know what function was being applied)"));
    if !Clflags.dump_flambda
    then
      Format.fprintf ppf "End of middle end:@ %a@."
        Flambda.print_program flam;
    check flam;
    (* CR-someday mshinwell: add -d... option for this *)
    (* dump_function_sizes flam ~backend; *)
    flam) ();
