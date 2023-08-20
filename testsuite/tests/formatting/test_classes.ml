(* TEST
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   flags = "-dsource -stop-after parsing";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_classes.ml.reference";
   check-ocamlc.byte-output;
 }{
   setup-ocamlc.byte-build-env;
   flags = "-dsource -stop-after parsing";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_classes.ml.reference";
   check-ocamlc.byte-output;
 }
*)
class virtual ['inh,'extra,'syn] logic_t =
  object
    inherit
      [(GT.string OCanren.logic, logic Std.List.logic) t,
      (GT.string OCanren.logic, logic Std.List.logic) t,(GT.string
                                                        OCanren.logic,
                                                        logic
                                                        Std.List.logic)
                                                        t,
      'inh,'extra,'syn] OCanren.logic_t
  end
