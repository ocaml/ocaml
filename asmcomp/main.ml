let main() =
  Arg.parse
    ["-dcmm", Arg.Unit(fun () -> Codegen.dump_cmm := true);
     "-dsel", Arg.Unit(fun () -> Codegen.dump_selection := true);
     "-dlive", Arg.Unit(fun () -> Codegen.dump_live := true;
                                  Printmach.print_live := true);
     "-dspill", Arg.Unit(fun () -> Codegen.dump_spill := true);
     "-dsplit", Arg.Unit(fun () -> Codegen.dump_split := true);
     "-dinterf", Arg.Unit(fun () -> Codegen.dump_interf := true);
     "-dprefer", Arg.Unit(fun () -> Codegen.dump_prefer := true);
     "-dalloc", Arg.Unit(fun () -> Codegen.dump_regalloc := true);
     "-dreload", Arg.Unit(fun () -> Codegen.dump_reload := true);
     "-dlinear", Arg.Unit(fun () -> Codegen.dump_linear := true)]
    Codegen.file

let _ = Printexc.catch main (); exit 0

