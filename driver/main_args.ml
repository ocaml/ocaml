(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module Make_options (F :
   sig
     val _a : unit -> unit
     val _c : unit -> unit
     val _cc : string -> unit
     val _cclib : string -> unit
     val _ccopt : string -> unit
     val _custom : unit -> unit
     val _g : unit -> unit
     val _i : unit -> unit
     val _I : string -> unit
     val _impl : string -> unit
     val _intf : string -> unit
     val _intf_suffix : string -> unit
     val _linkall : unit -> unit
     val _make_runtime : unit -> unit
     val _noassert : unit -> unit
     val _o : string -> unit
     val _output_obj : unit -> unit
     val _pp : string -> unit
     val _thread : unit -> unit
     val _unsafe : unit -> unit
     val _use_prims : string -> unit
     val _use_runtime : string -> unit
     val _v : unit -> unit
     val _verbose : unit -> unit
     val _w : string -> unit
     val _nopervasives : unit -> unit
     val _drawlambda : unit -> unit
     val _dlambda : unit -> unit
     val _dinstr : unit -> unit
     val anonymous : string -> unit
   end) =
struct
  let list = [
    "-a", Arg.Unit F._a, " Build a library";
    "-c", Arg.Unit F._c, " Compile only (do not link)";
    "-cc", Arg.String F._cc,
           "<comp>  Use <comp> as the C compiler and linker";
    "-cclib", Arg.String F._cclib, "<opt>  Pass option <opt> to the C linker";
    "-ccopt", Arg.String F._ccopt,
                      "<opt>  Pass option <opt> to the C compiler and linker";
     "-custom", Arg.Unit F._custom, " Link in custom mode";
     "-g", Arg.Unit F._g, " Save debugging information";
     "-i", Arg.Unit F._i, " Print the types";
     "-I", Arg.String F._I,
                        "<dir>  Add <dir> to the list of include directories";
     "-impl", Arg.String F._impl, "<file>  Compile <file> as a .ml file";
     "-intf", Arg.String F._intf, "<file>  Compile <file> as a .mli file";
     "-intf-suffix", Arg.String F._intf_suffix,
            "<file>  Suffix for interface file (default: .mli)";
     "-intf_suffix", Arg.String F._intf_suffix, "<file>  same as -intf-suffix";
     "-linkall", Arg.Unit F._linkall, " Link all modules, even unused ones";
     "-make-runtime", Arg.Unit F._make_runtime,
            " Build a runtime system with given C objects and libraries";
     "-make_runtime", Arg.Unit F._make_runtime, " same as -make-runtime";
     "-noassert", Arg.Unit F._noassert, " Do not compile assertion checks";
     "-o", Arg.String F._o, "<file>  Set output file name to <file>";
     "-output-obj", Arg.Unit F._output_obj,
                            "Output a C object file instead of an executable";
     "-pp", Arg.String F._pp,
                     "<command>  Pipe sources through preprocessor <command>";
     "-thread", Arg.Unit F._thread, " Use thread-safe standard library";
     "-unsafe", Arg.Unit F._unsafe,
                             " No bounds checking on array and string access";
     "-use-runtime", Arg.String F._use_runtime,
                   "<path>  Generate bytecode for the given runtime system";
     "-use_runtime", Arg.String F._use_runtime, "<path>  same as -use-runtime";
     "-v", Arg.Unit F._v, " Print compiler version number";
     "-verbose", Arg.Unit F._verbose, " Print calls to external commands";
     "-w", Arg.String F._w,
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    F/f enable/disable partially applied function\n\
       \032    M/m enable/disable overriden methods\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variables\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is A (all warnings enabled)";

     "-nopervasives", Arg.Unit F._nopervasives, " (undocumented)";
     "-drawlambda", Arg.Unit F._drawlambda, " (undocumented)";
     "-dlambda", Arg.Unit F._dlambda, " (undocumented)";
     "-dinstr", Arg.Unit F._dinstr, " (undocumented)";
     "-use-prims", Arg.String F._use_prims, "<file>  (undocumented)";

     "-", Arg.String F.anonymous,
           "<file>  Treat <file> as a file name (even if it starts with `-')";
  ]
end;;
