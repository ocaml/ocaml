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
     val _use_runtime : string -> unit
     val _v : unit -> unit
     val _verbose : unit -> unit
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
     "-intf_suffix", Arg.String F._intf_suffix,
            "<file>  Suffix for interface file (default: .mli)";
     "-linkall", Arg.Unit F._linkall, " Link all modules, even unused ones";
     "-make_runtime", Arg.Unit F._make_runtime,
            " Build a runtime system with given C objects and libraries";
     "-noassert", Arg.Unit F._noassert, " Do not compile assertion checks";
     "-o", Arg.String F._o, "<file>  Set output file name to <file>";
     "-output-obj", Arg.Unit F._output_obj,
                            "Output a C object file instead of an executable";
     "-pp", Arg.String F._pp,
                     "<command>  Pipe sources through preprocessor <command>";
     "-thread", Arg.Unit F._thread, " Use thread-safe standard library";
     "-unsafe", Arg.Unit F._unsafe,
                             " No bounds checking on array and string access";
     "-use_runtime", Arg.String F._use_runtime,
                   "<path> Generate bytecode for the given runtime system";
     "-v", Arg.Unit F._v, " Print compiler version number";
     "-verbose", Arg.Unit F._verbose, " Print calls to external commands";

     "-nopervasives", Arg.Unit F._nopervasives, " (undocumented)";
     "-drawlambda", Arg.Unit F._drawlambda, " (undocumented)";
     "-dlambda", Arg.Unit F._dlambda, " (undocumented)";
     "-dinstr", Arg.Unit F._dinstr, " (undocumented)";

     "-", Arg.String F.anonymous,
           "<file>  Treat <file> as a file name (even if it starts with `-')";
  ]
end;;
