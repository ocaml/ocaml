(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
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
     val _dllib : string -> unit
     val _dllpath : string -> unit
     val _dtypes : unit -> unit
     val _g : unit -> unit
     val _i : unit -> unit
     val _I : string -> unit
     val _impl : string -> unit
     val _intf : string -> unit
     val _intf_suffix : string -> unit
     val _labels : unit -> unit
     val _linkall : unit -> unit
     val _make_runtime : unit -> unit
     val _noassert : unit -> unit
     val _noautolink : unit -> unit
     val _nolabels : unit -> unit
     val _nostdlib : unit -> unit
     val _o : string -> unit
     val _output_obj : unit -> unit
     val _pack : unit -> unit
     val _pp : string -> unit
     val _principal : unit -> unit
     val _rectypes : unit -> unit
     val _thread : unit -> unit
     val _vmthread : unit -> unit
     val _unsafe : unit -> unit
     val _use_prims : string -> unit
     val _use_runtime : string -> unit
     val _v : unit -> unit
     val _version : unit -> unit
     val _verbose : unit -> unit
     val _w : string -> unit
     val _warn_error : string -> unit
     val _where : unit -> unit

     val _nopervasives : unit -> unit
     val _dparsetree : unit -> unit
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
           "<command>  Use <command> as the C compiler and linker";
    "-cclib", Arg.String F._cclib, "<opt>  Pass option <opt> to the C linker";
    "-ccopt", Arg.String F._ccopt,
           "<opt>  Pass option <opt> to the C compiler and linker";
    "-custom", Arg.Unit F._custom, " Link in custom mode";
    "-dllib", Arg.String F._dllib,
           "<lib>  Use the dynamically-loaded library <lib>";
    "-dllpath", Arg.String F._dllpath,
           "<dir>  Add <dir> to the run-time search path for shared libraries";
    "-dtypes", Arg.Unit F._dtypes, " Save type information in <filename>.annot";
    "-g", Arg.Unit F._g, " Save debugging information";
    "-i", Arg.Unit F._i, " Print inferred interface";
    "-I", Arg.String F._I,
           "<dir>  Add <dir> to the list of include directories";
    "-impl", Arg.String F._impl, "<file>  Compile <file> as a .ml file";
    "-intf", Arg.String F._intf, "<file>  Compile <file> as a .mli file";
    "-intf-suffix", Arg.String F._intf_suffix,
           "<string>  Suffix for interface files (default: .mli)";
    "-intf_suffix", Arg.String F._intf_suffix,
           "<string>  (deprecated) same as -intf-suffix";
    "-labels", Arg.Unit F._labels, " Use commuting label mode";
    "-linkall", Arg.Unit F._linkall, " Link all modules, even unused ones";
    "-make-runtime", Arg.Unit F._make_runtime,
           " Build a runtime system with given C objects and libraries";
    "-make_runtime", Arg.Unit F._make_runtime,
           " (deprecated) same as -make-runtime";
    "-modern", Arg.Unit F._labels, " (deprecated) same as -labels";
    "-noassert", Arg.Unit F._noassert, " Don't compile assertion checks";
    "-noautolink", Arg.Unit F._noautolink,
           " Don't automatically link C libraries specified in .cma files";
    "-nolabels", Arg.Unit F._nolabels, " Ignore non-optional labels in types";
    "-nostdlib", Arg.Unit F._nostdlib,
           " do not add default directory to the list of include directories";
    "-o", Arg.String F._o, "<file>  Set output file name to <file>";
    "-output-obj", Arg.Unit F._output_obj,
           " Output a C object file instead of an executable";
    "-pack", Arg.Unit F._pack,
           " Package the given .cmo files into one .cmo";
    "-pp", Arg.String F._pp,
           "<command>  Pipe sources through preprocessor <command>";
    "-principal", Arg.Unit F._principal,
           " Check principality of type inference";
    "-rectypes", Arg.Unit F._rectypes, " Allow arbitrary recursive types";
    "-thread", Arg.Unit F._thread, " Generate code that supports the system threads library";
    "-unsafe", Arg.Unit F._unsafe,
           " No bounds checking on array and string access";
    "-use-runtime", Arg.String F._use_runtime,
           "<file>  Generate bytecode for the given runtime system";
    "-use_runtime", Arg.String F._use_runtime,
           "<file>  (deprecated) same as -use-runtime";
    "-v", Arg.Unit F._v,
           " Print compiler version and location of standard library and exit";
    "-version", Arg.Unit F._version, " Print compiler version and exit";
    "-verbose", Arg.Unit F._verbose, " Print calls to external commands";
    "-vmthread", Arg.Unit F._vmthread, " Generate code that supports the threads library with VM-level scheduling";
    "-w", Arg.String F._w,
      "<flags>  Enable or disable warnings according to <flags>:\n\
      \032    A/a enable/disable all warnings\n\
      \032    C/c enable/disable suspicious comment\n\
      \032    D/d enable/disable deprecated features\n\
      \032    E/e enable/disable fragile match\n\
      \032    F/f enable/disable partially applied function\n\
      \032    L/l enable/disable labels omitted in application\n\
      \032    M/m enable/disable overriden method\n\
      \032    P/p enable/disable partial match\n\
      \032    S/s enable/disable non-unit statement\n\
      \032    U/u enable/disable unused match case\n\
      \032    V/v enable/disable hidden instance variable\n\
      \032    X/x enable/disable all other warnings\n\
      \032    default setting is \"Ale\"\n\
      \032    (all warnings but labels and fragile match enabled)";
    "-warn-error" , Arg.String F._warn_error,
      "<flags>  Treat the warnings enabled by <flags> as errors.\n\
      \032    See option -w for the list of flags.\n\
      \032    Default setting is \"a\" (warnings are not errors)";
    "-where", Arg.Unit F._where,
           " Print location of standard library and exit";
    "-nopervasives", Arg.Unit F._nopervasives, " (undocumented)";
    "-dparsetree", Arg.Unit F._dparsetree, " (undocumented)";
    "-drawlambda", Arg.Unit F._drawlambda, " (undocumented)";
    "-dlambda", Arg.Unit F._dlambda, " (undocumented)";
    "-dinstr", Arg.Unit F._dinstr, " (undocumented)";
    "-use-prims", Arg.String F._use_prims, "<file>  (undocumented)";

    "-", Arg.String F.anonymous,
           "<file>  Treat <file> as a file name (even if it starts with `-')";
  ]
end;;
