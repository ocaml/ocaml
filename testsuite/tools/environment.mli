(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Test Environments. A test environment is an installation of OCaml (in a
    given prefix) compiled in a build path packaged with a process environment
    in which commands can be executed. *)

(** {1 Environments} *)

type t
(** Environments constructed by {!make}. *)

val make : (Format.formatter -> string -> unit) -> verbose:bool
  -> test_root:string -> test_root_logical:string option
  -> phase:Harness.Import.phase
  -> prefix:string -> bindir_suffix:string -> libdir_suffix:string -> t
(** Constructs an environment with the given formatter for displaying paths and
    verbosity setting. An environment consists of a physical [~test_root] (with
    an alternate logical path, if that differs) and a {!phase} for a compiler
    installed to [~prefix] with binaries in [~bindir_suffix] in that prefix and
    libraries in [~libdir_suffix] respectively.

    The result can be used with {!run_process} and {!run_process_with_test_env}
    and scrubs the process environment, ensuring that bindir is present in
    {v PATH v} and that the loader can use libdir for shared libraries (i.e.
    with [LD_LIBRARY_PATH] / [DYLD_LIBRARY_PATH] set or updated). *)

val is_renamed : t -> bool
(** [is_renamed t] if [~phase = Renamed] *)

val test_root : t -> string
(** Retrieves the [~test_root] passed to {!make}. *)

val test_root_logical : t -> string option
(** Retrieves the [~test_root_logical] passed to {!make}. *)

val prefix : t -> string
(** Retrieves the [~prefix] passed to {!make}. *)

val bindir : t -> string
(** [bindir t] is the full bindir for [t]. *)

val libdir : t -> string
(** [libdir t] is the full libdir for [t]. *)

val libdir_suffix : t -> string
(** Retrieves the [~libdir_suffix] passed to {!make}. *)

val tool_path : t -> Harness.Import.mode -> string -> string -> string
(** [tool_path t mode bytecode_tool native_tool] returns the full path to the
    executable [bytecode_tool] if [mode = Bytecode] or [native_tool] otherwise.
    On Windows, this is included the {v .exe v} extension. *)

val ocamlrun : t -> string
(** [ocamlrun t] is the full path to the OCaml interpreter in [t]. *)

val in_libdir : t -> string -> string
(** [in_libdir t path] returns [path] in the full libdir for [t].

    For example, [in_libdir t "stublibs"] gives the full path to the directory
    used for bytecode stub libraries. *)

val in_test_root : t -> string -> string
(** [in_test_root t path] returns [path] in the test root directory (which is
    usually {v testsuite/in_prefix v}). *)

val pp_path : t -> (Format.formatter -> string -> unit)
(** Retrieves the formatter passed to {!make}. *)

val verbose : t -> bool
(** Retrieves the [~verbose] parameter passed to {!make}. *)

(** {1 Executable classification} *)

val classify_executable : string -> Harness.Import.executable
(** [classify_executable file] analyses [file] and returns an {!executable}
    classification for it. *)

val launched_via_stub : string -> bool
(** [launched_via_stub file] is [true] only for bytecode executables which use
    the executable launcher. *)

(** {1 Process Execution} *)

val run_process :
  ?runtime:bool -> ?stubs:bool -> ?stdlib:bool
  -> ?prefix_path_with_cwd:bool -> ?quiet:bool -> ?fails:bool
  -> t -> string -> ?argv0:string -> string list -> int * string list
(** [run_process t program ?argv0 args] executes [program] in [t]. [program] is
    searched in the {v PATH v} of [t] if it is not absolute. [args] specifies
    {e additional} arguments to pass to [program]. {v argv[0] v} is [program]
    but can be changed by passing [~argv0].

    Commands are assumed to exit with code 0 unless [~fails:true]. If a command
    exits with code 0 and [~fails:true] or a command exits with a non-zero code
    and [~fails:false], then the harness aborts.

    Commands executed are displayed, unless [~quiet:true] (if a command causes
    the harness to abort, it is displayed regardless of [~quiet:true]).

    If [program] is not absolute, PATH-searching is carried out using the usual
    mechanism for that platform. On Unix, the current directory will be added to
    the start of {v PATH v} if [~prefix_path_with_cwd:true]. This is the default
    behaviour on Windows, and [Invalid_argument] is raised if
    [~prefix_path_with_cwd:false] is passed on Windows.

    [?runtime], [?stubs] and [?stdlib] control the shim mechanisms. These
    parameters are ignored for the [Original] phase of [t] (i.e. they are
    ignored if [not (is_renamed t)]). In the [Renamed] phase, each controls a
    shim which is expected to be necessary for the command to succeed.
    [run_process] first attempts the command with no shim mechanisms. Then, if
    more than one shim mechanism has been enabled, it attempts the command with
    each of them disabled in turn but the others enabled (i.e. if
    [~runtime:true] and [~stubs:true] then command is executed four times: once
    with no shim mechanisms, once with just the stubs mechanism, once with just
    the runtime mechanism and finally with both the runtime and stubs
    mechanism). The harness aborts if any execution other than the last
    succeeds - i.e. the shim mechanisms must {e all} be required.

    [~runtime:true] means that [program] is instead passed as the first argument
    to [ocamlrun t] (the [?argv0] parameter is ignored). This allows bytecode
    executables which only have a hard-coded location for {v ocamlrun v} to be
    executed in the [Renamed] phase.

    [~stubs:true] causes [in_libdir "stublibs"] to be added to
    {v CAML_LD_LIBRARY_PATH v}. This allows bytecode executables which needs
    C stubs to be loaded to load in the [Renamed] phase despite the lines in
    {v ld.conf v} pointing to the [Original] libdir.

    [~stdlib:true] causes {v OCAMLLIB v} to be set to [libdir t] and allows the
    compilers to be invoked in the [Renamed] phase. It also allows the runtime
    to locate {v ld.conf v} in the [Renamed] phase, but the lines in that file
    will still point to the [Original] libdir (i.e. ~stdlib:true technically
    helps bytecode executables with C stubs execute in the [Renamed] stage, but
    it's not enough).

    Certain error conditions are translated to exit codes:
    - If [Unix.create_process] fails with [ENOENT] for a {v #! v}-style bytecode
      image, this is translated to exit code 127
    - [SIGABRT] is converted to exit code 134
    - On s390x and riscv, [SIGSEGV] is converted to exit code 139 *)

val run_process_with_test_env :
  ?runtime:bool
  -> caml_ld_library_path:string list option -> ocamllib:string option
  -> camllib:string option -> ?quiet:bool -> ?fails:bool
  -> t -> string -> ?argv0:string -> string list -> int * string list
(** [run_process_with_test_env] behaves as {run_process}, but with some
    additional options for the process environment. Only the [?runtime] shim is
    available via this call.

    [~caml_ld_library_path], [~ocamllib], and [~camllib] control the
    {v CAML_LD_LIBRARY_PATH v}, {v OCAMLLIB v} and {v CAMLLIB v} environment
    variables respectively. When [None], the corresponding environment variable
    will be unset, when [~caml_ld_library_path:[]] or [Some ""], then the
    corrpesonding environment variable will be set to the empty string.
    Otherwise, the environment variable is set to the provided value (with the
    correct separator and escaping used for {v CAML_LD_LIBRARY_PATH v}). *)

val display_output : string list -> unit
(** [display_output lines] displays [lines] of output formatted as
    {!run_process} does on error. *)

val input_artefact_from_file: t -> string
  -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t *
     int
(*** [input_artefact_from_file env file] returns a bigarray containing the
    content of [file] and its length, intended for use with the relocation test.
    OCaml artefacts which use compressed marshalling are decompressed (but the
    format itself is not necessarily preserved). On ELF-based systems, {v .a v}
    and {v .o v} files are passed to {v readelf v} and if any {v COMPRESSED v}
    sections are detected, then the file is first passed through
    {v objcopy --decompress-debug-sections v}. *)
