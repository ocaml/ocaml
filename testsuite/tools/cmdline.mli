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

(** Installation configuration. Includes the function for parsing the harness's
    command line. *)

val parse:
  string array
    -> (config:Harness.Import.config * pwd:string * prefix:string *
        bindir:string * bindir_suffix:string *
        libdir:string * libdir_suffix:string *
        summarise_only:bool * verbose:bool, int * string) Result.t
(** [parse argv] parses [argv] and either returns a tuple of properties derived
    from it, or a message to be displayed with an exit code.

    Only fields which can be determined from the commandline are set in
    [~config] - [launcher_searches_for_ocamlrun],
    [target_launcher_searches_for_ocamlrun] and [bytecode_shebangs_by_default]
    are always [false]; all libraries passed on the command line will be
    returned as singleton lists (i.e. no dependencies are computed)

    [~pwd] is the {v --pwd v} flag, used to determine the logical working
    directory. [~prefix], [~bindir], [~libdir], [~bindir_suffix] and
    [~libdir_suffix] are derived from the {v --bindir v} and {v --libdir v}
    flags.

    [~summarise_only] and [~verbose] are the {v --summary v} and {v --verbose v}
    flags. *)
