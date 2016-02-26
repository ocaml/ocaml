(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let _ =
  let args = Ccomp.quote_files (List.tl (Array.to_list Sys.argv)) in
  exit(Sys.command("ocamlc -I +compiler-libs -linkall ocamlcommon.cma \
                    ocamlbytecomp.cma ocamltoplevel.cma "
                   ^ args ^ " topstart.cmo"))
