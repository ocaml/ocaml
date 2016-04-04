(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
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

let sighandler _ =
  print_string "Got ctrl-C, exiting..."; print_newline();
  exit 0

let print_message delay c =
  while true do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle sighandler));
  ignore (Thread.create (print_message 0.6666666666) 'a');
  print_message 1.0 'b'
