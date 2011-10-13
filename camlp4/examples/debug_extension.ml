(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(*
 * No debugging code at all:
 *   $ camlp4o -parser Camlp4DebugParser debug_extension.ml
 *   true
 * Debugging code for lexing:
 *   $ STATIC_CAMLP4_DEBUG='lexing' camlp4o -parser Camlp4DebugParser debug_extension.ml
 *  let () =
 *    if Camlp4.Debug.mode "lexing"
 *    then Debug.printf "lexing" "TOKEN: Int %d" (2 * 21)
 *    else ()
 *  in true
 *
 * Debugging code for lexing and parsing:
 *   $ STATIC_CAMLP4_DEBUG='lexing:parsing' camlp4o -parser Camlp4DebugParser debug_extension.ml
 *   let () =
 *     if Camlp4.Debug.mode "lexing"
 *     then Debug.printf "lexing" "TOKEN: Int %d" (2 * 21)
 *     else () in
 *   let () =
 *     if Camlp4.Debug.mode "parsing"
 *     then Debug.printf "parsing" "RULE: ..."
 *     else ()
 *   in true
 *
 * Debugging code for any section:
 *  $ STATIC_CAMLP4_DEBUG='*' camlp4o -parser Camlp4DebugParser debug_extension.ml
 *  ... same output as above ...
 *
 * When you program is compiled you can use the CAMLP4_DEBUG variable to
 * activate some debugging sections.
 *
 * CAMLP4_DEBUG_FILE manage where messages goes (default is stderr).
 *)

camlp4_debug lexing  "TOKEN: Int %d" (2 * 21) in
camlp4_debug parsing "RULE: ..." in
true
