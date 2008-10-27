(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial shell version
 * - Nicolas Pouillard: rewriting in OCaml
 *)



open Camlp4;
open Camlp4_config;
open Filename;
open Format;

value (interfaces, options, includes) =
  let rec self (interf, opts, incl) =
    fun
    [ [] -> (List.rev interf, List.rev opts, List.rev incl)
    | ["-I"; dir :: args] -> self (interf, opts, [dir; "-I" :: incl]) args
    | ["-version" :: _] ->
        do { printf "mkcamlp4, version %s@." version; exit 0 }
    | [ arg :: args ] when check_suffix arg ".cmi" ->
        let basename = String.capitalize (Filename.chop_suffix
                         (Filename.basename arg) ".cmi") in
        self ([ basename :: interf ], opts, incl) args
    | [ arg :: args ] ->
        self (interf, [ arg :: opts ], incl) args ]
  in self ([], [], ["."; "-I"]) (List.tl (Array.to_list Sys.argv));

value run l =
  let cmd = String.concat " " l in
  let () = Format.printf "%s@." cmd in
  let st =
    Sys.command cmd
    (* 0 *)
  in
  if st <> 0 then failwith ("Exit: " ^ string_of_int st) else ();

value crc_ml = Filename.temp_file "crc_" ".ml";
value crc = Filename.chop_suffix crc_ml ".ml";
value clean () = run ["rm"; "-f"; crc_ml; crc^".cmi"; crc^".cmo"];

try do {
  run ([ocaml_standard_library^"/extract_crc"; "-I"; camlp4_standard_library]
      @ includes @ interfaces @ [">"; crc_ml]);

  let cout = open_out_gen [Open_wronly; Open_append; Open_text] 0o666 crc_ml in do {
    output_string cout "let _ = Dynlink.add_available_units crc_unit_list\n";
    close_out cout
  };

  run (["ocamlc"; "-I"; camlp4_standard_library; "camlp4lib.cma"; crc_ml]
       @ includes @ options @ ["Camlp4Bin.cmo"; "-linkall"]);
  clean();
}
with exc -> do { clean (); raise exc };
