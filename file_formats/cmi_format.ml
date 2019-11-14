(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Unsafe_string

type error =
  | Not_an_interface of filepath * Magic_number.parse_error
  | Unexpected_interface of filepath * Magic_number.unexpected_error
  | Corrupted_interface of filepath

exception Error of error

(* these type abbreviations are not exported;
   they are used to provide consistency across
   input_value and output_value usage. *)
type signature = Types.signature_item list
type flags = pers_flags list
type header = modname * signature

type cmi_infos = {
    cmi_name : modname;
    cmi_sign : signature;
    cmi_crcs : crcs;
    cmi_flags : flags;
}

let input_cmi ic =
  let (name, sign) = (input_value ic : header) in
  let crcs = (input_value ic : crcs) in
  let flags = (input_value ic : flags) in
  {
      cmi_name = name;
      cmi_sign = sign;
      cmi_crcs = crcs;
      cmi_flags = flags;
    }

let read_cmi_magic_number filename ic =
  let open Magic_number in
  match read_current_info ~expected_kind:(Some Cmi) ic with
    | Error (Parse_error err) ->
       raise (Error (Not_an_interface (filename, err)))
    | Error (Unexpected_error err) ->
       raise (Error (Unexpected_interface (filename, err)))
    | Ok _ -> ()

let read_cmi filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      read_cmi_magic_number filename ic;
      try input_cmi ic
      with End_of_file | Failure _ ->
        raise (Error (Corrupted_interface filename))
    )

let output_cmi filename oc cmi =
(* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc ((cmi.cmi_name, cmi.cmi_sign) : header);
  flush oc;
  let crc = Digest.file filename in
  let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
  output_value oc (crcs : crcs);
  output_value oc (cmi.cmi_flags : flags);
  crc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface (filename, err) ->
      fprintf ppf "%a@ is not a compiled interface.@;"
        Location.print_filename filename;
      pp_print_text ppf Magic_number.(explain_parse_error (Some Cmi) err);
  | Unexpected_interface (filename, err) ->
      fprintf ppf
        "%a@ does not follow the expected format.@;"
        Location.print_filename filename;
      pp_print_text ppf (Magic_number.explain_unexpected_error err);
  | Corrupted_interface filename ->
      fprintf ppf "Corrupted compiled interface@ %a."
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
