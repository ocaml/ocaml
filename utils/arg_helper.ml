(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 OCamlPro SAS                                    *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

let fatal err =
  prerr_endline err;
  exit 2

module Make (S : sig
  module Key : sig
    type t
    val of_string : string -> t
    module Map : Map.S with type key = t
  end

  module Value : sig
    type t
    val of_string : string -> t
  end
end) = struct
  type parsed = {
    default : S.Value.t;
    override : S.Value.t S.Key.Map.t;
  }

  let default v = { default = v; override = S.Key.Map.empty }

  let no_equals value =
    match String.index value '=' with
    | exception Not_found -> true
    | _index -> false

  exception Parse_failure of exn

  let parse_exn str ~update =
    let values = Misc.Stdlib.String.split str ~on:',' in
    let parsed =
      List.fold_left (fun acc value ->
          match String.index value '=' with
          | exception Not_found ->
            begin match S.Value.of_string value with
            | value -> { acc with default = value }
            | exception exn -> raise (Parse_failure exn)
            end
          | equals ->
            let key_value_pair = value in
            let length = String.length key_value_pair in
            assert (equals >= 0 && equals < length);
            if equals = 0 then begin
              raise (Parse_failure (
                Failure "Missing key in argument specification"))
            end;
            let key =
              let key = String.sub key_value_pair 0 equals in
              try S.Key.of_string key
              with exn -> raise (Parse_failure exn)
            in
            let value =
              let value =
                String.sub key_value_pair (equals + 1) (length - equals - 1)
              in
              try S.Value.of_string value
              with exn -> raise (Parse_failure exn)
            in
            { acc with override = S.Key.Map.add key value acc.override })
        !update
        values
    in
    update := parsed

  let parse str ~help_text ~update =
    match parse_exn str ~update with
    | () -> ()
    | exception (Parse_failure exn) ->
      fatal (Printf.sprintf "%s: %s" (Printexc.to_string exn) help_text)

  type parse_result =
    | Ok
    | Parse_failed of exn

  let parse_no_error str ~update =
    match parse_exn str ~update with
    | () -> Ok
    | exception (Parse_failure exn) -> Parse_failed exn

  let get ~key parsed =
    match S.Key.Map.find key parsed.override with
    | provided -> provided
    | exception Not_found ->
      parsed.default
end
