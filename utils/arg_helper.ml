(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
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

  let parse str ~help_text ~update =
    let values = Misc.Stdlib.String.split str ~on:',' in
    let parsed =
      List.fold_left (fun acc value ->
          match String.index value '=' with
          | exception Not_found ->
            begin match S.Value.of_string value with
            | value -> { acc with default = value }
            | exception exn ->
              fatal (Printf.sprintf "%s: %s" (Printexc.to_string exn) help_text)
            end
          | equals ->
            let key_value_pair = value in
            let length = String.length key_value_pair in
            if equals <= 0 || equals >= length - 1 then begin
              fatal help_text
            end;
            let key =
              let key = String.sub key_value_pair 0 equals in
              try S.Key.of_string key
              with exn ->
                fatal (Printf.sprintf "%s: %s"
                         (Printexc.to_string exn) help_text)
            in
            let value =
              let value =
                String.sub key_value_pair (equals + 1) (length - equals - 1)
              in
              try S.Value.of_string value
              with exn ->
                fatal (Printf.sprintf "%s: %s"
                         (Printexc.to_string exn) help_text)
            in
            { acc with override = S.Key.Map.add key value acc.override })
        !update
        values
    in
    update := parsed

  let get ~key parsed =
    match S.Key.Map.find key parsed.override with
    | provided -> provided
    | exception Not_found ->
      parsed.default
end
