(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Local
  | Parameter of { index : int; }

let local = Local

let parameter ~index =
  if index < 0 then begin
    Misc.fatal_errorf "Bad parameter index %d" index
  end;
  Parameter { index; }

let join t1 t2 =
  match t1, t2 with
  | Local, Local -> Local
  | Parameter { index; }, Local
  | Local, Parameter { index; } -> Parameter { index; }
  | Parameter { index = index1; }, Parameter { index = index2; } ->
    if index1 <> index2 then begin
      Misc.fatal_error "Cannot join [Is_parameter.t] values that disagree \
        on parameter indexes"
    end else begin
      Parameter { index = index1; }
    end

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Local, Local -> 0
    | Parameter { index = index1; }, Parameter { index = index2; } ->
      Stdlib.compare index1 index2
    | Local, Parameter _ -> -1
    | Parameter _, Local -> 1

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t = Hashtbl.hash t

  let print ppf t =
    match t with
    | Local -> Format.pp_print_string ppf "local"
    | Parameter { index; } ->
      Format.fprintf ppf "@[(parameter@ (index %d))@]" index

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)
