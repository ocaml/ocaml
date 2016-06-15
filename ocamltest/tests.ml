(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of tests, built from actions *)

type t = {
  test_name : string;
  test_run_by_default : bool;
  test_actions : Actions.t list
}

let compare t1 t2 = String.compare t1.test_name t2.test_name

let (tests: (string, t) Hashtbl.t) = Hashtbl.create 20

let register test = Hashtbl.add tests test.test_name test

let default_tests () =
  let f _test_namename test acc =
    if test.test_run_by_default then test::acc else acc in
  Hashtbl.fold f tests []

let lookup name =
  try Some (Hashtbl.find tests name)
  with Not_found -> None

let rec run_actions log env = function
  | [] -> Actions.Pass env
  | action::remaining_actions ->
    begin
      let result = Actions.run log env action in
      match result with
        | Actions.Pass env' -> run_actions log env' remaining_actions
        | _ ->
          Printf.fprintf log "Action %s returned %s\n%!"
            action.Actions.action_name (Actions.string_of_result result);
          result
    end

let run log env test =
  Printf.fprintf log "Running test %s with %d actions\n%!"
    test.test_name
    (List.length test.test_actions);
  run_actions log env test.test_actions

module TestSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)
