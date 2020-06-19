(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
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

let get_registered_tests () =
  let f _test_name test acc = test::acc in
  let unsorted_tests = Hashtbl.fold f tests [] in
  List.sort compare unsorted_tests

let default_tests () =
  let f _test_name test acc =
    if test.test_run_by_default then test::acc else acc in
  Hashtbl.fold f tests []

let lookup name =
  try Some (Hashtbl.find tests name)
  with Not_found -> None

let test_of_action action =
{
  test_name = Actions.name action;
  test_run_by_default = false;
  test_actions = [action]
}

let run_actions log testenv actions =
  let total = List.length actions in
  let rec run_actions_aux action_number env = function
    | [] -> (Result.pass, env)
    | action::remaining_actions ->
      begin
        Printf.fprintf log "\nRunning action %d/%d (%s)\n%!"
          action_number total (Actions.name action);
        let (result, env') = Actions.run log env action in
        Printf.fprintf log "Action %d/%d (%s) %s\n%!"
          action_number total (Actions.name action)
          (Result.string_of_result result);
        if Result.is_pass result
        then run_actions_aux (action_number+1) env' remaining_actions
        else (result, env')
      end in
  run_actions_aux 1 testenv actions

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
