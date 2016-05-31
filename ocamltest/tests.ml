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

let (tests: (string, t) Hashtbl.t) = Hashtbl.create 20

let register name run_by_default actions =
  let test =
    { test_name = name;
      test_run_by_default = run_by_default;
      test_actions = actions
    } in
  Hashtbl.add tests name test

let default_tests () =
  let f name test acc = if test.test_run_by_default then test::acc else acc in
  Hashtbl.fold f tests []

let lookup name =
  try Some (Hashtbl.find tests name)
  with Not_found -> None

let rec run_actions ppf env = function
  | [] -> Actions.Pass env
  | action::remaining_actions ->
    begin
      let result = Actions.run ppf env action in
      match result with
        | Actions.Pass env' -> run_actions ppf env' remaining_actions
        | _ -> result
    end

let run ppf env test =
  Format.fprintf ppf "Running test %s with %d actions\n%!"
    test.test_name
    (List.length test.test_actions);
  run_actions ppf env test.test_actions
