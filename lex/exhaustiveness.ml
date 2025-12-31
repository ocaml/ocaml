(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Martin Jambon                              *)
(*                                                                        *)
(*   Copyright 2025 Martin Jambon                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(*
   Check whether a rule may fail to match on some input
*)

open Printf

(************************)
(* Debugging *)
(************************)

(* Change to true to print automaton details for debugging purposes *)
let debug = false

(* Print what we care about for debugging purposes *)
let print_state state_id (state : Lexgen.automata) =
  printf "state %i\n" state_id;
  match state with
  | Perform (_action_id, _tag_actions) ->
      printf "  final\n"
  | Shift (trans, transitions) ->
      printf "  %i transitions\n" (Array.length transitions);
      (match trans with
       | No_remember ->
           printf "    no remember\n"
       | Remember (n, _tag_action) ->
           printf "    remember %i\n" n);
      Array.iteri (fun symbol ((move : Lexgen.automata_move), _mem_actions) ->
        match move with
        | Backtrack -> ()
        | Goto dst_state ->
            printf "    symbol %i: goto %i\n"
              symbol dst_state
      ) transitions

(************************)
(* Sets and maps *)
(************************)

(* For keeping track of visited nodes *)
module DFA_states = Set.Make (struct
    type t = int (* state ID *)
    let compare = Int.compare
end)

(* For mapping at most one state to a matching path *)
module DFA_state_map = Map.Make (struct
    type t = int (* state ID *)
    let compare = Int.compare
end)

(************************)
(* Automaton navigation *)
(************************)

(* Return the state's transitions if any *)
let get_transitions (state : Lexgen.automata) =
  match state with
  | Perform _ -> None
  | Shift (_, transitions) -> Some transitions

(* Indicate if a state is final.
   [is this correct?] *)
let is_final (state : Lexgen.automata) =
  match state with
  | Perform _ -> true
  | Shift (Remember _, _) -> true
  | Shift (No_remember, _) -> false

(*
   We assume the following encoding for input symbols triggering transitions:
   0-255: bytes
   256: end of input
*)
let is_end_of_input = function
  | 256 -> true
  | _ -> false

(****************************************************************)
(* Main algorithm for exhaustiveness checking *)
(****************************************************************)

(* Reconstruct a string from a stack of chars *)
let string_of_path (path : char list) =
  path
  |> List.rev
  |> List.to_seq
  |> String.of_seq

(* Local exception *)
exception Missing_transition of int (* 0-256 *)

(* This is Array.iteri but we start from the symbol for end-of-input (256,
   last index in the array).
   This allows us to find a shorter nonmatching string if a transition
   is missing for both end-of-input and some character. *)
let iter_symbols_in_preferred_order func ar =
  let last = Array.length ar - 1 in
  func last ar.(last);
  for i = 0 to last - 1 do
    func i ar.(i)
  done

let find_missing_transition (state : Lexgen.automata) =
  match get_transitions state with
  | None ->
      (* this is a final state without transitions *)
      None
  | Some transitions ->
      (* must have a transition for each byte and for eof *)
      try
        iter_symbols_in_preferred_order
          (fun trans ((dst : Lexgen.automata_move), _) ->
             match dst with
             | Backtrack ->
                 raise (Missing_transition trans)
             | Goto _state_id ->
                 ()
          ) transitions;
        None
      with Missing_transition trans -> Some trans

(* Local exception *)
exception Found_string of string

(*
   In order to match any input, each reachable state of the automaton
   must be either final or have a transition defined for any byte
   of input (0-255) and for the end-of-input/eof condition (256).

   We try to provide nice examples by favoring shorter strings.
   This is achieved by visiting the graph breadth-first instead of depth-first.
*)
let is_exhaustive
    (states : Lexgen.automata array) (initial_state : int) =
  if debug then
    printf "check initial state %i\n" initial_state;
  let rec bfs_visit
      visited
      (paths : char list DFA_state_map.t) =
    let visited, extended_paths =
      DFA_state_map.fold (fun state_id path (visited, extended_paths) ->
        let visited = DFA_states.add state_id visited in
        let state = states.(state_id) in
        if is_final state then
          (visited, extended_paths)
        else
          match get_transitions state with
          | None ->
              (visited, extended_paths)
          | Some transitions ->
              match find_missing_transition state with
              | Some symbol ->
                  (* We found a missing transition. Adding this character
                     or eof to the current path makes it a non-matching input *)
                  let failing_path =
                    if is_end_of_input symbol then
                      path
                    else
                      (Char.chr symbol) :: path
                  in
                  raise (Found_string (string_of_path failing_path))
              | None ->
                  (* We didn't find a missing transition. Follow the transitions
                     that land on a state that hasn't already been visited,
                     extending the path with the character associated with
                     the transition. *)
                  let extended_paths = ref extended_paths in
                  Array.iteri (fun symbol ((move : Lexgen.automata_move), _) ->
                    match move with
                    | Backtrack ->
                        (* missing transition that would have raised an
                           exception earlier *)
                        assert false
                    | Goto dst_state_id ->
                        if is_end_of_input symbol then
                          ()
                        else if not (DFA_states.mem dst_state_id visited)
                             && not (DFA_state_map.mem
                                       dst_state_id !extended_paths)
                        then
                          let extended_path = Char.chr symbol :: path in
                          extended_paths :=
                            DFA_state_map.add
                              dst_state_id extended_path !extended_paths
                  ) transitions;
                  (visited, !extended_paths)
      ) paths (visited, DFA_state_map.empty)
    in
    if DFA_state_map.is_empty extended_paths then
      (* We visited all the reachable nodes *)
      ()
    else
      bfs_visit visited extended_paths
  in
  try
    bfs_visit DFA_states.empty (DFA_state_map.singleton initial_state []);
    Ok ()
  with Found_string example ->
    Error example

(* TODO: obtain a good location for the rule and report it here *)
let warning (loc : Syntax.location) msg =
  eprintf "File \"%s\", line 1, character 0:\n\
           Warning: %s\n\
          "
    loc.loc_file msg

(* An entry is a 'rule' in an ocamllex file.
   It contains the rule name (useful for reporting) and the initial
   state in the automaton. *)
let check_entry loc
    (states : Lexgen.automata array)
    (e : (_, Syntax.location) Lexgen.automata_entry) =
  let initial_state, _mem_actions = e.auto_initial_state in
  match is_exhaustive states initial_state with
  | Ok () -> ()
  | Error example ->
      warning loc (sprintf "rule %s is not exhaustive.\n\
                            Here is an example of nonmatching input:\n\
                            %S"
                     e.auto_name example)

let check loc
    (states : Lexgen.automata array)
    (entries : (_, Syntax.location) Lexgen.automata_entry list) =
  if debug then (
    printf "number of states: %i\n" (Array.length states);
    Array.iteri print_state states
  );
  List.iter (check_entry loc states) entries
