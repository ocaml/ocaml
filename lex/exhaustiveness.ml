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
module DFA_states = Set.Make (Int)

(* For mapping at most one state to a matching path *)
module DFA_state_map = Map.Make (Int)

(************************)
(* Automaton navigation *)
(************************)

(* Return the state's transitions if any *)
let get_transitions (state : Lexgen.automata) =
  match state with
  | Perform _ -> None
  | Shift (_, transitions) -> Some transitions

(* Indicate if a state is final. *)
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
   Let us say that an automata state is 'total' if it matches any
   input byte (0-255) and the end-of-input/eof condition (256), and
   'partial' otherwise. The automaton is 'exhaustive' if any path from
   the input state to a partial state goes through a final state.

   Note: due the longest-match policy, there can be transitions out of a
   final states, which may lead to further final states corresponding to
   longer matches, or fail and backtrack to the last final state
   encountered.

   The [is_exhaustive] function checks that the automaton is exhaustive,
   or returns a counter-example, an input string that reaches a partial
   state without going through a final state.

   We try to provide nice examples by favoring shorter strings.
   This is achieved by visiting the graph breadth-first instead of depth-first.

   The cost of this function is bounded by the number of states and
   the number of possible transitions. This is fast enough in practice
   even though it could be faster if the characters were grouped into
   classes of equivalent characters as it is done during automaton
   construction.

   Algorithm overview:

   The input is a DFA and an initial state. A state is a node in a
   directed graph with labeled edges representing state
   transitions. Transitions represent an input symbol which is either
   a byte (of type 'char' and numbered 0-255) or the end of input
   (code 256, denoted 'eof' in ocamllex pattern syntax).

   We start from the initial state and visit all reachable states,
   stopping at final states. We fail if we encounter a partial state.
   To avoid visiting the same node multiple times, we keep track
   of the set of visited nodes.

   To obtain a sample input string, we keep an input path constructed
   from the sequence of transitions that were taken from the initial
   state. To provide one of the shortest nonmatching paths, we visit
   the graph breadth-first. This ensures all the paths being tracked
   have the same length at any given round of the visit. Since we only
   want to produce one nonmatching path, we use a map from state IDs to
   paths to ensure we keep at most one path per node.

   When a missing transition is found, we extend the path with the missing
   character (or nothing if the transition is eof) and we return it via
   an exception.
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

let make_hint ~shortest example =
  let opt_msg =
    match example with
    | "" ->
        if shortest then
          Some "consider adding '| eof { ... }'"
        else
          (* This is a very common occurrence *)
          Some {|consider adding '| "" { ... }'|}
    | str when String.length str = 1 ->
        if shortest then
          Some "consider adding '| _ eof { ... }'"
        else
          Some "consider adding '| _ { ... }'"
    | _ ->
        None
  in
  match opt_msg with
  | None -> ""
  | Some msg -> "\nHint: " ^ msg

(* An entry is a 'rule' in an ocamllex file.
   It contains the rule name (useful for reporting) and the initial
   state in the automaton. *)
let check_entry
    ~fatal
    (states : Lexgen.automata array)
    (e : (_, Syntax.location) Lexgen.automata_entry) =
  let initial_state, _mem_actions = e.auto_initial_state in
  match is_exhaustive states initial_state with
  | Ok () -> true
  | Error example ->
      Syntax.print_warning
        ~fatal
        ~name:"missing-case"
        e.auto_body_location
        (sprintf "rule \"%s\" is not exhaustive.\n\
                  Here is an example of nonmatching input:\n\
                  %S%s"
           e.auto_name example
           (make_hint ~shortest:e.auto_shortest example));
      false

let check
    ?(fatal = false)
    (states : Lexgen.automata array)
    (entries : (_, Syntax.location) Lexgen.automata_entry list) =
  if debug then (
    printf "number of states: %i\n" (Array.length states);
    Array.iteri print_state states
  );
  let results = List.map (check_entry ~fatal states) entries in
  if List.mem false results then
    Error ()
  else
    Ok ()
