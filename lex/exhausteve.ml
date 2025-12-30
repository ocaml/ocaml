(*
   This is source code from multiple files packed into one, taken from
   https://github.com/mjambon/exhausteve/tree/main/lib
   Git commit: aa222e26dd012e10eb2a03c0a1f4b9403bb6aeb9
*)

[@@@warning "-not-principal"]

module Conf = struct
  (** Global options *)

  type matching_mode =
    | Full (** the pattern must match the whole input *)
    | Prefix (** the pattern must match a prefix of the input *)

  type t = {
    matching_mode: matching_mode;
  }

  let default = {
    matching_mode = Full;
  }
end

module Char_class : sig
  (**
     Character classes

     A character class is a set of a characters that are handled identically.
     The term is used in two different but related contexts:
     - In a regular expression, it indicates that one of the characters
       of the set must match an input character.
     - In the automata derived from a regular expression (DFA, NFA),
       instead of having transitions that are identical for many characters,
       these characters with identical transitions are grouped into character
       classes which are then treated as one character. This drastically
       reduces the number of edges in the graph, making the computation of
       the automata fast and easy to debug.
  *)

  (** A character class is a set of characters (bytes of type [char]) *)
  type t

  (** Return the elements of the character class in order *)
  val elements : t -> char list
  val of_list : char list -> t

  (** Return one element *)
  val choose_opt : t -> char option

  (** Show the elements in a compact representation using ranges *)
  val show : t -> string
  val pp : Format.formatter -> t -> unit

  (** An empty character class can be used in a regular expression to
      represent the empty language as it is guaranteed to never match
      any input. *)
  val is_empty : t -> bool

  (** Add an element *)
  val singleton : char -> t
  val add : char -> t -> t

  (** A set of consecutive bytes *)
  val range : char -> char -> t

  (** Set operations to construct character classes *)
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t

  (** Predefined character classes for tests and such *)
  val any : t
  val empty : t
  val alpha : t
  val digit : t

  (** Whether a character belongs to a character class *)
  val mem : char -> t -> bool

  val fold : (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val iter : (char -> unit) -> t -> unit
end
= struct
  (* Character classes as they appear in the regexp tree

     Our automata use a partition of the byte alphabet into character classes
     of type Char_partitition.char_class which are character classes
     plus a unique identifier.
  *)

  open Printf

  module CC = Set.Make (Char)

  type t = CC.t

  let is_empty = CC.is_empty

  let add = CC.add
  let singleton = CC.singleton

  let range a b =
    let c = ref CC.empty in
    for i = Char.code a to Char.code b do
      c := CC.add (Char.chr i) !c
    done;
    !c

  let union = CC.union
  let inter = CC.inter
  let diff = CC.diff

  let any = range '\000' '\255'
  let empty = CC.empty
  let alpha = union (range 'a' 'z') (range 'A' 'Z')
  let digit = range '0' '9'

  let mem = CC.mem
  let elements = CC.elements
  let of_list = CC.of_list
  let choose_opt = CC.choose_opt
  let fold = CC.fold
  let iter = CC.iter

  let elements_as_ranges cc =
    let rec loop ranges first last chars =
      match chars with
      | [] ->
          List.rev ((first, last) :: ranges)
      | c :: chars ->
          (* Either extend the ongoing range or close it and add it to
             the accumulator *)
          if Char.code c = Char.code last + 1 then
            loop ranges first c chars
          else
            loop ((first, last) :: ranges) c c chars
    in
    (* Get the set of characters in lexicographic order, then identify
       consecutive ranges by scanning the list from left to right *)
    match elements cc with
    | first :: chars -> loop [] first first chars
    | [] -> []

  let show_char c = sprintf "%C" c

  let show_range (first, last) =
    if first = last then
      show_char first
    else if Char.code last = Char.code first + 1 then
      sprintf "%s, %s" (show_char first) (show_char last)
    else
      sprintf "%s-%s" (show_char first) (show_char last)

  let show cc =
    cc
    |> elements_as_ranges
    |> List.map show_range
    |> String.concat ", "

  let pp fmt cc =
    Format.pp_print_string fmt (show cc)
end

module Char_partition : sig
  (** Map a character to a unique character class. *)

  type t

  (** The zero-based index of a character class *)
  type symbol_id = private int

  (** A char class with its index within a partition *)
  type symbol = private {
    id: symbol_id;
    chars: Char_class.t;
  }

  (** Take a list of character classes S and return the partition P
      of the alphabet (the 256 bytes) into character classes such that
      for any p in P, all the characters in p occur in the same character
      classes in S.

      The input S is normally all the character classes extracted from
      the root regular expression. The partition is a grouping of characters
      into a smaller alphabet to be used to represent automaton transitions.
  *)
  val partition : Char_class.t list -> t

  (** Return the number N of character classes in the partition of the bytes,
      1 <= N <= 256. This is the size of the alphabet used for automaton
      transitions. *)
  val length : t -> int

  (** Return all the valid symbols *)
  val alphabet : t -> symbol list

  (** Associate a character with its char_class in the partition *)
  val assoc : t -> char -> symbol

  (** Standard operations needed to feed Set.Make, Map.Make,
      and Hashtbl.Make functors.

      We could use a functor to ensure statically that two char_class
      are indeed from the same partition but it's overkill. *)
  module Symbol : sig
    type t = symbol
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val show : t -> string
  end
end
= struct
  (** Map a character to a unique character class. *)

  open Printf

  type symbol_id = int

  type symbol = {
    id: symbol_id;
    chars: Char_class.t;
  }

  type t = {
    (* Array of character classes whose ID is the position in the array *)
    partition: symbol array;
    (* Array of length 256 mapping a char to its character class *)
    bytes: symbol array;
  }

  let length p = Array.length p.partition

  let alphabet p = Array.to_list p.partition

  let assoc p c = p.bytes.(Char.code c)

  let partition overlapping_char_classes =
    let input_ar = Array.of_list overlapping_char_classes in
    let subsets = Hashtbl.create 10 in
    let id_counter = ref 0 in
    let new_id () =
      let res = !id_counter in
      incr id_counter;
      res
    in
    (* For each character, build the list of character classes it belongs to
       (identified by their position in the input list) *)
    let bytes_map =
      Array.init 256 (fun i ->
        let char = Char.chr i in
        let memberships = ref [] in
        for j = Array.length input_ar - 1 downto 0 do
          if Char_class.mem char input_ar.(j) then
            memberships := j :: !memberships
        done;
        let key = !memberships in
        (match Hashtbl.find_opt subsets key with
         | None ->
             Hashtbl.add subsets key { id = new_id ();
                                       chars = Char_class.singleton char }
         | Some { id; chars } ->
             Hashtbl.replace subsets key { id;
                                           chars = Char_class.add char chars });
        key
      )
    in
    (* Finalize the subsets, order them by their assigned numeric ID *)
    let partition =
      let final_character_classes =
        Hashtbl.fold (fun _key cc ccs -> cc :: ccs) subsets []
      in
      let ar =
        List.sort (fun a b -> Int.compare a.id b.id) final_character_classes
        |> Array.of_list
      in
      Array.iteri (fun i x -> assert (x.id = i)) ar;
      ar
    in
    (* Create the map from byte to character class *)
    let bytes =
      Array.map (fun membership_key ->
        try Hashtbl.find subsets membership_key
        with Not_found -> assert false
      ) bytes_map
    in
    {
      partition;
      bytes;
    }

  module Symbol = struct
    type t = symbol
    let compare a b = Int.compare a.id b.id
    let equal a b = a.id = b.id
    let hash a = a.id

    let show_id id =
      sprintf "C%i" id

    let show x =
      sprintf "%s {%s}" (show_id x.id) (Char_class.show x.chars)
  end
end

module Regexp : sig
  (**
     A tree representing a regular expression over bytes
  *)

  (** The type of a regular expression

  {v
     let a = Char_class.singleton 'a'
     let b = Char_class.singleton 'b'

     a   : Char a
     ab  : Seq (Char a, Char b)
     a*  : Repeat (Char a)
     a|b : Alt (Char a, Char b)
     a+  : Seq (Char a, Repeat (Char a))
     a?  : Alt (Char a, Epsilon)
  v}
  *)
  type t =
    | Epsilon (** match the empty sequence *)
    | End_of_input (** match at the end of the string; useful in prefix mode *)
    | Char of Char_class.t (** match any input character in the character
                               class *)
    | Seq of t * t (** match two patterns in sequence *)
    | Alt of t * t (** match either one pattern or the other *)
    | Repeat of t (** match a pattern repeatedly, zero times or more *)
  [@@deriving show { with_path = false }]

  (** Match the pattern once or multiple times ("+" quantifier) *)
  val repeat1 : t -> t

  (** Match the pattern at most once ("?" quantifier) *)
  val opt : t -> t
end
= struct
  (*
     A tree representing a regular expression over bytes
  *)

  (* The type of a regular expression *)
  type t =
    | Epsilon
    | End_of_input
    | Char of Char_class.t
    | Seq of t * t
    | Alt of t * t
    | Repeat of t
  [@@deriving show { with_path = false }]

  let repeat1 re = Seq (re, Repeat re)

  let opt re = Alt (re, Epsilon)
end

module NFA : sig
  (** NFA representing a regular expression *)

  type transition =
    | Epsilon
        (** an empty transition; those aren't allowed in DFAs *)
    | Input of Char_partition.symbol
        (** a character of input being consumed *)
    | End_of_input
        (** end of input; treated mostly like an input character *)

  type state_id = private int

  val show_state_id : state_id -> string

  (** A state in the automaton i.e. a node in a directed graph with labeled
      edges.

      A state is said final or accepting if it marks the successful end
      of the match between the pattern and the input data.

      A transition is an arrow that usually takes us to another state while
      at the same time consuming a character of input. In an NFA, it's also
      possible to not consume any character of input and such transitions
      are called epsilon transitions. It's also possible to be sent to the
      same state while consuming a character of input.

      In an NFA, multiple identical transitions can exist and lead to
      different states.
  *)
  type state = {
    id: state_id;
      (** Unique state/node identifier *)
    final: bool;
      (** Whether this state is accepting/final *)
    transitions: (transition, state) Hashtbl.t;
      (** A transition links to one or more states. Use [Hashtbl.find_all]
          to access them. *)
  }

  (** The automaton defined over symbols that are groupings of equivalent
      characters. *)
  type t = {
    initial_state: state;
    states: state array;
    char_partition: Char_partition.t;
    mode: Conf.matching_mode;
  }

  (** Build an NFA equivalent to the given regular expression.

      The matching mode specifies whether we want to match the whole input
      string or if leaving some input unmatched is acceptable. *)
  val make : Conf.matching_mode -> Regexp.t -> t
end
= struct
  (* NFA representing a regular expression *)

  open Printf

  type transition =
    | Epsilon
    | Input of Char_partition.symbol
    | End_of_input

  type state_id = int

  (* 42 -> "N42" to avoid confusion with DFA states named Dxxx *)
  let show_state_id x = sprintf "N%i" x

  (* A state in the automaton i.e. a node in a directed graph with labeled
     edges.

     A state is said "final" or "accepting" if it marks the successful end
     of the match between the pattern and the input data.

     A transition is an arrow that takes us to another state while at the
     same time consuming a character of input. In an NFA, it's also possible
     to not consume any character of input and such transitions are called
     epsilon transitions.

     In an NFA, multiple identical transitions can exist and lead to
     different states.
  *)
  type state = {
    (* Unique state/node identifier *)
    id: state_id;
    (* Whether this state is accepting/final *)
    final: bool;
    (* A transition links to one or more states *)
    transitions: (transition, state) Hashtbl.t;
  }

  type t = {
    initial_state: state;
    states: state array;
    char_partition: Char_partition.t;
    mode: Conf.matching_mode;
  }

  (* Local regexp type over symbols rather than chars *)
  module RE = struct
    type t =
    | Empty (* empty character class *)
    | Epsilon
    | End_of_input
    | Char of Char_partition.symbol
    | Seq of t * t
    | Alt of t * t
    | Repeat of t
  end

  module Symbols = Set.Make (Char_partition.Symbol)

  let rec collect_char_classes (re : Regexp.t) : Char_class.t list =
    match re with
    | Epsilon -> []
    | End_of_input -> []
    | Char cc -> [cc]
    | Seq (a, b) -> collect_char_classes a @ collect_char_classes b
    | Alt (a, b) -> collect_char_classes a @ collect_char_classes b
    | Repeat a -> collect_char_classes a

  (*
     Map a character class in the original alphabet (char/bytes) to
     a smaller alphabet obtained by grouping characters that are treated
     equivalently by the automata. A Char_partition.symbol is a
     symbol in the new alphabet.
  *)
  let map_char_class (p : Char_partition.t) (cc : Char_class.t) : RE.t =
    let symbols =
      Char_class.fold (fun char symbols ->
        let symbol = Char_partition.assoc p char in
        Symbols.add symbol symbols
      ) cc Symbols.empty
    in
    match Symbols.elements symbols with
    | [] -> Empty
    | symbol :: symbols ->
        List.fold_right (fun symbol re -> RE.Alt (Char symbol, re))
          symbols (RE.Char symbol)

  let map_regexp (re : Regexp.t) : Char_partition.t * RE.t =
    (* Define the new alphabet by grouping equivalent characters *)
    let p = Char_partition.partition (collect_char_classes re) in
    (* Map the Char nodes to the new regexp type to be translated to an
       automaton *)
    let rec map (re : Regexp.t) : RE.t =
      match re with
      | Epsilon -> Epsilon
      | End_of_input -> End_of_input
      | Seq (a, b) -> Seq (map a, map b)
      | Alt (a, b) -> Alt (map a, map b)
      | Repeat a -> Repeat (map a)
      | Char cc -> map_char_class p cc
    in
    (p, map re)

  let make (mode: Conf.matching_mode) (re : Regexp.t) : t =
    let char_partition, re = map_regexp re in

    let state_counter = ref 0 in

    let new_id () =
      let res = !state_counter in
      incr state_counter;
      res
    in

    let all_states = Hashtbl.create 100 in

    let create_state ?(final = false) () =
      let id = new_id () in
      let state = {
        id;
        final;
        transitions = Hashtbl.create 10
      } in
      Hashtbl.add all_states id state;
      state
    in

    let add_transition from_state trans to_state =
      (* There may be multiple values under the same key (unlike in a DFA),
         to be retrieved with Hashtbl.find_all *)
      match trans with
      (* Avoid infinite loops: staying on the same state is only allowed
         when consuming input *)
      | Epsilon when from_state.id = to_state.id ->
          ()
      | _ ->
          Hashtbl.add from_state.transitions trans to_state
    in

    (* Translate the regular expression to take us from the current state
       to the next state after this regexp *)
    let rec translate_regexp cur_state (re : RE.t) next_state =
      match re with
      | Empty -> ()
      | Epsilon ->
          add_transition cur_state Epsilon next_state
      | End_of_input ->
          add_transition cur_state End_of_input next_state
      | Char c ->
          add_transition cur_state (Input c) next_state
      | Seq (a, b) ->
          let state = create_state () in
          translate_regexp cur_state a state;
          translate_regexp state b next_state
      | Alt (a, b) ->
          let state_a = create_state () in
          add_transition cur_state Epsilon state_a;
          translate_regexp state_a a next_state;
          let state_b = create_state () in
          add_transition cur_state Epsilon state_b;
          translate_regexp state_b b next_state
      | Repeat a ->
          let state = create_state () in
          add_transition cur_state Epsilon state;
          translate_regexp state a state;
          add_transition state Epsilon next_state
    in
    let initial_state = create_state () in
    let penultimate_state =
      (* This is the last state before requiring an end-of-input
         pseudocharacter in full mode. In prefix mode, we don't require
         reaching the end of the input so we mark this state as accepting
         (final). *)
      match mode with
      | Full ->
          let penultimate_state = create_state () in
          let final_state = create_state ~final:true () in
          add_transition penultimate_state End_of_input final_state;
          penultimate_state
      | Prefix ->
          create_state ~final:true ()
    in
    translate_regexp initial_state re penultimate_state;
    let state_array =
      Hashtbl.fold (fun _id state acc -> state :: acc) all_states []
      |> List.sort (fun a b -> compare a.id b.id)
      |> Array.of_list
    in
    Array.iteri (fun i state -> assert (state.id = i)) state_array;
    { initial_state;
      states = state_array;
      char_partition;
      mode }
end

module DFA : sig
  (** DFA (deterministic finite automaton) representing a regular expression *)

  type transition =
    | Input of Char_partition.symbol
    | End_of_input

  type state_id = private int

  val show_state_id : state_id -> string

  module NFA_states : Set.S with type elt = NFA.state

  val show_nfa_states : ?max_len:int -> NFA_states.t -> string

  (** A DFA state. The original unique ID is a set of NFA state IDs.
      The [id] field is a unique int generated from a counter. *)
  type state = {
    id: state_id;
    nfa_states: NFA_states.t;
    final: bool;
    transitions: (transition, state) Hashtbl.t;
  }

  val show_state : state -> string

  val compare_state : state -> state -> int

  type t = {
    initial_state: state;
    states: state array;
    char_partition: Char_partition.t;
    mode: Conf.matching_mode;
  }

  (** Build a DFA equivalent to the given NFA which itself is built
      from a regular expression. *)
  val make : NFA.t -> t
end
= struct
  (* DFA (deterministic finite automaton) representing a regular expression *)

  open Printf

  type transition =
    | Input of Char_partition.symbol
    | End_of_input

  type state_id = int

  let compare_state_id = Int.compare

  (* 42 -> "D42" to avoid confusion with NFA states named Nxxx *)
  let show_state_id id = sprintf "D%i" id

  let get_possible_transitions p =
    End_of_input ::
    (Char_partition.alphabet p |> List.map (fun symbol -> Input symbol))

  let nfa_trans_of_dfa_trans (trans : transition) : NFA.transition =
    match trans with
    | Input c -> Input c
    | End_of_input -> End_of_input

  (* A set of NFA state IDs, identifying a DFA state *)
  module NFA_states = Set.Make (struct
      type t = NFA.state
      let compare (a : NFA.state) (b : NFA.state) = compare a.id b.id
  end)

  let hash_nfa_states (x : NFA_states.t) =
    (* Get the elements as a sorted list before hashing them *)
    Hashtbl.hash (NFA_states.elements x)

  let union_of_nfa_transitions nfa_states =
    let tbl = Hashtbl.create 10 in
    NFA_states.iter (fun (state : NFA.state) ->
      Hashtbl.iter (fun trans dst_state ->
        Hashtbl.add tbl trans dst_state
      ) state.transitions
    ) nfa_states;
    tbl

  (* A DFA state. The original unique ID is a set of NFA state IDs.
     The 'id' field is a unique int generated from a counter. *)
  type state = {
    id: state_id;
    nfa_states: NFA_states.t;
    final: bool;
    transitions: (transition, state) Hashtbl.t;
  }

  type t = {
    initial_state: state;
    states: state array;
    char_partition: Char_partition.t;
    mode: Conf.matching_mode;
  }

  let compare_state a b =
    compare_state_id a.id b.id

  let show_nfa_states ?max_len nfa_states =
    let all = NFA_states.elements nfa_states in
    (match max_len with
     | Some n when List.length all > n ->
         (List.take n all
          |> List.map (fun (state : NFA.state) -> NFA.show_state_id state.id)
          |> String.concat ", ")
         ^ ", ..."
     | None
     | Some _ ->
         all
         |> List.map (fun (state : NFA.state) -> NFA.show_state_id state.id)
         |> String.concat ", "
    )
    |> sprintf "{%s}"

  let show_state state =
    sprintf "%s %s (%i transitions)%s"
      (show_state_id state.id)
      (show_nfa_states state.nfa_states)
      (Hashtbl.length state.transitions)
      (if state.final then " final"
       else "")

  (* A hash table module for mapping DFA state IDs to anything *)
  module NFA_states_tbl = Hashtbl.Make (struct
    type t = NFA_states.t
    let hash = hash_nfa_states
    let equal = NFA_states.equal
  end)

  (* Produce a set of all the states reachable via zero or more
     epsilon transitions *)
  let epsilon_closure (state : NFA.state) : NFA_states.t =
    let rec visit visited state =
      if NFA_states.mem state visited then
        visited
      else
        let visited = NFA_states.add state visited in
        let dst_states = Hashtbl.find_all state.transitions Epsilon in
        List.fold_left visit visited dst_states
    in
    visit NFA_states.empty state

  let merge_dst_nfa_states
      (nfa_states_before_epsilon_closure : NFA.state list) =
    List.fold_left (fun states state ->
      NFA_states.union states (epsilon_closure state)
    ) NFA_states.empty nfa_states_before_epsilon_closure

  let make (nfa : NFA.t) : t =
    let state_counter = ref 0 in

    let new_id () =
      let id = !state_counter in
      incr state_counter;
      id
    in

    let all_states = NFA_states_tbl.create 100 in

    (* Get or create a DFA state from a set of NFA states *)
    let get_dfa_state nfa_states =
      match NFA_states_tbl.find_opt all_states nfa_states with
      | Some state -> state
      | None ->
          let id = new_id () in
          let final =
            NFA_states.exists
              (fun (state : NFA.state) -> state.final) nfa_states in
          let state = {
            id;
            nfa_states;
            final;
            transitions = Hashtbl.create 10
          } in
          NFA_states_tbl.add all_states nfa_states state;
          state
    in

    let possible_transitions = get_possible_transitions nfa.char_partition in

    let rec translate_nfa_states (dfa_state : state) =
      let nfa_transitions = union_of_nfa_transitions dfa_state.nfa_states in
      (* Iterate over the alphabet *)
      List.iter (fun possible_trans ->
        let dst_nfa_states =
          Hashtbl.find_all nfa_transitions
            (nfa_trans_of_dfa_trans possible_trans)
          |> merge_dst_nfa_states
        in
        if not (NFA_states.is_empty dst_nfa_states) then
          let dst_dfa = get_dfa_state dst_nfa_states in
          if not (Hashtbl.mem dfa_state.transitions possible_trans) then (
            Hashtbl.add dfa_state.transitions possible_trans dst_dfa;
            translate_nfa_states dst_dfa
          )
      ) possible_transitions
    in

    let nfa_starts = merge_dst_nfa_states [nfa.initial_state] in
    let dfa_start = get_dfa_state nfa_starts in
    translate_nfa_states dfa_start;

    let state_array =
      NFA_states_tbl.fold (fun _id state acc -> state :: acc) all_states []
      |> List.sort (fun a b -> compare a.id b.id)
      |> Array.of_list
    in
    Array.iteri (fun i state -> assert (state.id = i)) state_array;

    { initial_state = dfa_start;
      states = state_array;
      char_partition = nfa.char_partition;
      mode = nfa.mode }
end

module Check : sig
  (** Analyze the DFA for exhaustiveness *)

  (** Convert a regular expression into a DFA usable for matching or
      for analysis. To get all the DFA states or to check the intermediate
      NFA, use the NFA and DFA modules directly instead. *)
  val compile : Conf.matching_mode -> Regexp.t -> DFA.t

  (** Check whether a DFA starting from the given state can match
      any input. If not, an example of non-matching input is returned. *)
  val is_exhaustive : DFA.t -> (unit, string) Result.t

  (** Test whether a string matches a regexp. This isn't very efficient,
      it's for testing purposes. *)
  val matches : DFA.t -> string -> bool
end
= struct
  (* Analyze the DFA for exhaustiveness *)

  let compile matching_mode re =
    let nfa = NFA.make matching_mode re in
    let dfa = DFA.make nfa in
    dfa

  module DFA_states = Set.Make (struct
      type t = DFA.state
      let compare = DFA.compare_state
  end)

  module DFA_state_map = Map.Make (struct
      type t = DFA.state
      let compare = DFA.compare_state
  end)

  let string_of_path (path : Char_partition.symbol list) =
    path
    |> List.rev
    |> List.map (fun (symbol: Char_partition.symbol) ->
      match Char_class.choose_opt symbol.chars with
      | None ->
          (* A symbol may not be created for an empty character class *)
          assert false
      | Some char -> char
    )
    |> List.to_seq
    |> String.of_seq

  exception Missing_transition of DFA.transition

  let get_possible_transitions (p : Char_partition.t) =
    DFA.End_of_input
    :: List.map (fun symbol -> DFA.Input symbol) (Char_partition.alphabet p)

  let find_missing_transition possible_transitions (state : DFA.state) =
    if state.final then
      None
    else
      let transitions = state.transitions in
      try
        List.iter (fun trans ->
          if not (Hashtbl.mem transitions trans) then
            raise (Missing_transition trans)
        ) possible_transitions;
        None
      with Missing_transition trans -> Some trans

  exception Found_string of string

  (*
     The DFA represents a regular expression.

     In order to match any input, each state of the automaton must be either
     final or have a transition defined for any character including the
     special end-of-input character.

     We try to provide nice examples by favoring shorter input strings.
     This is achieved by visiting the graph breadth-first instead of depth-first.
  *)
  let is_exhaustive (dfa : DFA.t) =
    let possible_transitions = get_possible_transitions dfa.char_partition in
    let rec bfs_visit
        visited
        (paths : Char_partition.symbol list DFA_state_map.t) =
      let visited, extended_paths =
        DFA_state_map.fold (fun state path (visited, extended_paths) ->
          let visited = DFA_states.add state visited in
          if state.final then
            (visited, extended_paths)
          else
            match find_missing_transition possible_transitions state with
            | Some trans ->
                (* We found a missing transition. Adding this character or eof to
                   the current path makes it a non-matching input *)
                let failing_path =
                  match trans with
                  | End_of_input -> path
                  | Input c -> c :: path
                in
                raise (Found_string (string_of_path failing_path))
            | None ->
                (* We didn't find a missing transition. Follow the transitions
                   that land on a state that hasn't already been visited,
                   extending the path with the character associated with
                   the transition. *)
                let extended_paths =
                  Hashtbl.fold (fun trans dst_state extended_paths ->
                    match (trans : DFA.transition) with
                    | End_of_input -> extended_paths
                    | Input c ->
                        if not (DFA_states.mem dst_state visited)
                        && not (DFA_state_map.mem dst_state extended_paths) then
                          DFA_state_map.add dst_state (c :: path) extended_paths
                        else
                          extended_paths
                  ) state.transitions extended_paths
                in
                (visited, extended_paths)
        ) paths (visited, DFA_state_map.empty)
      in
      if DFA_state_map.is_empty extended_paths then
        (* We visited all the reachable nodes *)
        ()
      else
        bfs_visit visited extended_paths
    in
    try
      bfs_visit DFA_states.empty (DFA_state_map.singleton dfa.initial_state []);
      Ok ()
    with Found_string example ->
      Error example

  let matches (dfa : DFA.t) str =
    let rec matches (state : DFA.state) chars =
      if state.final then
        true
      else
        match chars with
        | [] ->
            (match Hashtbl.find_opt state.transitions End_of_input with
             | Some state -> state.final
             | None -> false)
        | char :: chars ->
            let symbol = Char_partition.assoc dfa.char_partition char in
            match Hashtbl.find_opt state.transitions (Input symbol) with
            | Some state -> matches state chars
            | None -> false
    in
    let chars =
      str
      |> String.to_seq
      |> List.of_seq
    in
    matches dfa.initial_state chars
end

