(* Supermodule interface file generated with 'ocamlc -i'

   The ml file contains documented interfaces for each submodule.
*)
module Conf :
  sig
    type matching_mode = Full | Prefix
    type t = { matching_mode : matching_mode; }
    val default : t
  end
module Char_class :
  sig
    type t
    val elements : t -> char list
    val of_list : char list -> t
    val choose_opt : t -> char option
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val is_empty : t -> bool
    val singleton : char -> t
    val add : char -> t -> t
    val range : char -> char -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val any : t
    val empty : t
    val alpha : t
    val digit : t
    val mem : char -> t -> bool
    val fold : (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val iter : (char -> unit) -> t -> unit
  end
module Char_partition :
  sig
    type t
    type symbol_id = private int
    type symbol = private { id : symbol_id; chars : Char_class.t; }
    val partition : Char_class.t list -> t
    val length : t -> int
    val alphabet : t -> symbol list
    val assoc : t -> char -> symbol
    module Symbol :
      sig
        type t = symbol
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val hash : t -> int
        val show : t -> string
      end
  end
module Regexp :
  sig
    type t =
        Epsilon
      | End_of_input
      | Char of Char_class.t
      | Seq of t * t
      | Alt of t * t
      | Repeat of t
    val repeat1 : t -> t
    val opt : t -> t
  end
module NFA :
  sig
    type transition = Epsilon | Input of Char_partition.symbol | End_of_input
    type state_id = private int
    val show_state_id : state_id -> string
    type state = {
      id : state_id;
      final : bool;
      transitions : (transition, state) Hashtbl.t;
    }
    type t = {
      initial_state : state;
      states : state array;
      char_partition : Char_partition.t;
      mode : Conf.matching_mode;
    }
    val make : Conf.matching_mode -> Regexp.t -> t
  end
module DFA :
  sig
    type transition = Input of Char_partition.symbol | End_of_input
    type state_id = private int
    val show_state_id : state_id -> string
    module NFA_states :
      sig
        type elt = NFA.state
        type t
        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    val show_nfa_states : ?max_len:int -> NFA_states.t -> string
    type state = {
      id : state_id;
      nfa_states : NFA_states.t;
      final : bool;
      transitions : (transition, state) Hashtbl.t;
    }
    val show_state : state -> string
    val compare_state : state -> state -> int
    type t = {
      initial_state : state;
      states : state array;
      char_partition : Char_partition.t;
      mode : Conf.matching_mode;
    }
    val make : NFA.t -> t
  end
module Check :
  sig
    val compile : Conf.matching_mode -> Regexp.t -> DFA.t
    val is_exhaustive : DFA.t -> (unit, string) Result.t
    val matches : DFA.t -> string -> bool
  end
