module Convert : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module defines two APIs for parsers, namely the traditional API
   and the revised API, and offers conversions between these APIs. *)

(**An ocamlyacc-style, or Menhir-style, parser requires access to
   the lexer, which must be parameterized with a lexing buffer, and
   to the lexing buffer itself, where it reads position information.

   This traditional API is convenient when used with ocamllex, but
   inelegant when used with other lexer generators. *)
type ('token, 'semantic_value) traditional =
    (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

(**The revised API is independent of any lexer generator. Here, the
   parser only requires access to the lexer, and the lexer takes no
   parameters. The tokens returned by the lexer may contain position
   information. *)
type ('token, 'semantic_value) revised =
    (unit -> 'token) -> 'semantic_value

(* --------------------------------------------------------------------------- *)

(**[traditional2revised] converts a traditional parser, produced by
   ocamlyacc or Menhir, into a revised parser.

   A token of the revised lexer is essentially a triple of a token
   of the traditional lexer (or raw token), a start position, and
   and end position. The three [get] functions are accessors.

   We do not require the type ['token] to actually be a triple type.
   This enables complex applications where it is a record type with
   more than three fields. It also enables simple applications where
   positions are of no interest, so ['token] is just ['raw_token]
   and [get_startp] and [get_endp] return dummy positions. *)
val traditional2revised:
  ('token -> 'raw_token) ->
  ('token -> Lexing.position) ->
  ('token -> Lexing.position) ->
  ('raw_token, 'semantic_value) traditional ->
  ('token, 'semantic_value) revised

(* --------------------------------------------------------------------------- *)

(**[revised2traditional] converts a revised parser to a traditional parser. *)
val revised2traditional:
  ('raw_token -> Lexing.position -> Lexing.position -> 'token) ->
  ('token, 'semantic_value) revised ->
  ('raw_token, 'semantic_value) traditional

(* --------------------------------------------------------------------------- *)

(**This submodule offers simplified versions of the above conversions.
   In this simplified API, concrete triples are used. *)
module Simplified : sig

  (**[traditional2revised] converts a traditional parser, produced by
     ocamlyacc or Menhir, into a revised parser. *)
  val traditional2revised:
    ('token, 'semantic_value) traditional ->
    ('token * Lexing.position * Lexing.position, 'semantic_value) revised

  (**[revised2traditional] converts a revised parser to a traditional parser. *)
  val revised2traditional:
    ('token * Lexing.position * Lexing.position, 'semantic_value) revised ->
    ('token, 'semantic_value) traditional

end
end
module IncrementalEngine : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

type position = Lexing.position

(**This signature describes the incremental LR engine. When the engine
   is used in this mode, the user controls the lexer, and the parser
   suspends itself when it needs to read a new token. *)
module type INCREMENTAL_ENGINE = sig

  type token

  (**A value of type {!production} is (an index for) a production. The start
     productions (which do not exist in an [.mly] file, but are constructed by
     Menhir internally) are not part of this type. *)
  type production

  (**A value of type ['a env] represents a configuration of the automaton:
     current state, stack, lookahead token, etc. The parameter ['a] is the
     type of the semantic value that will eventually be produced if the parser
     succeeds.

     In normal operation, the parser works with checkpoints: see the functions
     {!offer} and {!resume}. However, it is also possible to work directly with
     environments (see the functions {!pop}, {!force_reduction}, and [feed]) and
     to reconstruct a checkpoint out of an environment (see {!input_needed}).
     This is considered advanced functionality; its purpose is to allow error
     recovery strategies to be programmed by the user. *)
  type 'a env

  (**The type ['a checkpoint] represents an intermediate or final state of the
     parser. An intermediate checkpoint is a suspension: it records the parser's
     current state, and allows parsing to be resumed. The parameter ['a] is
     the type of the semantic value that will eventually be produced if the
     parser succeeds.

     [Accepted] and [Rejected] are final checkpoints. [Accepted] carries a
     semantic value.

     [InputNeeded] is an intermediate checkpoint. It means that the parser wishes
     to read one token before continuing.

     [Shifting] is an intermediate checkpoint. It means that the parser is taking
     a shift transition. It exposes the state of the parser before and after
     the transition. The Boolean parameter tells whether the parser intends to
     request a new token after this transition. (It always does, except when
     it is about to accept.)

     [AboutToReduce] is an intermediate checkpoint. It means that the parser is
     about to perform a reduction step. It exposes the parser's current
     state as well as the production that is about to be reduced.

     [HandlingError] is an intermediate checkpoint. It means that the parser has
     detected an error and is currently handling it, in several steps. *)
  type 'a checkpoint = private
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  (**[offer] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [InputNeeded env]. [offer] expects
     the old checkpoint as well as a new token and produces a new checkpoint.
     It does not raise any exception. *)
  val offer:
    'a checkpoint ->
    token * position * position ->
    'a checkpoint

  (**The optional argument [strategy] influences the manner in which {!resume}
     deals with checkpoints of the form [HandlingError _]. Its default value
     is [`Legacy]. It can be briefly described as follows:

     - If the [error] token is used only to report errors (that is, if the
       [error] token appears only at the end of a production, whose semantic
       action raises an exception) then the simplified strategy should be
       preferred. (This includes the case where the [error] token does not
       appear at all in the grammar.)

     - If the [error] token is used to recover after an error, or if
       perfect backward compatibility is required, the legacy strategy
       should be selected.

     More details on strategies appear in the file [Engine.ml]. *)
  type strategy =
    [ `Legacy | `Simplified ]

  (**[resume] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [Shifting _], [AboutToReduce _], or
     [HandlingError _]. [resume] expects the old checkpoint and produces a
     new checkpoint. It does not raise any exception. *)
  val resume:
    ?strategy:strategy ->
    'a checkpoint ->
    'a checkpoint

  (**A token supplier is a function of no arguments which delivers a new token
     (together with its start and end positions) every time it is called. *)
  type supplier =
    unit -> token * position * position

  (**A pair of a lexer and a lexing buffer can be turned into a supplier. *)
  val lexer_lexbuf_to_supplier:
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    supplier

  (**The functions {!offer} and {!resume} are sufficient to write a parser loop.
     One can imagine many variations (which is why we expose these functions
     in the first place!). Here, we expose a few variations of the main loop,
     ready for use. *)

  (**[loop supplier checkpoint] begins parsing from [checkpoint], reading
     tokens from [supplier]. It continues parsing until it reaches a
     checkpoint of the form [Accepted v] or [Rejected]. In the former case, it
     returns [v]. In the latter case, it raises the exception [Error].
     The optional argument [strategy], whose default value is [Legacy],
     is passed to {!resume} and influences the error-handling strategy. *)
  val loop: ?strategy:strategy -> supplier -> 'a checkpoint -> 'a

  (**[loop_handle succeed fail supplier checkpoint] begins parsing from
     [checkpoint], reading tokens from [supplier]. It continues parsing until
     it reaches a checkpoint of the form [Accepted v] or [HandlingError env]
     (or [Rejected], but that should not happen, as [HandlingError _] will be
     observed first). In the former case, it calls [succeed v]. In the latter
     case, it calls [fail] with this checkpoint. It cannot raise [Error].

     This means that Menhir's error-handling procedure does not get a chance
     to run. For this reason, there is no [strategy] parameter. Instead, the
     user can implement her own error handling code, in the [fail]
     continuation. *)
  val loop_handle:
    ('a -> 'answer) ->
    ('a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (**[loop_handle_undo] is analogous to [loop_handle], except it passes a pair
     of checkpoints to the failure continuation.

     The first (and oldest) checkpoint is the last [InputNeeded] checkpoint that
     was encountered before the error was detected. The second (and newest)
     checkpoint is where the error was detected, as in [loop_handle]. Going back
     to the first checkpoint can be thought of as undoing any reductions that
     were performed after seeing the problematic token. (These reductions must
     be default reductions or spurious reductions.)

     [loop_handle_undo] must initially be applied to an [InputNeeded] checkpoint.
     The parser's initial checkpoints satisfy this constraint. *)
  val loop_handle_undo:
    ('a -> 'answer) ->
    ('a checkpoint -> 'a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (**[shifts checkpoint] assumes that [checkpoint] has been obtained by
     submitting a token to the parser. It runs the parser from [checkpoint],
     through an arbitrary number of reductions, until the parser either
     accepts this token (i.e., shifts) or rejects it (i.e., signals an error).
     If the parser decides to shift, then [Some env] is returned, where [env]
     is the parser's state just before shifting. Otherwise, [None] is
     returned.

     It is desirable that the semantic actions be side-effect free, or that
     their side-effects be harmless (replayable). *)
  val shifts: 'a checkpoint -> 'a env option

  (**The function [acceptable] allows testing, after an error has been
     detected, which tokens would have been accepted at this point. It is
     implemented using [shifts]. Its argument should be an [InputNeeded]
     checkpoint.

     For completeness, one must undo any spurious reductions before carrying out
     this test -- that is, one must apply [acceptable] to the FIRST checkpoint
     that is passed by [loop_handle_undo] to its failure continuation.

     This test causes some semantic actions to be run! The semantic actions
     should be side-effect free, or their side-effects should be harmless.

     The position [pos] is used as the start and end positions of the
     hypothetical token, and may be picked up by the semantic actions. We
     suggest using the position where the error was detected. *)
  val acceptable: 'a checkpoint -> token -> position -> bool

  (**The abstract type ['a lr1state] describes the non-initial states of the
     LR(1) automaton. The index ['a] represents the type of the semantic value
     associated with this state's incoming symbol. *)
  type 'a lr1state

  (**The states of the LR(1) automaton are numbered (from 0 and up). *)
  val number: _ lr1state -> int

  (* Productions are numbered. *)

  (**[production_index] maps a production to its integer index. *)
  val production_index: production -> int

  (**[find_production] maps a production index to a production.
     Its argument must be a valid index; use with care. *)
  val find_production: int -> production

  (**An element is a pair of a non-initial state [s] and a semantic value [v]
     associated with the incoming symbol of this state. The idea is, the value
     [v] was pushed onto the stack just before the state [s] was entered. Thus,
     for some type ['a], the state [s] has type ['a lr1state] and the value [v]
     has type ['a]. In other words, the type [element] is an existential type. *)
  type element =
    | Element: 'a lr1state * 'a * position * position -> element

  (**The parser's stack is (or, more precisely, can be viewed as) a stream of
     elements. The functions {!top} and {!pop} offer access to this stream. *)

  (**[top env] returns the parser's top stack element. The state contained in
     this stack element is the current state of the automaton. If the stack is
     empty, [None] is returned. In that case, the current state of the
     automaton must be an initial state. *)
  val top: 'a env -> element option

  (**[pop_many i env] pops [i] cells off the automaton's stack. This is done
     via [i] successive invocations of [pop]. Thus, [pop_many 1] is [pop]. The
     index [i] must be nonnegative. The time complexity is O(i). *)
  val pop_many: int -> 'a env -> 'a env option

  (**[get i env] returns the parser's [i]-th stack element. The index [i] is
     0-based: thus, [get 0] is [top]. If [i] is greater than or equal to the
     number of elements in the stack, [None] is returned. The time complexity
     is O(i). *)
  val get: int -> 'a env -> element option

  (**[current_state_number env] is (the integer number of) the automaton's
     current state. This works even if the automaton's stack is empty, in
     which case the current state is an initial state. This number can be
     passed as an argument to a [message] function generated by [menhir
     --compile-errors]. *)
  val current_state_number: 'a env -> int

  (**[equal env1 env2] tells whether the parser configurations [env1] and
     [env2] are equal in the sense that the automaton's current state is the
     same in [env1] and [env2] and the stack is *physically* the same in
     [env1] and [env2]. If [equal env1 env2] is [true], then the sequence of
     the stack elements, as observed via {!pop} and {!top}, must be the same in
     [env1] and [env2]. Also, if [equal env1 env2] holds, then the checkpoints
     [input_needed env1] and [input_needed env2] must be equivalent. The
     function [equal] has time complexity O(1). *)
  val equal: 'a env -> 'a env -> bool

  (**[positions env] returns the start and end positions of the current
     lookahead token. In an initial state, a pair of twice the initial
     position is returned. *)
  val positions: 'a env -> position * position

  (**When applied to an environment taken from a checkpoint of the form
     [AboutToReduce (env, prod)], the function [env_has_default_reduction]
     tells whether the reduction that is about to take place is a default
     reduction. *)
  val env_has_default_reduction: 'a env -> bool

  (**[state_has_default_reduction s] tells whether the state [s] has a default
     reduction. This includes the case where [s] is an accepting state. *)
  val state_has_default_reduction: _ lr1state -> bool

  (**[pop env] returns a new environment, where the parser's top stack cell
     has been popped off. (If the stack is empty, [None] is returned.) This
     amounts to pretending that the (terminal or nonterminal) symbol that
     corresponds to this stack cell has not been read. *)
  val pop: 'a env -> 'a env option

  (**[force_reduction prod env] should be called only if in the state [env]
     the parser is capable of reducing the production [prod]. If this
     condition is satisfied, then this production is reduced, which means that
     its semantic action is executed (this can have side effects!) and the
     automaton makes a goto (nonterminal) transition. If this condition is not
     satisfied, [Invalid_argument _] is raised. *)
  val force_reduction: production -> 'a env -> 'a env

  (**[input_needed env] returns [InputNeeded env]. That is, out of an [env]
     that might have been obtained via a series of calls to the functions
     [pop], [force_reduction], [feed], etc., it produces a checkpoint, which
     can be used to resume normal parsing, by supplying this checkpoint as an
     argument to [offer].

     This function should be used with some care. It could "mess up the
     lookahead" in the sense that it allows parsing to resume in an arbitrary
     state [s] with an arbitrary lookahead symbol [t], even though Menhir's
     reachability analysis (menhir --list-errors) might well think that it is
     impossible to reach this particular configuration. If one is using
     Menhir's new error reporting facility, this could cause the parser to
     reach an error state for which no error message has been prepared. *)
  val input_needed: 'a env -> 'a checkpoint

end

(**This signature is a fragment of the inspection API that is made available
   to the user when [--inspection] is used. This fragment contains type
   definitions for symbols. *)
module type SYMBOLS = sig

  (**The type ['a terminal] represents a terminal symbol. Its parameter ['a]
     represents the type of the semantic values associated with this symbol.
     The concrete definitions of this type is generated. *)
  type 'a terminal

  (**The type ['a nonterminal] represents a nonterminal symbol. Its parameter
     ['a] represents the type of the semantic values associated with this
     symbol. The concrete definitions of this type is generated. *)
  type 'a nonterminal

  (**The type ['a symbol] represents a terminal or nonterminal symbol. It is
     the disjoint union of the types ['a terminal] and ['a nonterminal]. *)
  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  (**The type [xsymbol] is an existentially quantified version of the type ['a
     symbol]. This type is useful in situations where ['a] is not statically
     known. *)
  type xsymbol =
    | X : 'a symbol -> xsymbol

end

(**This signature describes the inspection API that is made available to the
   user when [--inspection] is used. *)
module type INSPECTION = sig

  (* The types of symbols are described above. *)
  include SYMBOLS

  (**The type ['a lr1state] is meant to be the same as in {!INCREMENTAL_ENGINE}. *)
  type 'a lr1state

  (**The type [production] is meant to be the same as in {!INCREMENTAL_ENGINE}.
     It represents a production of the grammar. A production can be examined
     via the functions {!lhs} and {!rhs} below. *)
  type production

  (**An LR(0) item is a pair of a production [prod] and a valid index [i] into
     this production. That is, if the length of [rhs prod] is [n], then [i] is
     comprised between 0 and [n], inclusive. *)
  type item =
      production * int

  (** The following are total ordering functions. *)

  val compare_terminals: _ terminal -> _ terminal -> int
  val compare_nonterminals: _ nonterminal -> _ nonterminal -> int
  val compare_symbols: xsymbol -> xsymbol -> int
  val compare_productions: production -> production -> int
  val compare_items: item -> item -> int

  (**[incoming_symbol s] is the incoming symbol of the state [s], that is,
     the symbol that the parser must recognize before (has recognized when)
     it enters the state [s]. This function gives access to the semantic
     value [v] stored in a stack element [Element (s, v, _, _)]. Indeed,
     by case analysis on the symbol [incoming_symbol s], one discovers the
     type ['a] of the value [v]. *)
  val incoming_symbol: 'a lr1state -> 'a symbol

  (**[items s] is the set of the LR(0) items in the LR(0) core of the LR(1)
     state [s]. This set is not epsilon-closed. This set is presented as a
     list, in an arbitrary order. *)
  val items: _ lr1state -> item list

  (**[lhs prod] is the left-hand side of the production [prod]. This is
     always a non-terminal symbol. *)
  val lhs: production -> xsymbol

  (**[rhs prod] is the right-hand side of the production [prod]. This is
     a (possibly empty) sequence of (terminal or nonterminal) symbols. *)
  val rhs: production -> xsymbol list

  (**[nullable nt] tells whether the non-terminal symbol [nt] is nullable.
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)
  val nullable: _ nonterminal -> bool

  (**[first nt t] tells whether the FIRST set of the nonterminal symbol [nt]
     contains the terminal symbol [t]. That is, it is true if and only if
     [nt] produces a word that begins with [t]. *)
  val first: _ nonterminal -> _ terminal -> bool

  (**[xfirst] is analogous to [first], but expects a first argument of type
     [xsymbol] instead of [_ terminal]. *)
  val xfirst: xsymbol -> _ terminal -> bool

  (**[foreach_terminal] enumerates the terminal symbols, including [error]. *)
  val foreach_terminal:           (xsymbol -> 'a -> 'a) -> 'a -> 'a

  (**[foreach_terminal_but_error] enumerates the terminal symbols, excluding
     [error]. *)
  val foreach_terminal_but_error: (xsymbol -> 'a -> 'a) -> 'a -> 'a

  (**The type [env] is meant to be the same as in {!INCREMENTAL_ENGINE}. *)
  type 'a env

  (**[feed symbol startp semv endp env] causes the parser to consume the
     (terminal or nonterminal) symbol [symbol], accompanied with the semantic
     value [semv] and with the start and end positions [startp] and [endp].
     Thus, the automaton makes a transition, and reaches a new state. The
     stack grows by one cell. This operation is permitted only if the current
     state (as determined by [env]) has an outgoing transition labeled with
     [symbol]. Otherwise, [Invalid_argument _] is raised. *)
  val feed: 'a symbol -> position -> 'a -> position -> 'b env -> 'b env

end

(**This signature combines the incremental API and the inspection API. *)
module type EVERYTHING = sig

  include INCREMENTAL_ENGINE

  include INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a env := 'a env

end
end
module EngineTypes : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module defines several types and module types that are used in the
   specification of the module {!Engine}. *)

(* --------------------------------------------------------------------------- *)

(* It would be nice if we could keep the structure of stacks and environments
   hidden. However, stacks and environments must be accessible to semantic
   actions, so the following data structure definitions must be public. *)

(* --------------------------------------------------------------------------- *)

(**A stack is a linked list of cells. A sentinel cell -- which is its own
   successor -- is used to mark the bottom of the stack. The sentinel cell
   itself is not significant -- it contains dummy values. *)
type ('state, 'semantic_value) stack = {

  state: 'state;
  (**The state that we should go back to if we pop this stack cell.

     This convention means that the state contained in the top stack cell is
     not the current state [env.current]. It also means that the state found
     within the sentinel is a dummy -- it is never consulted. This convention
     is the same as that adopted by the code-based back-end. *)

  semv: 'semantic_value;
  (**The semantic value associated with the chunk of input that this cell
     represents. *)

  startp: Lexing.position;
  (**The start position of the chunk of input that this cell represents. *)

  endp: Lexing.position;
  (**The end position of the chunk of input that this cell represents. *)

  next: ('state, 'semantic_value) stack;
  (**The next cell down in the stack. If this is a self-pointer, then this
     cell is the sentinel, and the stack is conceptually empty. *)

}

(* --------------------------------------------------------------------------- *)

(**A parsing environment contains all of the parser's state (except for the
   current program point). *)
type ('state, 'semantic_value, 'token) env = {

  error: bool;
  (**If this flag is true, then the first component of [env.triple] should
     be ignored, as it has been logically overwritten with the [error]
     pseudo-token. *)

  triple: 'token * Lexing.position * Lexing.position;
  (**The last token that was obtained from the lexer, together with its start
     and end positions. Warning: before the first call to the lexer has taken
     place, a dummy (and possibly invalid) token is stored here. *)

  stack: ('state, 'semantic_value) stack;
  (**The stack. *)

  current: 'state;
  (**The current state. *)

}

(* --------------------------------------------------------------------------- *)

(**A number of logging hooks are used to (optionally) emit logging messages. *)
module type LOG = sig

  type state
  type terminal
  type production

  (* The comments below indicate the conventional messages that correspond to
     these hooks. *)

  (* State %d: *)

  val state: state -> unit

  (* Shifting (<terminal>) to state <state> *)

  val shift: terminal -> state -> unit

  (* Reducing a production should be logged either as a reduction
     event (for regular productions) or as an acceptance event (for
     start productions). *)

  (* Reducing production <production> / Accepting *)

  val reduce_or_accept: production -> unit

  (* Lookahead token is now <terminal> (<pos>-<pos>) *)

  val lookahead_token: terminal -> Lexing.position -> Lexing.position -> unit

  (* Initiating error handling *)

  val initiating_error_handling: unit -> unit

  (* Resuming error handling *)

  val resuming_error_handling: unit -> unit

  (* Handling error in state <state> *)

  val handling_error: state -> unit

end

(* --------------------------------------------------------------------------- *)

(**This signature describes the parameters that must be supplied to the LR
   engine. *)
module type TABLE = sig

  (**The type of automaton states. *)
  type state

  (**States are numbered. *)
  val number: state -> int

  (**The type of tokens. These can be thought of as real tokens, that is,
     tokens returned by the lexer. They carry a semantic value. This type
     does not include the [error] pseudo-token. *)
  type token

  (**The type of terminal symbols. These can be thought of as integer codes.
     They do not carry a semantic value. This type does include the [error]
     pseudo-token. *)
  type terminal

  (**The type of nonterminal symbols. *)
  type nonterminal

  (**The type of semantic values. *)
  type semantic_value

  (**A token is conceptually a pair of a (non-[error]) terminal symbol and a
     semantic value. The function [token2terminal] is the first the pair
     projection. *)
  val token2terminal: token -> terminal

  (**A token is conceptually a pair of a (non-[error]) terminal symbol and a
     semantic value. The function [token2value] is the second the pair
     projection. *)
  val token2value: token -> semantic_value

  (* Even though the [error] pseudo-token is not a real token, it is a
     terminal symbol. Furthermore, for regularity, it must have a semantic
     value. *)
  (**The terminal symbol associated with the [error] token. *)
  val error_terminal: terminal

  (**The semantic value associated with the [error] token. *)
  val error_value: semantic_value

  (**[foreach_terminal] iterates over all terminal symbols. *)
  val foreach_terminal: (terminal -> 'a -> 'a) -> 'a -> 'a

  (**The type of productions. *)
  type production

  (**[production_index] maps a production to its integer index. *)
  val production_index: production -> int

  (**[find_production] maps a production index to a production.
     Its argument must be a valid index; use with care. *)
  val find_production: int -> production

  (**If a state [s] has a default reduction on production [prod], then, upon
     entering [s], the automaton should reduce [prod] without consulting the
     lookahead token.

     [default_reduction s] determines whether the state [s] has a default
     reduction. Instead of returning a value of a sum type -- say, either
     [DefRed prod] or [NoDefRed] -- it accepts two continuations, and invokes
     just one of them. *)
  val default_reduction:
    state ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (**An LR automaton can normally take three kinds of actions: shift, reduce,
     or fail. (Acceptance is a particular case of reduction: it consists in
     reducing a start production.)

     There are two variants of the shift action. [shift/discard s] instructs
     the automaton to discard the current token, request a new one from the
     lexer, and move to state [s]. [shift/nodiscard s] instructs it to move to
     state [s] without requesting a new token. This instruction should be used
     when [s] has a default reduction on [#].

     The function [action] provides access to the automaton's action table. It
     maps a pair of a state and a terminal symbol to an action.

     Instead of returning a value of a sum type -- one of shift/discard,
     shift/nodiscard, reduce, or fail -- this function accepts three
     continuations, and invokes just one them.

     The parameters of the function [action] are as follows:

     - the first two parameters, a state and a terminal symbol, are used to
       look up the action table;

     - the next parameter is the semantic value associated with the above
       terminal symbol; it is not used, only passed along to the shift
       continuation, as explained below;

     - the shift continuation expects an environment; a flag that tells
       whether to discard the current token; the terminal symbol that
       is being shifted; its semantic value; and the target state of
       the transition;

     - the reduce continuation expects an environment and a production;

     - the fail continuation expects an environment;

     - the last parameter is the environment; it is not used, only passed
       along to the selected continuation. *)
  val action:
    state ->
    terminal ->
    semantic_value ->
    ('env -> bool -> terminal -> semantic_value -> state -> 'answer) ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (**[maybe_shift_t s t] determines whether there exists a transition out of
     the state [s], labeled with the terminal symbol [t], to some state
     [s']. If so, it returns [Some s']. Otherwise, it returns [None]. *)
  val maybe_shift_t : state -> terminal -> state option

  (**[may_reduce_prod s t prod] determines whether in the state [s], with
     lookahead symbol [t], the automaton reduces production [prod]. This test
     accounts for the possible existence of a default reduction. *)
  val may_reduce_prod : state -> terminal -> production -> bool

  (**The function [goto_nt] provides access to the automaton's goto table. It
     maps a pair of a state [s] and a nonterminal symbol [nt] to a state. The
     function call [goto_nt s nt] is permitted ONLY if the state [s] has an
     outgoing transition labeled [nt]. Otherwise, its result is undefined. *)
  val goto_nt : state -> nonterminal -> state

  (**The function [goto_prod] also provides access to the goto table. It maps
     a pair of a production [prod] and a state [s] to a state. The call
     [goto_prod prod s] is permitted ONLY if the state [s] has an outgoing
     transition labeled with the nonterminal symbol [lhs prod]. *)
  val       goto_prod: state -> production  -> state

  (**The function [maybe_goto_nt] serves the same purpose as [goto_nt].
     Compared to [goto_nt], it involves an additional dynamic check, so it CAN
     be called even the state [s] has no outgoing transition labeled [nt]. *)
  val maybe_goto_nt:   state -> nonterminal -> state option

  (**[lhs prod] returns the left-hand side of production [prod],
     a nonterminal symbol. *)
  val lhs: production -> nonterminal

  (**[is_start prod] tells whether the production [prod] is a start
     production. *)
  val is_start: production -> bool

  (**A semantic action can raise the exception [Error]. *)
  exception Error

  (**By convention, a semantic action is responsible for:

     1. fetching whatever semantic values and positions it needs off the stack;

     2. popping an appropriate number of cells off the stack, as dictated
        by the length of the right-hand side of the production;

     3. computing a new semantic value, as well as new start and end positions;

     4. pushing a new stack cell, which contains the three values
        computed in step 3;

     5. returning the new stack computed in steps 2 and 4.  *)
  type semantic_action =
      (state, semantic_value, token) env -> (state, semantic_value) stack

  (* Point 1 above is essentially forced upon us: if semantic values were
     fetched off the stack by this interpreter, then the calling convention
     for semantic actions would be variadic: not all semantic actions would
     have the same number of arguments. The rest follows rather naturally. *)

  (**The function [semantic_action] maps a production to its semantic action. *)
  val semantic_action: production -> semantic_action

  (**[may_reduce state prod] tests whether the state [state] is capable of
     reducing the production [prod]. This function is currently costly and
     is not used by the core LR engine. It is used in the implementation
     of certain functions, such as [force_reduction], which allow the engine
     to be driven programmatically. *)
  val may_reduce: state -> production -> bool

  (**If the flag [log] is false, then the logging functions are not called.
     If it is [true], then they are called. *)
  val log : bool

  (**The logging hooks required by the LR engine. *)
  module Log : LOG
    with type state := state
     and type terminal := terminal
     and type production := production

end

(* --------------------------------------------------------------------------- *)

(**This signature describes the monolithic (traditional) LR engine. When the
   engine is used in this mode, the parser controls the lexer. *)
module type MONOLITHIC_ENGINE = sig

  type state

  type token

  type semantic_value

  exception Error

  (**An entry point to the engine requires a start state, a lexer, and a
     lexing buffer. It either succeeds and produces a semantic value, or fails
     and raises {!Error}. *)
  val entry:
    (* strategy: *) [ `Legacy | `Simplified ] -> (* see [IncrementalEngine] *)
    state ->
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    semantic_value

end

(* --------------------------------------------------------------------------- *)

(**This signature describes just the entry point of the incremental LR engine.
   It is a supplement to {!IncrementalEngine.INCREMENTAL_ENGINE}.

   The [start] function is set apart because we do not wish to publish it as
   part of the generated file  [parser.mli]. Instead, the table back-end will
   publish specialized versions of it, with a suitable type cast. *)
module type INCREMENTAL_ENGINE_START = sig

  type state
  type semantic_value
  type 'a checkpoint

  (**[start] is an entry point. It requires a start state and a start position
     and begins the parsing process. If the lexer is based on an OCaml lexing
     buffer, the start position should be [lexbuf.lex_curr_p]. [start] produces
     a checkpoint, which usually will be an [InputNeeded] checkpoint. (It could
     be [Accepted] if this starting state accepts only the empty word. It could
     be [Rejected] if this starting state accepts no word at all.) It does not
     raise any exception.

     [start s pos] should really produce a checkpoint of type ['a checkpoint],
     for a fixed ['a] that depends on the state [s]. We cannot express this, so
     we use [semantic_value checkpoint], which is safe. The table back-end uses
     [Obj.magic] to produce safe specialized versions of [start]. *)
  val start:
    state ->
    Lexing.position ->
    semantic_value checkpoint

end

(* --------------------------------------------------------------------------- *)

(**This signature describes the LR engine, which combines the monolithic
   and incremental interfaces. *)
module type ENGINE = sig

  include MONOLITHIC_ENGINE

  include IncrementalEngine.INCREMENTAL_ENGINE
    with type token := token
     and type 'a lr1state = state (* useful for us; hidden from the end user *)

  include INCREMENTAL_ENGINE_START
    with type state := state
     and type semantic_value := semantic_value
     and type 'a checkpoint := 'a checkpoint

end
end
module Engine : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open EngineTypes

(**The LR parsing engine. *)
module Make (T : TABLE)
: ENGINE
  with type state = T.state
   and type token = T.token
   and type semantic_value = T.semantic_value
   and type production = T.production
   and type 'a env = (T.state, T.semantic_value, T.token) EngineTypes.env

(* We would prefer not to expose the definition of the type [env].
   However, it must be exposed because some of the code in the
   inspection API needs access to the engine's internals; see
   [InspectionTableInterpreter]. Everything would be simpler if
   --inspection was always ON, but that would lead to bigger parse
   tables for everybody. *)
end
module ErrorReports : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Lexing

(**This module offers functions that can help produce syntax error messages. *)

(* -------------------------------------------------------------------------- *)

(**A two-place buffer of type ['a buffer] holds zero, one, or two elements of
   type ['a]. Some of the functions below keep track of the start and end
   positions of the last two tokens in a two-place buffer. This helps nicely
   show where a syntax error took place. *)
type 'a buffer

(**[wrap lexer] returns a pair of a new (initially empty) buffer and a lexer
   which internally relies on [lexer] and updates [buffer] on the fly whenever
   a token is demanded.

   The type of the buffer is [(position * position) buffer], which means that
   it stores two pairs of positions, which are the start and end positions of
   the last two tokens.

   The type of the lexer is [lexbuf -> 'token]. The start and end positions
   of each token are read from [lexbuf]. *)
val wrap:
  ((lexbuf -> 'token) as 'lexer) ->
  (position * position) buffer * 'lexer

(**[wrap_supplier] is analogous to {!wrap}, except the type of the lexer is
   [unit -> 'token * position * position]. No [lexbuf] is involved. *)
val wrap_supplier:
  ((unit -> 'token * position * position) as 'lexer) ->
  (position * position) buffer * 'lexer

(**[show f buffer] prints the contents of the buffer, producing a string that
   is typically of the form "after '%s' and before '%s'". The function [f] is
   used to print an element. The buffer MUST be nonempty. *)
val show: ('a -> string) -> 'a buffer -> string

(**[last buffer] returns the last element of the buffer. The buffer MUST be
   nonempty. *)
val last: 'a buffer -> 'a

(* -------------------------------------------------------------------------- *)

(**[extract text (pos1, pos2)] extracts the sub-string of [text] delimited
   by the positions [pos1] and [pos2]. *)
val extract: string -> position * position -> string

(**[sanitize text] eliminates any special characters from the text [text].
   A special character is a character whose ASCII code is less than 32.
   Every special character is replaced with a single space character. *)
val sanitize: string -> string

(**[compress text] replaces every run of at least one whitespace character
   with exactly one space character. *)
val compress: string -> string

(**[shorten k text] limits the length of [text] to [2k+3] characters. If the
   text is too long, a fragment in the middle is replaced with an ellipsis. *)
val shorten: int -> string -> string

(**[expand f text] searches [text] for occurrences of [$k], where [k]
   is a nonnegative integer literal, and replaces each such occurrence
   with the string [f k]. *)
val expand: (int -> string) -> string -> string
end
module LexerUtil : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Lexing

(**[init filename lexbuf] initializes the lexing buffer [lexbuf] so
   that the positions that are subsequently read from it refer to the
   file [filename]. It returns [lexbuf]. *)
val init: string -> lexbuf -> lexbuf

(**[read filename] reads the entire contents of the file [filename] and
   returns a pair of this content (a string) and a lexing buffer that
   has been initialized, based on this string. *)
val read: string -> string * lexbuf

(**[newline lexbuf] increments the line counter stored within [lexbuf]. It
   should be invoked by the lexer itself every time a newline character is
   consumed. This allows maintaining a current the line number in [lexbuf].
   It is synonymous with [Lexing.new_line lexbuf]. *)
val newline: lexbuf -> unit

(**[range (startpos, endpos)] prints a textual description of the range
   delimited by the start and end positions [startpos] and [endpos].
   This description is one line long and ends in a newline character.
   This description mentions the file name, the line number, and a range
   of characters on this line. The line number is correct only if [newline]
   has been correctly used, as described dabove. *)
val range: position * position -> string

(**[tabulate is_eof lexer] tabulates the lexer [lexer]: that is, it
   immediately runs this lexer all the way until an EOF token is found, stores
   the tokens in an array in memory, and returns a new lexer which (when
   invoked) reads tokens from this array. The function [lexer] is not allowed
   to raise an exception, and must produce a finite stream of tokens: that is,
   after a finite number of invocations, it must return a token that is
   identified by the function [is_eof] as an EOF token.

   Both the existing lexer [lexer] and the new lexer returned by [tabulate
   is_eof lexer] are functions of type [unit -> 'a], where the type ['a] is
   likely to be instantiated with a triple of a token and two positions, as
   per the revised lexer API described in the module {!Convert}. *)
val tabulate: ('a -> bool) -> (unit -> 'a) -> (unit -> 'a)
end
module Printers : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module offers printers for several of the data structures involved in
   the incremental API. It is considered unstable and may be modified or
   removed in the future. *)
module Make

  (I : IncrementalEngine.EVERYTHING)

  (User : sig

    (**[print s] is supposed to send the string [s] to some output channel. *)
    val print: string -> unit

    (**[print_symbol s] is supposed to print a representation of the symbol [s]. *)
    val print_symbol: I.xsymbol -> unit

    (**[print_element e] is supposed to print a representation of the element [e].
       This function is optional; if it is not provided, [print_element_as_symbol]
       (defined below) is used instead. *)
    val print_element: (I.element -> unit) option

  end)

: sig

  open I

  (**Printing a list of symbols. *)
  val print_symbols: xsymbol list -> unit

  (**Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)
  val print_element_as_symbol: element -> unit

  (**Printing a stack as a list of elements. This function needs an element
     printer. It uses [print_element] if provided by the user; otherwise
     it uses [print_element_as_symbol]. (Ending with a newline.) *)
  val print_stack: 'a env -> unit

  (**Printing an item. (Ending with a newline.) *)
  val print_item: item -> unit

  (**Printing a production. (Ending with a newline.) *)
  val print_production: production -> unit

  (**Printing the current LR(1) state. The current state is first displayed
     as a number; then the list of its LR(0) items is printed. (Ending with
     a newline.) *)
  val print_current_state: 'a env -> unit

  (**Printing a summary of the stack and current state. This function just
     calls [print_stack] and [print_current_state] in succession. *)
  val print_env: 'a env -> unit

end
end
module PackedIntArray : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module allows packing an array of (small) integers inside a string,
   using less than one word of memory per array element. *)

(**A packed integer array is represented as a pair of an integer [k] and
   a string [s]. The integer [k] is the number of bits per integer that we
   use. The string [s] is just an array of bits, which is read in 8-bit
   chunks.

   The OCaml programming language treats string literals and array literals
   in slightly different ways: the former are statically allocated, while
   the latter are dynamically allocated. (This is rather arbitrary.) In the
   context of Menhir's table-based back-end, where compact, immutable
   integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)
type t =
  int * string

(**[pack a] turns an array of integers into a packed integer array.

   Because the sign bit is the most significant bit, the magnitude of any
   negative number is the word size. In other words, as soon as the array
   [a] contains any negative numbers, [pack] does not achieve any space
   savings. *)
val pack: int array -> t

(* [get] is now commented out, as it is no longer used.
(**[get t i] returns the integer stored in the packed array [t] at index [i].
   Together, [pack] and [get] satisfy the following property: if the index [i]
   is within bounds, then [get (pack a) i] equals [a.(i)]. *)
val get: t -> int -> int
 *)

(**[get1 s i] returns the integer stored in the packed array [s] at index [i].
   It assumes (and does not check) that the array's bit width is [1]. The
   packed array [s] is just a string; there is no bit width component.
   In other words, it is [get], specialized to the bit width 1. *)
val get1: string -> int -> int

(**[get2] is [get], specialized to the bit width 2. *)
val get2: string -> int -> int

(**[get4] is [get], specialized to the bit width 4. *)
val get4: string -> int -> int

(**[get8] is [get], specialized to the bit width 8. *)
val get8: string -> int -> int

(**[get16] is [get], specialized to the bit width 16. *)
val get16: string -> int -> int

(**[get32] is [get], specialized to the bit width 32. *)
val get32: string -> int -> int
end
module RowDisplacementDecode : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**Row displacement aims to compress a two-dimensional table where some values
   are considered insignificant. *)

(* This idea reportedly appears in Aho and Ullman's "Principles of Compiler
   Design" (1977). It is evaluated in Tarjan and Yao's "Storing a Sparse
   Table" (1979) and in Dencker, Dürre, and Heuft's "Optimization of Parser
   Tables for Portable Compilers" (1984). *)

(* We place the encoding functions and the decoding functions in two distinct
   modules. This is the decoding module. It is part of the runtime support
   library MenhirLib. *)

(**A displacement is a nonnegative integer, which, once decoded in a certain
   way, represents a possibly negative offset into a data array. *)
type displacement =
  int

(**A compressed table is represented as a pair of a displacement array and a
   data array. If the functions [get_displacement] and [get_data] offer read
   access to these arrays, then [get get_displacement get_data i j] returns
   the value found at indices [i] and [j] in the compressed table. This call
   is permitted only if the value found at indices [i] and [j] in the original
   table is significant. *)
val get:
  ('displacement -> int -> displacement) ->
  ('data -> int -> 'a) ->
  'displacement * 'data ->
  int -> int ->
  'a

(**The auxiliary function [decode] is part of the implementation of [get].
   It is exposed because it is used by the specialized versions of [get]
   that the table back-end generates. See [TableUtils]. *)
val decode: displacement -> int
end
module LinearizedArray : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module offers support for linearizing an array of arrays (of possibly
   different lengths). All of the data is placed in a single data array, which
   is constructed by concatenating all of the little arrays. An entry array,
   which contains offsets into the data array, is also constructed. *)
type 'a t =
  (* data: *)   'a array *
  (* entry: *) int array

(**[make a] turns the array of arrays [a] into a linearized array. *)
val make: 'a array array -> 'a t

(**[read la i j] reads the linearized array [la] at indices [i] and [j].
   Thus, [read (make a) i j] is equivalent to [a.(i).(j)]. *)
val read: 'a t -> int -> int -> 'a

(**[write la i j v] writes the value [v] into the linearized array [la]
   at indices [i] and [j]. *)
val write: 'a t -> int -> int -> 'a -> unit

(**[length la] is the number of rows of the array [la].
   Thus, [length (make a)] is equivalent to [Array.length a]. *)
val length: 'a t -> int

(**[row_length la i] is the length of the row at index [i] in the linearized
   array [la].
   Thus, [row_length (make a) i] is equivalent to [Array.length a.(i)]. *)
val row_length: 'a t -> int -> int

(**[read_row la i] reads the row at index [i], producing a list. Thus,
   [read_row (make a) i] is equivalent to [Array.to_list a.(i)]. *)
val read_row: 'a t -> int -> 'a list

(* The following variant reads the linearized array via accessors
   [get_data : int -> 'a] and [get_entry : int -> int]. *)

(**Provided the accessor functions [get_data] and [get_entry] offers read
   access to the data and entry arrays of the linearized array [la],
   [read_row_via get_data get_entry i] reads a row at index [i]
   in the linearized array [la]. *)
val read_row_via:
  (* get_data: *)  (int -> 'a) ->
  (* get_entry: *) (int -> int) ->
  (* i: *)         int ->
                   'a list
end
module TableFormat : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This signature defines the format of the parse tables.
   It is used as an argument to [TableInterpreter.Make]. *)
module type TABLES = sig

  (**The type of tokens. *)
  type token

  (**[terminal_count] is the number of terminal symbols, without [#]. *)
  val terminal_count: int

  (**[token2terminal] maps a token to a terminal symbol, represented
     by its internal integer code. *)
  val token2terminal: token -> int

  (**[error_terminal] is the integer code of the special token [error]. *)
  val error_terminal: int

  (**[token2value] maps a token to its semantic value. *)
  val token2value: token -> Obj.t

  (**Traditionally, an LR automaton is described by two tables, namely, an
     action table and a goto table. See, for instance, the Dragon book.

     The action table is a two-dimensional matrix that maps a state and a
     lookahead token to an action. An action is one of: shift to a certain
     state, reduce a certain production, accept, or fail.

     The goto table is a two-dimensional matrix that maps a state and a
     non-terminal symbol to either a state or undefined. By construction, this
     table is sparse: its undefined entries are never looked up. A compression
     technique is free to overlap them with other entries.

     In Menhir, things are slightly different. If a state has a default
     reduction on token [#], then that reduction must be performed without
     consulting the lookahead token. As a result, we must first determine
     whether that is the case, before we can obtain a lookahead token and use
     it as an index in the action table.

     Thus, Menhir's tables are as follows. *)

  (**The default reduction table, a one-dimensional table, maps a state to
     either ``no default reduction'' (encoded as: 0) or ``by default, reduce
     prod'' (encoded as: 1 + prod). The action table is looked up only when
     there is no default reduction. *)
  val default_reduction: int -> int

  (**Menhir follows Dencker, Dürre and Heuft, who point out that, although the
     action table is not sparse by nature (i.e., the error entries are
     significant), it can be made sparse by first factoring out a binary error
     matrix, then replacing the error entries in the action table with
     undefined entries. Thus: *)

  (**The error bitmap, a two-dimensional table, maps a state and a terminal
     symbol to either "fail" (encoded as: 0) or "do not fail" (encoded as: 1).
     The action table is looked up only in the latter case.

     The function [error] offers read access to the error bitmap.

     The error bitmap does not contain a column for the [#] pseudo-terminal.
     Thus, its width is [terminal_count]. *)
  val error: int -> int -> int

  (**The action table, a two-dimensional table, maps a state and a terminal
     to one of ``shift to state s and discard the current token'' (encoded
     as: [s | 10]), ``shift to state s without discarding the current token''
     (encoded as: [es | 11]), or ``reduce prod'' (encoded as: [prod | 01]).

     Like the error bitmap, the action table does not contain a column for the
     [#] pseudo-terminal. *)
  val action: int -> int -> int

  (**A one-dimensional table, [lhs], maps a production to its left-hand side
     (a non-terminal symbol). *)
  val lhs: int -> int

  (**The goto table, a two-dimensional table, maps a state and a non-terminal
     symbol to either undefined (encoded as: 0) or a new state s (encoded as:
     1 + s).. *)
  val goto: int -> int -> int

  (**[start] is the number of start productions. A production [prod] is a
     start production if and only if [prod < start] holds. This is also the
     number of start symbols. A nonterminal symbol [nt] is a start symbol if
     and only if [nt < start] holds. *)
  val start: int

  (**The semantic action table, a one-dimensional table, maps productions to
     semantic actions. The calling convention for semantic actions is
     described in [EngineTypes]. This table contains ONLY NON-START
     PRODUCTIONS, so the indexing is off by [start]. Be careful. *)
  val semantic_action: ((int, Obj.t, token) EngineTypes.env ->
                        (int, Obj.t)        EngineTypes.stack) array

  (**The exception [Error] can be raised by semantic actions, caught by the
     engine, and raised again by the engine for the final user to observe. *)
  exception Error

  (**[trace] indicates whether a trace should be generated. Generating a trace
     requires two extra tables, which respectively map a terminal symbol and a
     production to a string. *)
  val trace: (string array * string array) option

end
end
module InspectionTableFormat : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This signature defines the format of the tables that are produced (in
   addition to the tables described in [TableFormat]) when the command line
   switch [--inspection] is enabled. It is used as an argument to
   {!InspectionTableInterpreter.Make}. *)
module type TABLES = sig

  (* The types of symbols. *)
  include IncrementalEngine.SYMBOLS

  (**The type ['a lr1state] describes an LR(1) state. The generated parser
     defines it internally as [int]. *)
  type 'a lr1state

  (**Some of the tables that follow use encodings of (terminal and
     nonterminal) symbols as integers. So, we need functions that
     map the integer encoding of a symbol to its algebraic encoding. *)

  (**[terminal] maps an integer code for a terminal symbol to a (terminal)
     symbol. *)
  val    terminal: int -> xsymbol

  (**[nonterminal] maps an integer code for a nonterminal symbol to a
     (nonterminal) symbol. *)
  val nonterminal: int -> xsymbol

  (**The left-hand side of every production already appears in the
     signature [TableFormat.TABLES], so we need not repeat it here. *)

  (**The table [rhs] provides access to the right-hand side of every
     production. The encoding of symbols as integers in described in
     [TableBackend]. *)
  val rhs: int -> int list

  (**A mapping of every (non-initial) state to its LR(0) core. *)
  val lr0_core: int -> int

  (**A mapping of every LR(0) state to its set of LR(0) items. Each item is
     represented in its packed form (see [Item]) as an integer. *)
  val lr0_items: int -> int list

  (**A mapping of every LR(0) state to its incoming symbol, if it has one. *)
  val lr0_incoming: int -> int

  (**A table that tells which non-terminal symbols are nullable. *)
  val nullable: int -> int (* 0 or 1 *)

  (**A two-table dimensional table, indexed by a nonterminal symbol and
     by a terminal symbol (other than [#]), encodes the FIRST sets. *)
  val first: int -> int -> int (* 0 or 1 *)

end
end
module InspectionTableInterpreter : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This functor is invoked inside the generated parser, in [--table] mode. It
   produces no code! It simply constructs the types [symbol] and [xsymbol] on
   top of the generated types [terminal] and [nonterminal]. *)
module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end)

: IncrementalEngine.SYMBOLS
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal

(**This functor is invoked inside the generated parser, in [--table] mode. It
   constructs the inspection API on top of the inspection tables described in
   [InspectionTableFormat]. *)
module Make
  (TT : TableFormat.TABLES)
  (IT : InspectionTableFormat.TABLES
        with type 'a lr1state = int)
  (ET : EngineTypes.TABLE
        with type terminal = int
         and type nonterminal = int
         and type semantic_value = Obj.t)
  (E : sig
     type 'a env = (ET.state, ET.semantic_value, ET.token) EngineTypes.env
   end)

: IncrementalEngine.INSPECTION
  with type 'a terminal := 'a IT.terminal
   and type 'a nonterminal := 'a IT.nonterminal
   and type 'a lr1state := 'a IT.lr1state
   and type production := int
   and type 'a env := 'a E.env
end
module TableInterpreter : sig
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module provides a thin decoding layer for the generated tables, thus
   providing an API that is suitable for use by [Engine.Make]. *)
module MakeEngineTable
  (T : TableFormat.TABLES)
: EngineTypes.TABLE
    with type state = int
     and type token = T.token
     and type semantic_value = Obj.t
     and type production = int
     and type terminal = int
     and type nonterminal = int

(* The exception [Error] is declared within the generated parser. This is
   preferable to pre-declaring it here, as it ensures that each parser gets
   its own, distinct [Error] exception. This is consistent with the code-based
   back-end. *)
end
module StaticVersion : sig
val require_20260209: unit
end
