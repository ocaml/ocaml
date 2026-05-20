(** The Runtime library is used by the generated programs.
    It defines a compact representation for the automaton in the form of simple
    bytecoded instructions and a sparse index. *)

(** At runtime, an lr1 state is just an integer *)
type lr1 = int

(** For the interpreter, a clause is also represented by an integer.
    When a clause matches, its id is returned and it is the duty of the
    (generated) client program to map it to a semantic action. *)
type clause = int

(** Registers are also named by small integers. *)
type register = int

type priority = int

(* Representation of the automaton as sparse tables and bytecoded programs *)

(** A sparse table stores many partial mapping from [0..k-1] to [0..v-1]
    (the set of keys and values) and is directly serialized to a string.

    The first two bytes are used to encode the size in bytes of keys and
    values. Possible values are 1, 2, 3, or 4.
    For instance, a table starting with "\x01\x02..." represents a table
    with one byte keys and two bytes values.

    The rest is just a sequence of cells composed of a key and a value.
    The size of a table is therefore always of the form:
      2 + (size_k + size_v) * n

    In the current implementation, keys are lr1 states and values are program
    counters.
*)
module Sparse_table : sig
  type row = int
  type col = int
  type value = int

  val get1 : string -> int -> int
  val get2 : string -> int -> int
  val get3 : string -> int -> int
  val get4 : string -> int -> int

  type t = {
    displacement: int -> int;
    offset: int;
    keys: int -> int;
    values: int -> int;
  }

  val lookup : t -> row -> col -> value option
end

(** The code of a program is a sequence of serialized [program_instruction] *)
type program_code = string

(** A [program_counter] is an offset in the [program_code] string.
    By construction it always point at the beginning of an instruction
    (otherwise, it is invalid). *)
type program_counter = int

(** The instructions of the bytecode language of the matcher *)
type program_instruction =
  | Store of register
    (** [Store r] stores the state at the top of the parser stack in
        register [r]. *)
  | Move of register * register
    (** [Move (src, dst)] sets register [dst] to the value in register [src]. *)
  | Swap of register * register
    (** [Swap (r1, r2)] exchanges the values of register [r1] and [r2]. *)
  | Clear of register
  | Yield of program_counter
    (** Jump and consume input:
        [Yield pc] stops the current interpretation to consume one state of the
        input stack. After consuming, execution should resume at [pc]. *)
  | Accept of clause * priority * register option array
    (** When reaching [Accept (clause, priority, captures)], the matcher found
        that clause number [clause] is matching at priority level [priority].
        Add it to the set of matching candidates. If [clause] already match,
        replace the match if [priority] is less than or equal to the previous
        level. Resume execution.
        [captures] defines the variables captured in the clause definition:
        [None] if it is unbound, [Some reg] if it is bound to the value stored
        in register [reg].
    *)
  | Match of Sparse_table.row
    (** [Match sidx] lookup the sparse table for a cell matching the state
        at the top of the parser stack at index [sidx].
        If the lookup is successful, it returns the [pc] should jump to.
        If unsuccesful, execution continue on next instruction.
    *)
  | Priority of clause * priority * priority
    (** [Priority (clause, p1, p2)] remaps the priority of a previous match.
      If [clause] matched at priority [p1], it should now be considered a match
      at priority [p2].*)
  | Halt
    (** Program is finished, there will be no more matches. *)

(** The type of values stored in a register.
    All registers are initialized with [Empty], and [Clear] set a register back
    to empty. [Value x] represents the capture of semantic value [x].
    [Initial] is a place holder that represents the initial frame of the
    parser's stack. There is no semantic value associated to it, but one can
    still refer to the position, which will be the initial position of the file
    being parsed. *)

type 'a register_value =
  | Empty
  | Location of Lexing.position * Lexing.position
  | Value of 'a

type 'a register_values = 'a register_value array

(** [program_step program pc] decodes the instruction at address [!pc] and
    increases [pc]. *)
val program_step : program_code -> program_counter ref -> program_instruction

(** All the elements composing an error matching program. *)
type program = {
  registers : int;
  initial : program_counter;
  table : Sparse_table.t;
  code : program_code;
}

(** A minimal module type mimicking [Menhir] incremental interface,
    suitable to run a matching program. *)
module type Parser = sig
  (** Represents the state of the parser (mostly a parsing stack) *)
  type 'a env

  (** The type of semantic values stored in stack frame *)
  type element

  (** Returns the current lr1 state (the state at the top of the stack) *)
  val current_state_number : 'a env -> int

  (** Returns the semantic value at the top of the stack, if any. *)
  val top : 'a env -> element option

  (** Returns the stack with the top frame removed,
      or [None] for an empty stack *)
  val pop : 'a env -> 'a env option

  (** Returns the starting and ending positions of the element at the top of the
      stack *)
  val positions: 'a env -> Lexing.position * Lexing.position
end

(** A matching candidate.
    An lrgrep program found that a clause matches with the a set of register
    values.
    However this might not be the clause with the highest priority.
    The action is not executed immediately and the candidate is saved for
    later.
    ['element] is the type of stack elements (semantic values) of the parser.
*)
type 'element candidate = clause * 'element register_values

(** Instantiate an interpreter for a parser representation *)
module Interpreter (P : Parser) : sig
  (** Run the program on a concrete parser, return the list of candidates. *)
  val lrgrep_run : program -> 'a P.env -> P.element candidate list
end
