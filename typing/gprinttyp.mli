(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(**
 This module provides function for printing type expressions as digraph using
    graphviz format. This is mostly aimed at providing a better representation
    of type expressions during debugging session.
*)
(**
A type node is printed as
{[
    .---------------.
    |           lvl |
    | <desc>  ID    |---->
    |               |--->
    .---------------.
]}
where the description part might be:
- a path: [list/8!]
- a type variable: ['name], [α], [β], [γ]
- [*] for tuples
- [→] for arrows type
- an universal type variable: [[β]∀], ['name ∀], ...
- [[mod X with ...]] for a first class module

- [∀] for a universal type binder

The more complex encoding for polymorphic variants and object types uses nodes
as head of the subgraph representing those types

- [[obj...]] for the head of an object subgraph
- [[Nil]] for the end of an object subgraph
- [[Row...]] for the head of a polymorphic variant subgraph

- [[Subst]] for a temporary substitution node

Then each nodes is relied by arrows to any of its children types.

- Type variables, universal type variables, [Nil], and [Subst] nodes don't have
  children.

- For tuples, the children types are the elements of the tuple. For instance,
  [int * float] is represented as
{[
  .------.   0     .-------.
  | *  1 |-------->| int! 2|
  .------.         .-------.
     |
     | 1
     v
   .----------.
   | float! 3 |
   .----------.
]}

- For arrows, the children types are the type of the argument and the result
  type. For instance, for [int -> float]:
{[
  .------.   0     .-------.
  | →  4 |-------->| int! 2|
  .------.         .-------.
     |
     | 1
     v
   .----------.
   | float! 3 |
   .----------.
]}

- For type constructor, like list the main children nodes are the argument
  types. For instance, [(int,float) result] is represented as:

{[
  .-------------.   0     .-------.
  | Result.t  5 |-------->| int! 2|
  .-------------.         .-------.
     |
     | 1
     v
   .----------.
   | float! 3 |
   .----------.
]}

Moreover, type abbreviations might be linked to the expanded nodes.
If I define: [type 'a pair = 'a * 'a], a type expression [int pair] might
correspond to the nodes:

{[
  .--------.   0    .--------.
  | pair 6 |------> | int! 2 |
  .--------.        .--------.
     ┆                  ^
     ┆ expand           |
     ┆                  |
  .------.   0 + 1      |
  | *  7 |------>-------.
  .------.
]}

- Universal type binders have two kind of children: bound variables,
  and the main body. For instance, ['a. 'a -> 'a] is represented as
{[

  .------.   bind    .-------.
  |  ∀ 8 |----------> | 𝛼 10 |
  .------.            .------.
     |                  ^
     |                  |
     v                  |
  .------.   0 + 1      |
  | →  9 |------>-------.
  .------.

]}

- [[Subst]] node are children are the type graph guarded by the
  substitution node, and an eventual link to the parent row variable.

- The children of first-class modules are the type expressions that may appear
  in the right hand side of constraints.
  For instance, [module M with type t = 'a and type u = 'b] is represented as
{[
  .----------------------.   0     .-----.
  | [mod M with t, u] 11 |-------->| 𝛼 12|
  .----------------------.         .-----
     |
     | 1
     v
   .------.
   | 𝛽 13 |
   .------.
]}


- The children of [obj] (resp. [row]) are the methods (resp. constructor) of the
  object type (resp. polymorphic variant). Each method is then linked to its
  type. To make them easier to read they are grouped inside graphviz cluster.
  For instance, [<a:int; m:'self; ..> as 'self] will be represented as:

{[

  .----------------.
  | .----------.    |
  | | [obj] 14 |<------<-----<-----.
  | .----------.    |              |
  |       ┆         |              |
  | .-------------. |    .------.  |    .-------.
  | | a public 15 |----->| ∀ 18 |----->| int! 2 |
  | .-------------. |    .------.  |    .-------.
  |        ┆        |              |
  | .-------------. |   .------.   |
  | | m public 16 |-----| ∀ 19 |>--|
  | .------------.  |   .------.
  |     ┆           |
  |     ┆ row var   |
  |     ┆           |
  |   .-------.     |
  |   | '_ 17 |     |
  |   .-------.     |
  .-----------------.

]}
*)

type digraph
(** Digraph with nodes, edges, hyperedges and subgraphes *)

type params
(** Various possible choices on how to represent types, see the {!params}
    functions for more detail.*)

type element
(** Graph element, see the {!node}, {!edge} and {!hyperedge} function *)

type decoration
(** Visual decoration on graph elements, see the {!Decoration} module.*)


val types: title:string -> params -> (decoration * Types.type_expr) list -> unit
(** Print a graph to the file
    [asprintf "%s/%04d-%s-%a.dot"
       dump_dir
       session_unique_id
       title
       pp_context context
    ]

 If the [dump_dir] flag is not set, the local directory is used.
 See the {!context} type on how and why to setup the context. *)

(** Full version of {!types} that allow to print any kind of graph element *)
val nodes: title:string -> params -> (decoration * element) list -> unit

val params:
  ?elide_links:bool ->
  ?expansion_as_hyperedge:bool ->
  ?short_ids:bool ->
  ?colorize:bool ->
  ?follow_expansions:bool ->
  unit -> params
(** Choice of details for printing type graphes:
    - if [elide_links] is [true] link nodes are not displayed (default:[true])
    - with [expansion_as_hyperedge], memoized constructor expansion are
    displayed as a hyperedge between the node storing the memoized expansion,
    the expanded node and the expansion (default:[false]).
    - with [short_ids], we use an independent counter for node ids, in order to
     have shorter ids for small digraphs (default:[true]).
    - with [colorize] nodes are colorized according to their typechecker ids
      (default:[true]).
    - with [follow_expansions], we add memoized type constructor expansions to
      the digraph (default:[true]).
*)

(** Update an existing [params] with new values. *)
val update_params:
  ?elide_links:bool ->
  ?expansion_as_hyperedge:bool ->
  ?short_ids:bool ->
  ?colorize:bool ->
  ?follow_expansions:bool ->
  params -> params

val node: Types.type_expr -> element
val edge: Types.type_expr -> Types.type_expr -> element

type dir = Toward | From
val hyperedge: (dir * decoration * Types.type_expr) list -> element
(** Edges between more than two elements. *)

(** {1 Node and decoration types} *)
module Decoration: sig
  type color =
    | Named of string
    | HSL of {h:float;s:float;l:float}

  val green: color
  val blue: color
  val red:color
  val purple:color
  val hsl: h:float -> s:float -> l:float -> color

  type style =
    | Filled of color option
    | Dotted
    | Dash

  type shape =
    | Ellipse
    | Circle
    | Diamond

  type property =
    | Color of color
    | Font_color of color
    | Style of style
    | Label of string list
    | Shape of shape
  val filled: color -> property
  val txt: string -> property
  val make: property list -> decoration
end

(** {1 Digraph construction and printing}*)

val make: params -> (decoration * element) list -> digraph
val add: params -> (decoration * element) list -> digraph -> digraph

(** add a subgraph to a digraph, only fresh nodes are added to the subgraph *)
val add_subgraph:
  params -> decoration -> (decoration * element) list -> digraph -> digraph

(** groups existing nodes inside a subgraph *)
val group_nodes: decoration * digraph -> digraph -> digraph

val pp: Format.formatter -> digraph -> unit


(** {1 Debugging helper functions } *)

(** {2 Generic print debugging function} *)

(** Conditional graph printing *)
val debug_on: (unit -> bool) ref

(** [debug_off f] switches off debugging before running [f]. *)
val debug_off: (unit -> 'a) -> 'a

(** [debug f] runs [f] when [!debug_on ()]*)
val debug: (unit -> unit) -> unit

(** {2 Node tracking functions }*)

(** [register_type (lbl,ty)] adds the type [t] to all graph printed until
    {!forget} is called *)
val register_type: decoration * Types.type_expr -> unit

(** [register_subgraph params tys] groups together all types reachable from
    [tys] at this point in printed digraphs, until {!forget} is called *)
val register_subgraph:
  params -> ?decoration:decoration -> Types.type_expr list -> unit

(** Forget all recorded context types *)
val forget : unit -> unit

(** {2 Contextual information}

  Those functions can be used to modify the filename of the generated digraphs.
  Use those functions to provide contextual information on a graph emitted
  during an execution trace.*)
type 'a context
val global: string context
val loc: Warnings.loc context
val set_context: 'a context -> 'a -> unit
val with_context: 'a context -> 'a -> (unit -> 'b) -> 'b
