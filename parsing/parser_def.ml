open Asttypes
open Parsetree
open Docstrings

(* Helper functions for desugaring array indexing operators *)
type paren_kind = Paren | Brace | Bracket

(* We classify the dimension of indices: Bigarray distinguishes
   indices of dimension 1,2,3, or more. Similarly, user-defined
   indexing operator behave differently for indices of dimension 1
   or more.
*)
type index_dim =
  | One
  | Two
  | Three
  | Many
type ('dot,'index) array_family = {

  name:
    Lexing.position * Lexing.position -> 'dot -> assign:bool -> paren_kind
  -> index_dim -> Longident.t Location.loc
  (*
    This functions computes the name of the explicit indexing operator
    associated with a sugared array indexing expression.

    For instance, for builtin arrays, if Clflags.unsafe is set,
    * [ a.[index] ]     =>  [String.unsafe_get]
    * [ a.{x,y} <- 1 ]  =>  [ Bigarray.Array2.unsafe_set]

    User-defined indexing operator follows a more local convention:
    * [ a .%(index)]     => [ (.%()) ]
    * [ a.![1;2] <- 0 ]  => [(.![;..]<-)]
    * [ a.My.Map.?(0) => [My.Map.(.?())]
  *);

  index:
    Lexing.position * Lexing.position -> paren_kind -> 'index
    -> index_dim * (arg_label * expression) list
   (*
     [index (start,stop) paren index] computes the dimension of the
     index argument and how it should be desugared when transformed
     to a list of arguments for the indexing operator.
     In particular, in both the Bigarray case and the user-defined case,
     beyond a certain dimension, multiple indices are packed into a single
     array argument:
     * [ a.(x) ]       => [ [One, [Nolabel, <<x>>] ]
     * [ a.{1,2} ]     => [ [Two, [Nolabel, <<1>>; Nolabel, <<2>>] ]
     * [ a.{1,2,3,4} ] => [ [Many, [Nolabel, <<[|1;2;3;4|]>>] ] ]
   *);

}

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_constraint: value_constraint option;
    lb_is_pun: bool;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }
