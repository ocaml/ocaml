(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)



(** Camlp4 signature repository *)

(** {6 Basic signatures} *)

(** Signature with just a type. *)
module type Type = sig
  type t;
end;

(** Signature for errors modules, an Error modules can be registred with
    the {!ErrorHandler.Register} functor in order to be well printed. *)
module type Error = sig
  type t;
  exception E of t;
  value to_string : t -> string;
  value print : Format.formatter -> t -> unit;
end;

(** A signature for extensions identifiers. *)
module type Id = sig

  (** The name of the extension, typically the module name. *)
  value name    : string;

  (** The version of the extension, typically $ Id$ with a versionning system. *)
  value version : string;

end;

(** A signature for warnings abstract from locations. *)
module Warning (Loc : Type) = struct
  module type S = sig
    type warning = Loc.t -> string -> unit;
    value default_warning : warning;
    value current_warning : ref warning;
    value print_warning   : warning;
  end;
end;

(** {6 Advanced signatures} *)

(** A signature for locations. *)
module type Loc = sig

  type t;

  (** Return a start location for the given file name.
      This location starts at the begining of the file. *)
  value mk : string -> t;

  (** The [ghost] location can be used when no location
      information is available. *)
  value ghost : t;

  (** {6 Conversion functions} *)

  (** Return a location where both positions are set the given position. *)
  value of_lexing_position : Lexing.position -> t;

  (** Return an OCaml location. *)
  value to_ocaml_location : t -> Camlp4_import.Location.t;

  (** Return a location from an OCaml location. *)
  value of_ocaml_location : Camlp4_import.Location.t -> t;

  (** Return a location from ocamllex buffer. *)
  value of_lexbuf : Lexing.lexbuf -> t;

  (** Return a location from [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
  value of_tuple : (string * int * int * int * int * int * int * bool) -> t;

  (** Return [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
  value to_tuple : t -> (string * int * int * int * int * int * int * bool);

  (** [merge loc1 loc2] Return a location that starts at [loc1] and end at [loc2]. *)
  value merge : t -> t -> t;

  (** The stop pos becomes equal to the start pos. *)
  value join : t -> t;

  (** [move selector n loc]
      Return the location where positions are moved.
      Affected positions are chosen with [selector].
      Returned positions have their character offset plus [n]. *)
  value move : [= `start | `stop | `both ] -> int -> t -> t;

  (** [shift n loc] Return the location where the new start position is the old
      stop position, and where the new stop position character offset is the
      old one plus [n]. *)
  value shift : int -> t -> t;

  (** [move_line n loc] Return the location with the old line count plus [n].
      The "begin of line" of both positions become the current offset. *)
  value move_line : int -> t -> t;

  (** {6 Accessors} *)

  (** Return the file name *)
  value file_name  : t -> string;

  (** Return the line number of the begining of this location. *)
  value start_line : t -> int;

  (** Return the line number of the ending of this location. *)
  value stop_line  : t -> int;

  (** Returns the number of characters from the begining of the file
      to the begining of the line of location's begining. *)
  value start_bol  : t -> int;

  (** Returns the number of characters from the begining of the file
      to the begining of the line of location's ending. *)
  value stop_bol   : t -> int;

  (** Returns the number of characters from the begining of the file
      of the begining of this location. *)
  value start_off  : t -> int;

  (** Return the number of characters from the begining of the file
      of the ending of this location. *)
  value stop_off   : t -> int;

  (** Return the start position as a Lexing.position. *)
  value start_pos  : t -> Lexing.position;

  (** Return the stop position as a Lexing.position. *)
  value stop_pos   : t -> Lexing.position;

  (** Generally, return true if this location does not come
      from an input stream. *)
  value is_ghost   : t -> bool;

  (** Return the associated ghost location. *)
  value ghostify   : t -> t;

  (** Return the location with the give file name *)
  value set_file_name : string -> t -> t;

  (** [strictly_before loc1 loc2] True if the stop position of [loc1] is
      strictly_before the start position of [loc2]. *)
  value strictly_before : t -> t -> bool;

  (** Return the location with an absolute file name. *)
  value make_absolute : t -> t;

  (** Print the location into the formatter in a format suitable for error
      reporting. *)
  value print : Format.formatter -> t -> unit;

  (** Print the location in a short format useful for debugging. *)
  value dump  : Format.formatter -> t -> unit;

  (** Same as {!print} but return a string instead of printting it. *)
  value to_string : t -> string;

  (** [Exc_located loc e] is an encapsulation of the exception [e] with
      the input location [loc]. To be used in quotation expanders
      and in grammars to specify some input location for an error.
      Do not raise this exception directly: rather use the following
      function [Loc.raise]. *)
  exception Exc_located of t and exn;

  (** [raise loc e], if [e] is already an [Exc_located] exception,
      re-raise it, else raise the exception [Exc_located loc e]. *)
  value raise : t -> exn -> 'a;

  (** The name of the location variable used in grammars and in
      the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
  value name : ref string;

end;

(** Abstract syntax tree minimal signature.
    Types of this signature are abstract.
    See the {!Camlp4Ast} signature for a concrete definition. *)
module type Ast = sig

  (** {6 Syntactic categories as abstract types} *)

  type loc;
  type meta_bool;
  type meta_option 'a;
  type meta_list 'a;
  type ctyp;
  type patt;
  type expr;
  type module_type;
  type sig_item;
  type with_constr;
  type module_expr;
  type str_item;
  type class_type;
  type class_sig_item;
  type class_expr;
  type class_str_item;
  type match_case;
  type ident;
  type binding;
  type rec_binding;
  type module_binding;

  (** {6 Location accessors} *)

  value loc_of_ctyp : ctyp -> loc;
  value loc_of_patt : patt -> loc;
  value loc_of_expr : expr -> loc;
  value loc_of_module_type : module_type -> loc;
  value loc_of_module_expr : module_expr -> loc;
  value loc_of_sig_item : sig_item -> loc;
  value loc_of_str_item : str_item -> loc;
  value loc_of_class_type : class_type -> loc;
  value loc_of_class_sig_item : class_sig_item -> loc;
  value loc_of_class_expr : class_expr -> loc;
  value loc_of_class_str_item : class_str_item -> loc;
  value loc_of_with_constr : with_constr -> loc;
  value loc_of_binding : binding -> loc;
  value loc_of_rec_binding : rec_binding -> loc;
  value loc_of_module_binding : module_binding -> loc;
  value loc_of_match_case : match_case -> loc;
  value loc_of_ident : ident -> loc;

  (** {6 Traversals} *)

  (** This class is the base class for map traversal on the Ast.
      To make a custom traversal class one just extend it like that:
      
      This example swap pairs expression contents:
      open Camlp4.PreCast;
      [class swap = object
        inherit Ast.map as super;
        method expr e =
          match super#expr e with
          \[ <:expr\@_loc< ($e1$, $e2$) >> -> <:expr< ($e2$, $e1$) >>
          | e -> e \];
      end;
      value _loc = Loc.ghost;
      value map = (new swap)#expr;
      assert (map <:expr< fun x -> (x, 42) >> = <:expr< fun x -> (42, x) >>);]
  *)
  class map : object ('self_type)
    method string : string -> string;
    method list : ! 'a 'b . ('self_type -> 'a -> 'b) -> list 'a -> list 'b;
    method meta_bool : meta_bool -> meta_bool;
    method meta_option : ! 'a 'b . ('self_type -> 'a -> 'b) -> meta_option 'a -> meta_option 'b;
    method meta_list : ! 'a 'b . ('self_type -> 'a -> 'b) -> meta_list 'a -> meta_list 'b;
    method loc : loc -> loc;
    method expr : expr -> expr;
    method patt : patt -> patt;
    method ctyp : ctyp -> ctyp;
    method str_item : str_item -> str_item;
    method sig_item : sig_item -> sig_item;

    method module_expr : module_expr -> module_expr;
    method module_type : module_type -> module_type;
    method class_expr : class_expr -> class_expr;
    method class_type : class_type -> class_type;
    method class_sig_item : class_sig_item -> class_sig_item;
    method class_str_item : class_str_item -> class_str_item;
    method with_constr : with_constr -> with_constr;
    method binding : binding -> binding;
    method rec_binding : rec_binding -> rec_binding;
    method module_binding : module_binding -> module_binding;
    method match_case : match_case -> match_case;
    method ident : ident -> ident;

    method unknown : ! 'a. 'a -> 'a;
  end;

  (** Fold style traversal *)
  class fold : object ('self_type)
    method string : string -> 'self_type;
    method list : ! 'a . ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type;
    method meta_bool : meta_bool -> 'self_type;
    method meta_option : ! 'a . ('self_type -> 'a -> 'self_type) -> meta_option 'a -> 'self_type;
    method meta_list : ! 'a . ('self_type -> 'a -> 'self_type) -> meta_list 'a -> 'self_type;
    method loc : loc -> 'self_type;
    method expr : expr -> 'self_type;
    method patt : patt -> 'self_type;
    method ctyp : ctyp -> 'self_type;
    method str_item : str_item -> 'self_type;
    method sig_item : sig_item -> 'self_type;
    method module_expr : module_expr -> 'self_type;
    method module_type : module_type -> 'self_type;
    method class_expr : class_expr -> 'self_type;
    method class_type : class_type -> 'self_type;
    method class_sig_item : class_sig_item -> 'self_type;
    method class_str_item : class_str_item -> 'self_type;
    method with_constr : with_constr -> 'self_type;
    method binding : binding -> 'self_type;
    method rec_binding : rec_binding -> 'self_type;
    method module_binding : module_binding -> 'self_type;
    method match_case : match_case -> 'self_type;
    method ident : ident -> 'self_type;

    method unknown : ! 'a. 'a -> 'self_type;
  end;

end;


(** Signature for OCaml syntax trees. *) (*
    This signature is an extension of {!Ast}
    It provides:
      - Types for all kinds of structure.
      - Map: A base class for map traversals.
      - Map classes and functions for common kinds.

    == Core language ==
    ctyp               :: Representaion of types
    patt               :: The type of patterns
    expr               :: The type of expressions
    match_case         :: The type of cases for match/function/try constructions
    ident              :: The type of identifiers (including path like Foo(X).Bar.y)
    binding            :: The type of let bindings
    rec_binding        :: The type of record definitions

    == Modules ==
    module_type        :: The type of module types
    sig_item           :: The type of signature items
    str_item           :: The type of structure items
    module_expr        :: The type of module expressions
    module_binding     :: The type of recursive module definitions
    with_constr        :: The type of `with' constraints

    == Classes ==
    class_type         :: The type of class types
    class_sig_item     :: The type of class signature items
    class_expr         :: The type of class expressions
    class_str_item     :: The type of class structure items
 *)
module type Camlp4Ast = sig

  (** The inner module for locations *)
  module Loc : Loc;

  INCLUDE "camlp4/Camlp4/Camlp4Ast.partial.ml";

  value loc_of_ctyp : ctyp -> loc;
  value loc_of_patt : patt -> loc;
  value loc_of_expr : expr -> loc;
  value loc_of_module_type : module_type -> loc;
  value loc_of_module_expr : module_expr -> loc;
  value loc_of_sig_item : sig_item -> loc;
  value loc_of_str_item : str_item -> loc;
  value loc_of_class_type : class_type -> loc;
  value loc_of_class_sig_item : class_sig_item -> loc;
  value loc_of_class_expr : class_expr -> loc;
  value loc_of_class_str_item : class_str_item -> loc;
  value loc_of_with_constr : with_constr -> loc;
  value loc_of_binding : binding -> loc;
  value loc_of_rec_binding : rec_binding -> loc;
  value loc_of_module_binding : module_binding -> loc;
  value loc_of_match_case : match_case -> loc;
  value loc_of_ident : ident -> loc;

  module Meta : sig
    module type META_LOC = sig
      (* The first location is where to put the returned pattern.
          Generally it's _loc to match with <:patt< ... >> quotations.
          The second location is the one to treat. *)
      value meta_loc_patt : loc -> loc -> patt;
      (* The first location is where to put the returned expression.
          Generally it's _loc to match with <:expr< ... >> quotations.
          The second location is the one to treat. *)
      value meta_loc_expr : loc -> loc -> expr;
    end;
    module MetaLoc : sig
      value meta_loc_patt : loc -> loc -> patt;
      value meta_loc_expr : loc -> loc -> expr;
    end;
    module MetaGhostLoc : sig
      value meta_loc_patt : loc -> 'a -> patt;
      value meta_loc_expr : loc -> 'a -> expr;
    end;
    module MetaLocVar : sig
      value meta_loc_patt : loc -> 'a -> patt;
      value meta_loc_expr : loc -> 'a -> expr;
    end;
    module Make (MetaLoc : META_LOC) : sig
      module Expr : sig
        value meta_string : loc -> string -> expr;
        value meta_int : loc -> string -> expr;
        value meta_float : loc -> string -> expr;
        value meta_char : loc -> string -> expr;
        value meta_bool : loc -> bool -> expr;
        value meta_list : (loc -> 'a -> expr) -> loc -> list 'a -> expr;
        value meta_binding : loc -> binding -> expr;
        value meta_rec_binding : loc -> rec_binding -> expr;
        value meta_class_expr : loc -> class_expr -> expr;
        value meta_class_sig_item : loc -> class_sig_item -> expr;
        value meta_class_str_item : loc -> class_str_item -> expr;
        value meta_class_type : loc -> class_type -> expr;
        value meta_ctyp : loc -> ctyp -> expr;
        value meta_expr : loc -> expr -> expr;
        value meta_ident : loc -> ident -> expr;
        value meta_match_case : loc -> match_case -> expr;
        value meta_module_binding : loc -> module_binding -> expr;
        value meta_module_expr : loc -> module_expr -> expr;
        value meta_module_type : loc -> module_type -> expr;
        value meta_patt : loc -> patt -> expr;
        value meta_sig_item : loc -> sig_item -> expr;
        value meta_str_item : loc -> str_item -> expr;
        value meta_with_constr : loc -> with_constr -> expr;
      end;
      module Patt : sig
        value meta_string : loc -> string -> patt;
        value meta_int : loc -> string -> patt;
        value meta_float : loc -> string -> patt;
        value meta_char : loc -> string -> patt;
        value meta_bool : loc -> bool -> patt;
        value meta_list : (loc -> 'a -> patt) -> loc -> list 'a -> patt;
        value meta_binding : loc -> binding -> patt;
        value meta_rec_binding : loc -> rec_binding -> patt;
        value meta_class_expr : loc -> class_expr -> patt;
        value meta_class_sig_item : loc -> class_sig_item -> patt;
        value meta_class_str_item : loc -> class_str_item -> patt;
        value meta_class_type : loc -> class_type -> patt;
        value meta_ctyp : loc -> ctyp -> patt;
        value meta_expr : loc -> expr -> patt;
        value meta_ident : loc -> ident -> patt;
        value meta_match_case : loc -> match_case -> patt;
        value meta_module_binding : loc -> module_binding -> patt;
        value meta_module_expr : loc -> module_expr -> patt;
        value meta_module_type : loc -> module_type -> patt;
        value meta_patt : loc -> patt -> patt;
        value meta_sig_item : loc -> sig_item -> patt;
        value meta_str_item : loc -> str_item -> patt;
        value meta_with_constr : loc -> with_constr -> patt;
      end;
    end;
  end;

  (** See {!Ast.map}. *)
  class map : object ('self_type)
    method string : string -> string;
    method list : ! 'a 'b . ('self_type -> 'a -> 'b) -> list 'a -> list 'b;
    method meta_bool : meta_bool -> meta_bool;
    method meta_option : ! 'a 'b . ('self_type -> 'a -> 'b) -> meta_option 'a -> meta_option 'b;
    method meta_list : ! 'a 'b . ('self_type -> 'a -> 'b) -> meta_list 'a -> meta_list 'b;
    method loc : loc -> loc;
    method expr : expr -> expr;
    method patt : patt -> patt;
    method ctyp : ctyp -> ctyp;
    method str_item : str_item -> str_item;
    method sig_item : sig_item -> sig_item;

    method module_expr : module_expr -> module_expr;
    method module_type : module_type -> module_type;
    method class_expr : class_expr -> class_expr;
    method class_type : class_type -> class_type;
    method class_sig_item : class_sig_item -> class_sig_item;
    method class_str_item : class_str_item -> class_str_item;
    method with_constr : with_constr -> with_constr;
    method binding : binding -> binding;
    method rec_binding : rec_binding -> rec_binding;
    method module_binding : module_binding -> module_binding;
    method match_case : match_case -> match_case;
    method ident : ident -> ident;

    method unknown : ! 'a. 'a -> 'a;
  end;

  (** See {!Ast.fold}. *)
  class fold : object ('self_type)
    method string : string -> 'self_type;
    method list : ! 'a . ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type;
    method meta_bool : meta_bool -> 'self_type;
    method meta_option : ! 'a . ('self_type -> 'a -> 'self_type) -> meta_option 'a -> 'self_type;
    method meta_list : ! 'a . ('self_type -> 'a -> 'self_type) -> meta_list 'a -> 'self_type;
    method loc : loc -> 'self_type;
    method expr : expr -> 'self_type;
    method patt : patt -> 'self_type;
    method ctyp : ctyp -> 'self_type;
    method str_item : str_item -> 'self_type;
    method sig_item : sig_item -> 'self_type;
    method module_expr : module_expr -> 'self_type;
    method module_type : module_type -> 'self_type;
    method class_expr : class_expr -> 'self_type;
    method class_type : class_type -> 'self_type;
    method class_sig_item : class_sig_item -> 'self_type;
    method class_str_item : class_str_item -> 'self_type;
    method with_constr : with_constr -> 'self_type;
    method binding : binding -> 'self_type;
    method rec_binding : rec_binding -> 'self_type;
    method module_binding : module_binding -> 'self_type;
    method match_case : match_case -> 'self_type;
    method ident : ident -> 'self_type;

    method unknown : ! 'a. 'a -> 'self_type;
  end;

  value map_expr : (expr -> expr) -> map;
  value map_patt : (patt -> patt) -> map;
  value map_ctyp : (ctyp -> ctyp) -> map;
  value map_str_item : (str_item -> str_item) -> map;
  value map_sig_item : (sig_item -> sig_item) -> map;
  value map_loc : (loc -> loc) -> map;

  value ident_of_expr : expr -> ident;
  value ident_of_patt : patt -> ident;
  value ident_of_ctyp : ctyp -> ident;

  value biAnd_of_list : list binding -> binding;
  value rbSem_of_list : list rec_binding -> rec_binding;
  value paSem_of_list : list patt -> patt;
  value paCom_of_list : list patt -> patt;
  value tyOr_of_list : list ctyp -> ctyp;
  value tyAnd_of_list : list ctyp -> ctyp;
  value tyAmp_of_list : list ctyp -> ctyp;
  value tySem_of_list : list ctyp -> ctyp;
  value tyCom_of_list : list ctyp -> ctyp;
  value tySta_of_list : list ctyp -> ctyp;
  value stSem_of_list : list str_item -> str_item;
  value sgSem_of_list : list sig_item -> sig_item;
  value crSem_of_list : list class_str_item -> class_str_item;
  value cgSem_of_list : list class_sig_item -> class_sig_item;
  value ctAnd_of_list : list class_type -> class_type;
  value ceAnd_of_list : list class_expr -> class_expr;
  value wcAnd_of_list : list with_constr -> with_constr;
  value meApp_of_list : list module_expr -> module_expr;
  value mbAnd_of_list : list module_binding -> module_binding;
  value mcOr_of_list : list match_case -> match_case;
  value idAcc_of_list : list ident -> ident;
  value idApp_of_list : list ident -> ident;
  value exSem_of_list : list expr -> expr;
  value exCom_of_list : list expr -> expr;

  value list_of_ctyp : ctyp -> list ctyp -> list ctyp;
  value list_of_binding : binding -> list binding -> list binding;
  value list_of_rec_binding : rec_binding -> list rec_binding -> list rec_binding;
  value list_of_with_constr : with_constr -> list with_constr -> list with_constr;
  value list_of_patt : patt -> list patt -> list patt;
  value list_of_expr : expr -> list expr -> list expr;
  value list_of_str_item : str_item -> list str_item -> list str_item;
  value list_of_sig_item : sig_item -> list sig_item -> list sig_item;
  value list_of_class_sig_item : class_sig_item -> list class_sig_item -> list class_sig_item;
  value list_of_class_str_item : class_str_item -> list class_str_item -> list class_str_item;
  value list_of_class_type : class_type -> list class_type -> list class_type;
  value list_of_class_expr : class_expr -> list class_expr -> list class_expr;
  value list_of_module_expr : module_expr -> list module_expr -> list module_expr;
  value list_of_module_binding : module_binding -> list module_binding -> list module_binding;
  value list_of_match_case : match_case -> list match_case -> list match_case;
  value list_of_ident : ident -> list ident -> list ident;

  (** Like [String.escape] but takes care to not
      escape antiquotations strings. *)
  value safe_string_escaped : string -> string;

  (** Returns True if the given pattern is irrefutable. *)
  value is_irrefut_patt : patt -> bool;

  value is_constructor : ident -> bool;
  value is_patt_constructor : patt -> bool;
  value is_expr_constructor : expr -> bool;

  value ty_of_stl : (Loc.t * string * list ctyp) -> ctyp;
  value ty_of_sbt : (Loc.t * string * bool * ctyp) -> ctyp;
  value bi_of_pe : (patt * expr) -> binding;
  value pel_of_binding : binding -> list (patt * expr);
  value binding_of_pel : list (patt * expr) -> binding;
  value sum_type_of_list : list (Loc.t * string * list ctyp) -> ctyp;
  value record_type_of_list : list (Loc.t * string * bool * ctyp) -> ctyp;
end;

(** This functor is a restriction functor.
    It takes a Camlp4Ast module and gives the Ast one.
    Typical use is for [with] constraints.
    Example: ... with module Ast = Camlp4.Sig.Camlp4AstToAst Camlp4Ast *)
module Camlp4AstToAst (M : Camlp4Ast) : Ast
  with type loc = M.loc
   and type meta_bool = M.meta_bool
   and type meta_option 'a = M.meta_option 'a
   and type meta_list 'a = M.meta_list 'a
   and type ctyp = M.ctyp
   and type patt = M.patt
   and type expr = M.expr
   and type module_type = M.module_type
   and type sig_item = M.sig_item
   and type with_constr = M.with_constr
   and type module_expr = M.module_expr
   and type str_item = M.str_item
   and type class_type = M.class_type
   and type class_sig_item = M.class_sig_item
   and type class_expr = M.class_expr
   and type class_str_item = M.class_str_item
   and type binding = M.binding
   and type rec_binding = M.rec_binding
   and type module_binding = M.module_binding
   and type match_case = M.match_case
   and type ident = M.ident
= M;

(** Concrete definition of Camlp4 ASTs abstracted from locations.
    Since the Ast contains locations, this functor produces Ast types
    for a given location type. *)
module MakeCamlp4Ast (Loc : Type) = struct

  INCLUDE "camlp4/Camlp4/Camlp4Ast.partial.ml";

end;

(** {6 Filters} *)

(** A type for stream filters. *)
type stream_filter 'a 'loc = Stream.t ('a * 'loc) -> Stream.t ('a * 'loc);

(** Registerinng and folding of Ast filters.
    Two kinds of filters must be handled:
      - Implementation filters: str_item -> str_item.
      - Interface filters: sig_item -> sig_item. *)
module type AstFilters = sig

  module Ast : Camlp4Ast;

  type filter 'a = 'a -> 'a;

  value register_sig_item_filter : (filter Ast.sig_item) -> unit;
  value register_str_item_filter : (filter Ast.str_item) -> unit;
  value register_topphrase_filter : (filter Ast.str_item) -> unit;

  value fold_interf_filters : ('a -> filter Ast.sig_item -> 'a) -> 'a -> 'a;
  value fold_implem_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;
  value fold_topphrase_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;

end;

(** ASTs as one single dynamic type *)
module type DynAst = sig
  module Ast : Ast;
  type tag 'a;

  value ctyp_tag : tag Ast.ctyp;
  value patt_tag : tag Ast.patt;
  value expr_tag : tag Ast.expr;
  value module_type_tag : tag Ast.module_type;
  value sig_item_tag : tag Ast.sig_item;
  value with_constr_tag : tag Ast.with_constr;
  value module_expr_tag : tag Ast.module_expr;
  value str_item_tag : tag Ast.str_item;
  value class_type_tag : tag Ast.class_type;
  value class_sig_item_tag : tag Ast.class_sig_item;
  value class_expr_tag : tag Ast.class_expr;
  value class_str_item_tag : tag Ast.class_str_item;
  value match_case_tag : tag Ast.match_case;
  value ident_tag : tag Ast.ident;
  value binding_tag : tag Ast.binding;
  value rec_binding_tag : tag Ast.rec_binding;
  value module_binding_tag : tag Ast.module_binding;

  value string_of_tag : tag 'a -> string;

  module Pack (X : sig type t 'a; end) : sig
    type pack;
    value pack : tag 'a -> X.t 'a -> pack;
    value unpack : tag 'a -> pack -> X.t 'a;
    value print_tag : Format.formatter -> pack -> unit;
  end;
end;


(** {6 Quotation operations} *)

(** The generic quotation type.
    To see how fields are used here is an example:
       <:q_name@q_loc<q_contents>>
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
type quotation =
  { q_name     : string ;
    q_loc      : string ;
    q_shift    : int    ;
    q_contents : string };

(** The signature for a quotation expander registery. *)
module type Quotation = sig
  module Ast : Ast;
  module DynAst : DynAst with module Ast = Ast;
  open Ast;

  (** The [loc] is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
  type expand_fun 'a = loc -> option string -> string -> 'a;

  (** [add name exp] adds the quotation [name] associated with the
      expander [exp]. *)
  value add : string -> DynAst.tag 'a -> expand_fun 'a -> unit;

  (** [find name] returns the expander of the given quotation name. *)
  value find : string -> DynAst.tag 'a -> expand_fun 'a;

  (** [default] holds the default quotation name. *)
  value default : ref string;

  (** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
      It's a parser wrapper, this function handles the error reporting for you. *)
  value parse_quotation_result :
    (loc -> string -> 'a) -> loc -> quotation -> string -> string -> 'a;

  (** function translating quotation names; default = identity *)
  value translate : ref (string -> string);

  value expand : loc -> quotation -> DynAst.tag 'a -> 'a;

  (** [dump_file] optionally tells Camlp4 to dump the
      result of an expander if this result is syntactically incorrect.
      If [None] (default), this result is not dumped. If [Some fname], the
      result is dumped in the file [fname]. *)
  value dump_file : ref (option string);

  module Error : Error;

end;

(** {6 Tokens} *)

(** A signature for tokens. *)
module type Token = sig

  module Loc : Loc;

  type t;

  value to_string : t -> string;
  
  value print : Format.formatter -> t -> unit;

  value match_keyword : string -> t -> bool;

  value extract_string : t -> string;

  module Filter : sig

    type token_filter = stream_filter t Loc.t;

    (** The type for this filter chain.
        A basic implementation just store the [is_keyword] function given
        by [mk] and use it in the [filter] function. *)
    type t;

    (** The given predicate function returns true if the given string
        is a keyword. This function can be used in filters to translate
        identifier tokens to keyword tokens. *)
    value mk : (string -> bool) -> t;

    (** This function allows to register a new filter to the token filter chain.
        You can choose to not support these and raise an exception. *)
    value define_filter : t -> (token_filter -> token_filter) -> unit;

    (** This function filter the given stream and return a filtered stream.
        A basic implementation just match identifiers against the [is_keyword]
        function to produce token keywords instead. *)
    value filter : t -> token_filter;

    (** Called by the grammar system when a keyword is used. 
        The boolean argument is True when it's the first time that keyword
        is used. If you do not care about this information just return [()]. *)
    value keyword_added : t -> string -> bool -> unit;

    (** Called by the grammar system when a keyword is no longer used.
        If you do not care about this information just return [()]. *)
    value keyword_removed : t -> string -> unit;
  end;

  module Error : Error;
end;

(** This signature describes tokens for the Objective Caml and the Revised
    syntax lexing rules. For some tokens the data constructor holds two
    representations with the evaluated one and the source one. For example
    the INT data constructor holds an integer and a string, this string can
    contains more information that's needed for a good pretty-printing
    ("42", "4_2", "0000042", "0b0101010"...).

    The meaning of the tokens are:
-      [KEYWORD s] is the keyword [s].
-      [LIDENT s] is the ident [s] starting with a lowercase letter.
-      [UIDENT s] is the ident [s] starting with an uppercase letter.
-      [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
        the integer constant [i] whose string source is [s].
-      [FLOAT f s] is the float constant [f] whose string source is [s].
-      [STRING s s'] is the string constant [s] whose string source is [s'].
-      [CHAR c s] is the character constant [c] whose string source is [s].
-      [QUOTATION q] is a quotation [q], see {!Quotation.t} for more information.
-      [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
-      [EOI] is the end of input.

     Warning: the second string associated with the constructor [STRING] is
     the string found in the source without any interpretation. In particular,
     the backslashes are not interpreted. For example, if the input is ["\n"]
     the string is *not* a string with one element containing the character
     "return", but a string of two elements: the backslash and the character
     ["n"]. To interpret a string use the first string of the [STRING]
     constructor (or if you need to compute it use the module
     {!Camlp4.Struct.Token.Eval}. Same thing for the constructor [CHAR]. *)
type camlp4_token =
  [ KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of quotation
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

(** A signature for specialized tokens. *)
module type Camlp4Token = Token with type t = camlp4_token;

(** {6 Dynamic loaders} *)

(** A signature for dynamic loaders. *)
module type DynLoader = sig
  type t;
  exception Error of string and string;

  (** [mk ?ocaml_stdlib ?camlp4_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:False] *)
  value mk : ?ocaml_stdlib: bool -> ?camlp4_stdlib: bool -> unit -> t;

  (** Fold over the current load path list. *)
  value fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a;

  (** [load f] Load the file [f]. If [f] is not an absolute path name,
      the load path list used to find the directory of [f]. *)
  value load : t -> string -> unit;

  (** [include_dir d] Add the directory [d] in the current load path
      list (like the common -I option). *)
  value include_dir : t -> string -> unit;

  (** [find_in_path f] Returns the full path of the file [f] if
      [f] is in the current load path, raises [Not_found] otherwise. *)
  value find_in_path : t -> string -> string;

  (** [is_native] [True] if we are in native code, [False] for bytecode. *)
  value is_native : bool;
end;

(** A signature for grammars. *)
module Grammar = struct

  (** Internal signature for sematantic actions of grammars,
      not for the casual user. These functions are unsafe. *)
  module type Action = sig
    type  t    ;

    value mk    : 'a ->  t;
    value get   :  t -> 'a;
    value getf  :  t -> ('a -> 'b);
    value getf2 :  t -> ('a -> 'b -> 'c);
  end;

  type assoc =
    [ NonA
    | RightA
    | LeftA ];

  type position =
    [ First
    | Last
    | Before of string
    | After of string
    | Level of string ];

  (** Common signature for {!Sig.Grammar.Static} and {!Sig.Grammar.Dynamic}. *)
  module type Structure = sig
    module Loc    : Loc;
    module Action : Action;
    module Token  : Token with module Loc = Loc;

    type gram;
    type internal_entry;
    type tree;

    type token_pattern = ((Token.t -> bool) * string);

    type symbol =
      [ Smeta of string and list symbol and Action.t
      | Snterm of internal_entry
      | Snterml of internal_entry and string
      | Slist0 of symbol
      | Slist0sep of symbol and symbol
      | Slist1 of symbol
      | Slist1sep of symbol and symbol
      | Sopt of symbol
      | Sself
      | Snext
      | Stoken of token_pattern
      | Skeyword of string
      | Stree of tree ];

    type production_rule = (list symbol * Action.t);
    type single_extend_statment =
      (option string * option assoc * list production_rule);
    type extend_statment =
      (option position * list single_extend_statment);
    type delete_statment = list symbol;

    type fold 'a 'b 'c =
      internal_entry -> list symbol ->
        (Stream.t 'a -> 'b) -> Stream.t 'a -> 'c;

    type foldsep 'a 'b 'c =
      internal_entry -> list symbol ->
        (Stream.t 'a -> 'b) -> (Stream.t 'a -> unit) -> Stream.t 'a -> 'c;

  end;

  (** Signature for Camlp4 grammars. Here the dynamic means that you can produce as
      many grammar values as needed with a single grammar module.
      If you do not need many grammar values it's preferable to use a static one. *)
  module type Dynamic = sig
    include Structure;

    (** Make a new grammar. *)
    value mk : unit -> gram;

    module Entry : sig
      (** The abstract type of grammar entries. The type parameter is the type
          of the semantic actions that are associated with this entry. *)
      type t 'a;
  
      (** Make a new entry from the given name. *)
      value mk : gram -> string -> t 'a;
  
      (** Make a new entry from a name and an hand made token parser. *)
      value of_parser :
        gram -> string -> (Stream.t (Token.t * Loc.t) -> 'a) -> t 'a;

      (** Clear the entry and setup this parser instead. *)
      value setup_parser :
        t 'a -> (Stream.t (Token.t * Loc.t) -> 'a) -> unit;

      (** Get the entry name. *)
      value name : t 'a -> string;

      (** Print the given entry into the given formatter. *)
      value print : Format.formatter -> t 'a -> unit;

      (** Same as {!print} but show the left-factorization. *)
      value dump : Format.formatter -> t 'a -> unit;

      (**/**)
      value obj : t 'a -> internal_entry;
      value clear : t 'a -> unit;
      (**/**)
    end;

    (** [get_filter g] Get the {!Token.Filter} associated to the [g]. *)
    value get_filter : gram -> Token.Filter.t;

    type not_filtered 'a;

    (** This function is called by the EXTEND ... END syntax. *)
    value extend      : Entry.t 'a -> extend_statment -> unit;

    (** The delete rule. *)
    value delete_rule : Entry.t 'a -> delete_statment -> unit;

    value srules      : Entry.t 'a -> list (list symbol * Action.t) -> symbol;
    value sfold0      : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
    value sfold1      : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
    value sfold0sep   : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b;
    (* value sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)

    (** Use the lexer to produce a non filtered token stream from a char stream. *)
    value lex : gram -> Loc.t -> Stream.t char
                    -> not_filtered (Stream.t (Token.t * Loc.t));

    (** Token stream from string. *)
    value lex_string : gram -> Loc.t -> string
                            -> not_filtered (Stream.t (Token.t * Loc.t));

    (** Filter a token stream using the {!Token.Filter} module *)
    value filter : gram -> not_filtered (Stream.t (Token.t * Loc.t))
                                      -> Stream.t (Token.t * Loc.t);

    (** Lex, filter and parse a stream of character. *)
    value parse : Entry.t 'a -> Loc.t -> Stream.t char -> 'a;

    (** Same as {!parse} but from a string. *)
    value parse_string : Entry.t 'a -> Loc.t -> string -> 'a;

    (** Parse a token stream that is not filtered yet. *)
    value parse_tokens_before_filter :
      Entry.t 'a -> not_filtered (Stream.t (Token.t * Loc.t)) -> 'a;

    (** Parse a token stream that is already filtered. *)
    value parse_tokens_after_filter :
      Entry.t 'a -> Stream.t (Token.t * Loc.t) -> 'a;

  end;

  (** Signature for Camlp4 grammars. Here the static means that there is only
      one grammar value by grammar module. If you do not need to store the grammar
      value it's preferable to use a static one. *)
  module type Static = sig
    include Structure;

    module Entry : sig
      (** The abstract type of grammar entries. The type parameter is the type
          of the semantic actions that are associated with this entry. *)
      type t 'a;

      (** Make a new entry from the given name. *)
      value mk : string -> t 'a;

      (** Make a new entry from a name and an hand made token parser. *)
      value of_parser :
        string -> (Stream.t (Token.t * Loc.t) -> 'a) -> t 'a;

      (** Clear the entry and setup this parser instead. *)
      value setup_parser :
        t 'a -> (Stream.t (Token.t * Loc.t) -> 'a) -> unit;

      (** Get the entry name. *)
      value name : t 'a -> string;

      (** Print the given entry into the given formatter. *)
      value print : Format.formatter -> t 'a -> unit;

      (** Same as {!print} but show the left-factorization. *)
      value dump : Format.formatter -> t 'a -> unit;

      (**/**)
      value obj : t 'a -> internal_entry;
      value clear : t 'a -> unit;
      (**/**)
    end;

    (** Get the {!Token.Filter} associated to the grammar module. *)
    value get_filter : unit -> Token.Filter.t;

    type not_filtered 'a;

    (** This function is called by the EXTEND ... END syntax. *)
    value extend      : Entry.t 'a -> extend_statment -> unit;

    (** The delete rule. *)
    value delete_rule : Entry.t 'a -> delete_statment -> unit;
    value srules      : Entry.t 'a -> list (list symbol * Action.t) -> symbol;
    value sfold0      : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
    value sfold1      : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
    value sfold0sep   : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b;
    (* value sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)

    (** Use the lexer to produce a non filtered token stream from a char stream. *)
    value lex : Loc.t -> Stream.t char
                      -> not_filtered (Stream.t (Token.t * Loc.t));
    (** Token stream from string. *)
    value lex_string : Loc.t -> string
                            -> not_filtered (Stream.t (Token.t * Loc.t));

    (** Filter a token stream using the {!Token.Filter} module *)
    value filter : not_filtered (Stream.t (Token.t * Loc.t))
                              -> Stream.t (Token.t * Loc.t);

    (** Lex, filter and parse a stream of character. *)
    value parse : Entry.t 'a -> Loc.t -> Stream.t char -> 'a;

    (** Same as {!parse} but from a string. *)
    value parse_string : Entry.t 'a -> Loc.t -> string -> 'a;

    (** Parse a token stream that is not filtered yet. *)
    value parse_tokens_before_filter :
      Entry.t 'a -> not_filtered (Stream.t (Token.t * Loc.t)) -> 'a;

    (** Parse a token stream that is already filtered. *)
    value parse_tokens_after_filter :
      Entry.t 'a -> Stream.t (Token.t * Loc.t) -> 'a;

  end;

end;

(** A signature for lexers. *)
module type Lexer = sig
  module Loc : Loc;
  module Token : Token with module Loc = Loc;
  module Error : Error;

  (** The constructor for a lexing function. The character stream is the input
      stream to be lexed. The result is a stream of pairs of a token and
      a location.
      The lexer do not use global (mutable) variables: instantiations
      of [Lexer.mk ()] do not perturb each other. *)
  value mk : unit -> (Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t));
end;


(** A signature for parsers abstract from ASTs. *)
module Parser (Ast : Ast) = struct
  module type SIMPLE = sig
    (** The parse function for expressions.
        The underlying expression grammar entry is generally "expr; EOI". *)
    value parse_expr : Ast.loc -> string -> Ast.expr;

    (** The parse function for patterns.
        The underlying pattern grammar entry is generally "patt; EOI". *)
    value parse_patt : Ast.loc -> string -> Ast.patt;
  end;

  module type S = sig

    (** Called when parsing an implementation (ml file) to build the syntax
        tree; the returned list contains the phrases (structure items) as a
        single "declare" node (a list of structure items);   if  the parser
        encounter a directive it stops (since the directive may change  the
        syntax), the given [directive_handler] function  evaluates  it  and
        the parsing starts again. *)
    value parse_implem : ?directive_handler:(Ast.str_item -> option Ast.str_item) ->
                        Ast.loc -> Stream.t char -> Ast.str_item;

    (** Same as {!parse_implem} but for interface (mli file). *)
    value parse_interf : ?directive_handler:(Ast.sig_item -> option Ast.sig_item) ->
                        Ast.loc -> Stream.t char -> Ast.sig_item;
  end;
end;

(** A signature for printers abstract from ASTs. *)
module Printer (Ast : Ast) = struct
  module type S = sig

    value print_interf : ?input_file:string -> ?output_file:string ->
                        Ast.sig_item -> unit;
    value print_implem : ?input_file:string -> ?output_file:string ->
                        Ast.str_item -> unit;

  end;
end;

(** A syntax module is a sort of constistent bunch of modules and values.
   In such a module you have a parser, a printer, and also modules for
   locations, syntax trees, tokens, grammars, quotations, anti-quotations.
   There is also the main grammar entries. *)
module type Syntax = sig
  module Loc            : Loc;
  module Ast            : Ast with type loc = Loc.t;
  module Token          : Token with module Loc = Loc;
  module Gram           : Grammar.Static with module Loc = Loc and module Token = Token;
  module Quotation      : Quotation with module Ast = Ast;

  module AntiquotSyntax : (Parser Ast).SIMPLE;

  include (Warning Loc).S;
  include (Parser  Ast).S;
  include (Printer Ast).S;
end;

(** A syntax module is a sort of constistent bunch of modules and values.
    In such a module you have a parser, a printer, and also modules for
    locations, syntax trees, tokens, grammars, quotations, anti-quotations.
    There is also the main grammar entries. *)
module type Camlp4Syntax = sig
  module Loc            : Loc;

  module Ast            : Camlp4Ast with module Loc = Loc;
  module Token          : Camlp4Token with module Loc = Loc;

  module Gram           : Grammar.Static with module Loc = Loc and module Token = Token;
  module Quotation      : Quotation with module Ast = Camlp4AstToAst Ast;

  module AntiquotSyntax : (Parser Ast).SIMPLE;

  include (Warning Loc).S;
  include (Parser  Ast).S;
  include (Printer Ast).S;

  value interf : Gram.Entry.t (list Ast.sig_item * option Loc.t);
  value implem : Gram.Entry.t (list Ast.str_item * option Loc.t);
  value top_phrase : Gram.Entry.t (option Ast.str_item);
  value use_file : Gram.Entry.t (list Ast.str_item * option Loc.t);
  value a_CHAR : Gram.Entry.t string;
  value a_FLOAT : Gram.Entry.t string;
  value a_INT : Gram.Entry.t string;
  value a_INT32 : Gram.Entry.t string;
  value a_INT64 : Gram.Entry.t string;
  value a_LABEL : Gram.Entry.t string;
  value a_LIDENT : Gram.Entry.t string;
  value a_NATIVEINT : Gram.Entry.t string;
  value a_OPTLABEL : Gram.Entry.t string;
  value a_STRING : Gram.Entry.t string;
  value a_UIDENT : Gram.Entry.t string;
  value a_ident : Gram.Entry.t string;
  value amp_ctyp : Gram.Entry.t Ast.ctyp;
  value and_ctyp : Gram.Entry.t Ast.ctyp;
  value match_case : Gram.Entry.t Ast.match_case;
  value match_case0 : Gram.Entry.t Ast.match_case;
  value match_case_quot : Gram.Entry.t Ast.match_case;
  value binding : Gram.Entry.t Ast.binding;
  value binding_quot : Gram.Entry.t Ast.binding;
  value rec_binding_quot : Gram.Entry.t Ast.rec_binding;
  value class_declaration : Gram.Entry.t Ast.class_expr;
  value class_description : Gram.Entry.t Ast.class_type;
  value class_expr : Gram.Entry.t Ast.class_expr;
  value class_expr_quot : Gram.Entry.t Ast.class_expr;
  value class_fun_binding : Gram.Entry.t Ast.class_expr;
  value class_fun_def : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_expr : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_type : Gram.Entry.t Ast.class_type;
  value class_longident : Gram.Entry.t Ast.ident;
  value class_longident_and_param : Gram.Entry.t Ast.class_expr;
  value class_name_and_param : Gram.Entry.t (string * Ast.ctyp);
  value class_sig_item : Gram.Entry.t Ast.class_sig_item;
  value class_sig_item_quot : Gram.Entry.t Ast.class_sig_item;
  value class_signature : Gram.Entry.t Ast.class_sig_item;
  value class_str_item : Gram.Entry.t Ast.class_str_item;
  value class_str_item_quot : Gram.Entry.t Ast.class_str_item;
  value class_structure : Gram.Entry.t Ast.class_str_item;
  value class_type : Gram.Entry.t Ast.class_type;
  value class_type_declaration : Gram.Entry.t Ast.class_type;
  value class_type_longident : Gram.Entry.t Ast.ident;
  value class_type_longident_and_param : Gram.Entry.t Ast.class_type;
  value class_type_plus : Gram.Entry.t Ast.class_type;
  value class_type_quot : Gram.Entry.t Ast.class_type;
  value comma_ctyp : Gram.Entry.t Ast.ctyp;
  value comma_expr : Gram.Entry.t Ast.expr;
  value comma_ipatt : Gram.Entry.t Ast.patt;
  value comma_patt : Gram.Entry.t Ast.patt;
  value comma_type_parameter : Gram.Entry.t Ast.ctyp;
  value constrain : Gram.Entry.t (Ast.ctyp * Ast.ctyp);
  value constructor_arg_list : Gram.Entry.t Ast.ctyp;
  value constructor_declaration : Gram.Entry.t Ast.ctyp;
  value constructor_declarations : Gram.Entry.t Ast.ctyp;
  value ctyp : Gram.Entry.t Ast.ctyp;
  value ctyp_quot : Gram.Entry.t Ast.ctyp;
  value cvalue_binding : Gram.Entry.t Ast.expr;
  value direction_flag : Gram.Entry.t Ast.meta_bool;
  value dummy : Gram.Entry.t unit;
  value eq_expr : Gram.Entry.t (string -> Ast.patt -> Ast.patt);
  value expr : Gram.Entry.t Ast.expr;
  value expr_eoi : Gram.Entry.t Ast.expr;
  value expr_quot : Gram.Entry.t Ast.expr;
  value field_expr : Gram.Entry.t Ast.rec_binding;
  value field_expr_list : Gram.Entry.t Ast.rec_binding;
  value fun_binding : Gram.Entry.t Ast.expr;
  value fun_def : Gram.Entry.t Ast.expr;
  value ident : Gram.Entry.t Ast.ident;
  value ident_quot : Gram.Entry.t Ast.ident;
  value ipatt : Gram.Entry.t Ast.patt;
  value ipatt_tcon : Gram.Entry.t Ast.patt;
  value label : Gram.Entry.t string;
  value label_declaration : Gram.Entry.t Ast.ctyp;
  value label_declaration_list : Gram.Entry.t Ast.ctyp;
  value label_expr : Gram.Entry.t Ast.rec_binding;
  value label_expr_list : Gram.Entry.t Ast.rec_binding;
  value label_ipatt : Gram.Entry.t Ast.patt;
  value label_ipatt_list : Gram.Entry.t Ast.patt;
  value label_longident : Gram.Entry.t Ast.ident;
  value label_patt : Gram.Entry.t Ast.patt;
  value label_patt_list : Gram.Entry.t Ast.patt;
  value labeled_ipatt : Gram.Entry.t Ast.patt;
  value let_binding : Gram.Entry.t Ast.binding;
  value meth_list : Gram.Entry.t (Ast.ctyp * Ast.meta_bool);
  value meth_decl : Gram.Entry.t Ast.ctyp;
  value module_binding : Gram.Entry.t Ast.module_binding;
  value module_binding0 : Gram.Entry.t Ast.module_expr;
  value module_binding_quot : Gram.Entry.t Ast.module_binding;
  value module_declaration : Gram.Entry.t Ast.module_type;
  value module_expr : Gram.Entry.t Ast.module_expr;
  value module_expr_quot : Gram.Entry.t Ast.module_expr;
  value module_longident : Gram.Entry.t Ast.ident;
  value module_longident_with_app : Gram.Entry.t Ast.ident;
  value module_rec_declaration : Gram.Entry.t Ast.module_binding;
  value module_type : Gram.Entry.t Ast.module_type;
  value module_type_quot : Gram.Entry.t Ast.module_type;
  value more_ctyp : Gram.Entry.t Ast.ctyp;
  value name_tags : Gram.Entry.t Ast.ctyp;
  value opt_as_lident : Gram.Entry.t string;
  value opt_class_self_patt : Gram.Entry.t Ast.patt;
  value opt_class_self_type : Gram.Entry.t Ast.ctyp;
  value opt_comma_ctyp : Gram.Entry.t Ast.ctyp;
  value opt_dot_dot : Gram.Entry.t Ast.meta_bool;
  value opt_eq_ctyp : Gram.Entry.t Ast.ctyp;
  value opt_expr : Gram.Entry.t Ast.expr;
  value opt_meth_list : Gram.Entry.t Ast.ctyp;
  value opt_mutable : Gram.Entry.t Ast.meta_bool;
  value opt_polyt : Gram.Entry.t Ast.ctyp;
  value opt_private : Gram.Entry.t Ast.meta_bool;
  value opt_rec : Gram.Entry.t Ast.meta_bool;
  value opt_virtual : Gram.Entry.t Ast.meta_bool;
  value opt_when_expr : Gram.Entry.t Ast.expr;
  value patt : Gram.Entry.t Ast.patt;
  value patt_as_patt_opt : Gram.Entry.t Ast.patt;
  value patt_eoi : Gram.Entry.t Ast.patt;
  value patt_quot : Gram.Entry.t Ast.patt;
  value patt_tcon : Gram.Entry.t Ast.patt;
  value phrase : Gram.Entry.t Ast.str_item;
  value poly_type : Gram.Entry.t Ast.ctyp;
  value row_field : Gram.Entry.t Ast.ctyp;
  value sem_expr : Gram.Entry.t Ast.expr;
  value sem_expr_for_list : Gram.Entry.t (Ast.expr -> Ast.expr);
  value sem_patt : Gram.Entry.t Ast.patt;
  value sem_patt_for_list : Gram.Entry.t (Ast.patt -> Ast.patt);
  value semi : Gram.Entry.t unit;
  value sequence : Gram.Entry.t Ast.expr;
  value do_sequence : Gram.Entry.t Ast.expr;
  value sig_item : Gram.Entry.t Ast.sig_item;
  value sig_item_quot : Gram.Entry.t Ast.sig_item;
  value sig_items : Gram.Entry.t Ast.sig_item;
  value star_ctyp : Gram.Entry.t Ast.ctyp;
  value str_item : Gram.Entry.t Ast.str_item;
  value str_item_quot : Gram.Entry.t Ast.str_item;
  value str_items : Gram.Entry.t Ast.str_item;
  value type_constraint : Gram.Entry.t unit;
  value type_declaration : Gram.Entry.t Ast.ctyp;
  value type_ident_and_parameters : Gram.Entry.t (string * list Ast.ctyp);
  value type_kind : Gram.Entry.t Ast.ctyp;
  value type_longident : Gram.Entry.t Ast.ident;
  value type_longident_and_parameters : Gram.Entry.t Ast.ctyp;
  value type_parameter : Gram.Entry.t Ast.ctyp;
  value type_parameters : Gram.Entry.t (Ast.ctyp -> Ast.ctyp);
  value typevars : Gram.Entry.t Ast.ctyp;
  value val_longident : Gram.Entry.t Ast.ident;
  value value_let : Gram.Entry.t unit;
  value value_val : Gram.Entry.t unit;
  value with_constr : Gram.Entry.t Ast.with_constr;
  value with_constr_quot : Gram.Entry.t Ast.with_constr;
  value prefixop : Gram.Entry.t Ast.expr;
  value infixop0 : Gram.Entry.t Ast.expr;
  value infixop1 : Gram.Entry.t Ast.expr;
  value infixop2 : Gram.Entry.t Ast.expr;
  value infixop3 : Gram.Entry.t Ast.expr;
  value infixop4 : Gram.Entry.t Ast.expr;
end;

(** A signature for syntax extension (syntax -> syntax functors). *)
module type SyntaxExtension = functor (Syn : Syntax)
                    -> (Syntax with module Loc            = Syn.Loc
                                and module Ast            = Syn.Ast
                                and module Token          = Syn.Token
                                and module Gram           = Syn.Gram
                                and module Quotation      = Syn.Quotation);

