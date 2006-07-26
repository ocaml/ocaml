#default_quotation "expr";

open Camlp4.PreCast;
open Format;

module FV = Camlp4.Struct.FreeVars.Make Ast;
module PP = Camlp4.Printers.OCaml.Make Syntax;
module S = FV.S;

value _loc = Loc.ghost;

value pervasives =
  let list =
    [ "+"; "-"; "/"; "*" (* ... *) ]
  in List.fold_right S.add list S.empty;

value f e =
  let fv = FV.free_vars pervasives e in
  S.fold (fun x acc -> << fun ~ $x$ -> $acc$ >>) fv e;

value print_expr = (new PP.printer ())#expr;

printf "%a@." print_expr (f <<let z = x + 2 in x + 2 * y * x * z>>);

