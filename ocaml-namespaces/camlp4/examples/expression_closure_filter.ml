(* camlp4r *)
#default_quotation "expr";

open Camlp4.PreCast;
open Format;

module FV = Camlp4.Struct.FreeVars.Make Ast;
module S = FV.S;

value _loc = Loc.ghost;

value pervasives =
  let list =
    [ "+"; "-"; "/"; "*" (* ... *) ]
  in List.fold_right S.add list S.empty;

value collect_free_vars_sets =
  object (self)
    inherit FV.fold_free_vars [S.t] S.add ~env_init:pervasives S.empty as super;
    value free_sets = [];
    method set_free free = {< free = free >};
    method expr =
      fun
      [ << close_expr $e$ >> -> (self#expr e)#add_current_free#set_free free
      | e -> super#expr e ];
    method add_current_free = {< free_sets = [ free :: free_sets ] >};
    method free_sets = free_sets;
  end;

value apply_close_expr next_free_set =
  object (self)
    inherit Ast.map as super;
    method expr =
      fun
      [ << close_expr $e$ >> ->
          let e = self#expr e in
          let fv = next_free_set () in
          S.fold (fun x acc -> << fun ~ $x$ -> $acc$ >>) fv e
      | e -> super#expr e ];
  end;

value f st =
  let fv_sets = ref (collect_free_vars_sets#str_item st)#free_sets in
  let next_free_set () =
    match fv_sets.val with
    [ [] -> assert False
    | [x::xs] -> let () = fv_sets.val := xs in x ]
  in (apply_close_expr next_free_set)#str_item st;

AstFilters.register_str_item_filter f;
