(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Project_closure_index = struct
  type t = {
    arity_of_first_function : int;
    closure_index : int;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf { arity_of_first_function; closure_index; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(arity_of_first_function@ %d)@]@ \
          @[<hov 1>(closure_index@ %d)@]\
          )@]"
        arity_of_first_function
        closure_index

    let compare
          { arity_of_first_function = arity_of_first_function1;
            closure_index = closure_index1;
          }
          { arity_of_first_function = arity_of_first_function2;
            closure_index = closure_index2;
          } =
      let c =
        Numbers.Int.compare arity_of_first_function1 arity_of_first_function2
      in
      if c <> 0 then c
      else Numbers.Int.compare closure_index1 closure_index2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash _t = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Closure_index = struct
  type t = {
    arity : int;
    closure_index : int;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf { arity; closure_index; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(arity@ %d)@]@ \
          @[<hov 1>(closure_index@ %d)@]\
          )@]"
        arity
        closure_index

    let compare
          { arity = arity1;
            closure_index = closure_index1;
          }
          { arity = arity2;
            closure_index = closure_index2;
          } =
      let c =
        Numbers.Int.compare arity1 arity2
      in
      if c <> 0 then c
      else Numbers.Int.compare closure_index1 closure_index2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash _t = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

type result = {
  project_closure_indexes : Project_closure_index.t Closure_id.Map.t;
  move_within_set_of_closures_indexes : Closure_index.t Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t Closure_id.Map.t;
}

let add_closure_offsets
      { project_closure_indexes; move_within_set_of_closures_indexes;
        free_variable_offsets; }
      ({ function_decls; free_vars } : Flambda.set_of_closures) =
  let num_decls = Variable.Map.cardinal function_decls.funs in
  (* Compute closure ordering and thus which functions' closure is to be the
     "first" one in each set.  The pointers to the "first" ones will become the
     "set of closures" pointers (on which [Project_closure]s operate). *)
  let assign_offsets id function_decl
        (project_closure_indexes, move_within_set_of_closures_indexes,
          first_closure_var_offsets, closure_index) =
    let arity = Flambda_utils.function_arity function_decl in
    let first_closure_var_offset =
      2 (* code pointer (or currying wrapper) and arity *)
      + (if arity > 1 then 1 else 0)  (* full application code pointer *)
      + (num_decls - 1)  (* pointers to all other closures in the same set *)
    in
    let closure_id = Closure_id.wrap id in
    if Closure_id.Map.mem closure_id project_closure_indexes
      || Closure_id.Map.mem closure_id move_within_set_of_closures_indexes
      || Closure_id.Map.mem closure_id first_closure_var_offsets
    then begin
      Misc.fatal_errorf "Closure_offsets.add_closure_offsets: function \
          offset for %a would be defined multiple times"
        Closure_id.print closure_id
    end;
    let arity_of_first_function =
      let _fun_var, func_decl = Variable.Map.min_binding function_decls.funs in
      Flambda_utils.function_arity func_decl
    in
    let project_closure_indexes =
      Closure_id.Map.add closure_id
        { Project_closure_index. arity_of_first_function; closure_index; }
        project_closure_indexes
    in
    let move_within_set_of_closures_indexes =
      Closure_id.Map.add closure_id
        { Closure_index. arity; closure_index; }
        move_within_set_of_closures_indexes
    in
    let first_closure_var_offsets =
      Closure_id.Map.add closure_id first_closure_var_offset
        first_closure_var_offsets
    in
    project_closure_indexes, move_within_set_of_closures_indexes,
      first_closure_var_offsets, closure_index + 1
  in
  let project_closure_indexes, move_within_set_of_closures_indexes,
      first_closure_var_offsets, _closure_index =
    Variable.Map.fold assign_offsets
      function_decls.funs
      (project_closure_indexes, move_within_set_of_closures_indexes,
        Closure_id.Map.empty, 0)
  in
  (* Adds the mapping of free variables to their offset.  Recall that
     projections of [Var_within_closure]s are only currently used when
     compiling accesses to the closure of a function from outside that
     function (in particular, as a result of inlining).  Accesses to
     a function's own closure are compiled directly via normal [Var]
     accesses. *)
  (* CR-someday mshinwell: As discussed with lwhite, maybe this isn't
     ideal, and the self accesses should be explicitly marked too.
     mshinwell: fixed in Flambda 2.0 *)
  let free_variable_offsets' =
    Closure_id.Map.mapi (fun closure_id first_closure_var_offset ->
        let offsets_this_closure, _ =
          Variable.Map.fold (fun var _ (offsets_this_closure, offset) ->
              let var_within_closure = Var_within_closure.wrap var in
              if Var_within_closure.Map.mem var_within_closure
                   offsets_this_closure
              then begin
                Misc.fatal_errorf "Closure_offsets.add_closure_offsets: free \
                    variable offset for %a in closure %a would be defined \
                    multiple times"
                  Var_within_closure.print var_within_closure
                  Closure_id.print closure_id
              end;
              let offsets_this_closure =
                Var_within_closure.Map.add var_within_closure offset
                  offsets_this_closure
              in
              offsets_this_closure, offset + 1)
            free_vars
            (Var_within_closure.Map.empty, first_closure_var_offset)
        in
        offsets_this_closure)
      first_closure_var_offsets
  in
  let free_variable_offsets =
    Closure_id.Map.disjoint_union free_variable_offsets free_variable_offsets'
  in
  { project_closure_indexes;
    move_within_set_of_closures_indexes;
    free_variable_offsets;
  }

let compute (program:Flambda.program) =
  let init : result =
    { project_closure_indexes = Closure_id.Map.empty;
      move_within_set_of_closures_indexes = Closure_id.Map.empty;
      free_variable_offsets = Closure_id.Map.empty;
    }
  in
  let r =
    List.fold_left add_closure_offsets
      init (Flambda_utils.all_sets_of_closures program)
  in
  r
