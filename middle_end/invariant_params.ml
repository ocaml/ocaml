(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

(* CR-someday pchambart to pchambart: in fact partial application doesn't
   work because there are no 'known' partial application left: they are
   converted to applications new partial function declaration.
   That can be improved (and many other cases) by keeping track of aliases in
   closure of functions. *)

(* A parameter [x] of the function [f] is considered as unchanging if during
   an 'external' (call from outside the set of closures) call of [f], every
   recursive call of [f] all the instances of [x] are aliased to the original
   one.

   This function computes an underapproximation of that set by computing the
   flow of parameters between the different function of the set of closures.
   We will write (f, x) <- (g, y) to denote that the parameter [x] of the
   function [f] can be an alias of the parameter [y] of the function [g].
   (f, x) <- Anything denote that unknown values can flow to [x].
   The '<-' relation is transitive.

   [x] is not unchanging if either
      (f, x) <- Anything
   or (f, x) <- (f, y) with x != y

   Notice that having (f, x) <- (g, a) and (f, x) <- (g, b) does not make
   x not unchanging. This is because (g, a) and (g, b) represent necessarily
   different values only if g is the externaly called function. If some
   value where created during the execution of the function that could
   flow to (g, a), then (g, a) <- Anything, so (f, x) <- Anything.

 *)

(* This is computed in two steps:
   * accumulate the atomic <- relations
   * compute the transitive closure

   We record [(f, x) <- Argument (g, y)] when the function g calls f and
   the y parameter of g is used as argument for the x parameter of f. For
   instance in

     let rec f x = ...
     and g y = f x

   We record [(f, x) <- Anything] when some unknown values can flow to a the
   [y] parameter.

     let rec f x = f 1

   We record also [(f, x) <- Anything] if [f] could escape. This is over
   approximated by considering that a function escape when its variable is used
   for something else than an application:

     let rec f x = (f, f)


  The <- relation is represented by the type

     t Variable.Pair.Map.t

  if [Variable.Pair.Set.mem (g, y) s] and
  [Argument s = Variable.Pair.Map.find (f, x) relation]
  then (f, x) <- (g, y) is in the relation.

*)

type t =
  | Anything
  | Arguments of Variable.Pair.Set.t

let _print ppf = function
  | Anything -> Format.fprintf ppf "Anything"
  | Arguments args ->
      Format.fprintf ppf "Arguments: @[<hv>%a@]"
        Variable.Pair.Set.print args

let transitive_closure state =
  let union s1 s2 =
    match s1, s2 with
    | Anything, _ | _, Anything -> Anything
    | Arguments s1, Arguments s2 -> Arguments (Variable.Pair.Set.union s1 s2)
  in
  let equal s1 s2 =
    match s1, s2 with
    | Anything, Arguments _ | Arguments _, Anything -> false
    | Anything, Anything -> true
    | Arguments s1, Arguments s2 -> Variable.Pair.Set.equal s1 s2
  in
  let update arg state =
    let original_set =
      try Variable.Pair.Map.find arg state with
      | Not_found -> Arguments Variable.Pair.Set.empty
    in
    match original_set with
    | Anything -> state
    | Arguments arguments ->
        let set =
          Variable.Pair.Set.fold
            (fun orig acc->
               let set =
                 try Variable.Pair.Map.find orig state with
                 | Not_found -> Arguments Variable.Pair.Set.empty in
               union set acc)
            arguments original_set
        in
        Variable.Pair.Map.add arg set state
  in
  let once state =
    Variable.Pair.Map.fold (fun arg _ state -> update arg state) state state
  in
  let rec fp state =
    let state' = once state in
    if Variable.Pair.Map.equal equal state state'
    then state
    else fp state'
  in
  fp state

(* CR-soon pchambart: to move to Flambda_utils and document
   mshinwell: I think this calculation is basically the same as
   [Flambda_utils.fun_vars_referenced_in_decls], so we should try
   to share code.  However let's defer until after 4.03.  (And note CR
   below.)
*)
(* Finds variables that represent the functions.
   In a construction like:
     let f x =
       let g = Symbol f_closure in
       ..
   the variable g is bound to the symbol f_closure which
   is the current closure.
   The result of [function_variable_alias] will contain
   the association [g -> f]
*)
let function_variable_alias
    (function_decls : Flambda.function_declarations)
    ~backend =
  let fun_vars = Variable.Map.keys function_decls.funs in
  let symbols_to_fun_vars =
    let module Backend = (val backend : Backend_intf.S) in
    Variable.Set.fold (fun fun_var symbols_to_fun_vars ->
        let closure_id = Closure_id.wrap fun_var in
        let symbol = Backend.closure_symbol closure_id in
        Symbol.Map.add symbol fun_var symbols_to_fun_vars)
      fun_vars
      Symbol.Map.empty
  in
  let fun_var_bindings = ref Variable.Map.empty in
  Variable.Map.iter (fun _ ( function_decl : Flambda.function_declaration ) ->
      Flambda_iterators.iter_all_toplevel_immutable_let_and_let_rec_bindings
        ~f:(fun var named ->
           (* CR-soon mshinwell: consider having the body passed to this
              function and using fv calculation instead of used_variables.
              Need to be careful of "let rec" *)
           match named with
           | Symbol sym ->
             begin match Symbol.Map.find sym symbols_to_fun_vars with
             | exception Not_found -> ()
             | fun_var ->
               fun_var_bindings :=
                 Variable.Map.add var fun_var !fun_var_bindings
             end
           | _ -> ())
        function_decl.body)
    function_decls.funs;
  !fun_var_bindings

let invariant_params_in_recursion (decls : Flambda.function_declarations)
      ~backend =
  let function_variable_alias = function_variable_alias ~backend decls in
  let escaping_functions = Variable.Tbl.create 13 in
  let relation = ref Variable.Pair.Map.empty in
  let param_indexes_by_fun_vars =
    Variable.Map.map (fun (decl : Flambda.function_declaration) ->
        Array.of_list decl.params)
      decls.funs
  in
  let link ~callee ~callee_arg ~caller ~caller_arg =
    let kind =
      try Variable.Pair.Map.find (callee, callee_arg) !relation with
      | Not_found -> Arguments Variable.Pair.Set.empty
    in
    match kind with
    | Anything -> ()
    | Arguments set ->
      relation :=
        Variable.Pair.Map.add (callee, callee_arg)
          (Arguments (Variable.Pair.Set.add (caller, caller_arg) set))
          !relation
  in
  let argument_may_be_anything ~callee ~callee_arg =
    relation := Variable.Pair.Map.add (callee, callee_arg) Anything !relation
  in
  let find_callee_arg ~callee ~callee_pos =
    match Variable.Map.find callee param_indexes_by_fun_vars with
    | exception Not_found -> None (* not a recursive call *)
    | arr ->
      (* Ignore overapplied parameters: they are applied to a different
         function. *)
      if callee_pos < Array.length arr then Some arr.(callee_pos)
      else None
  in
  (* If the called closure is in the current set of closures, record the
     relation (callee, callee_arg) <- (caller, caller_arg) *)
  let check_argument ~caller ~callee ~callee_pos ~caller_arg =
    match find_callee_arg ~callee ~callee_pos with
    | None -> () (* not a recursive call *)
    | Some callee_arg ->
      match Variable.Map.find caller decls.funs with
      | exception Not_found ->
        assert false
      | { params } ->
        (* We only track dataflow for parameters of functions, not
           arbitrary variables. *)
        if List.mem caller_arg params then
          link ~caller ~caller_arg ~callee ~callee_arg
        else
          argument_may_be_anything ~callee ~callee_arg
  in
  let test_escape var =
    let fun_var =
      match Variable.Map.find var function_variable_alias with
      | exception Not_found -> var
      | fun_var -> fun_var
    in
    if Variable.Map.mem fun_var decls.funs
    then Variable.Tbl.add escaping_functions fun_var ()
  in
  let arity ~callee =
    match Variable.Map.find callee decls.funs with
    | exception Not_found -> 0
    | func -> Flambda_utils.function_arity func
  in
  let check_expr ~caller (expr : Flambda.t) =
    match expr with
    | Apply { func; args } ->
      let callee =
        match Variable.Map.find func function_variable_alias with
        | exception Not_found -> func
        | callee -> callee
      in
      let num_args = List.length args in
      for callee_pos = num_args to (arity ~callee) - 1 do
        (* If a function is partially applied, consider all missing
           arguments as "anything". *)
        match find_callee_arg ~callee ~callee_pos with
        | None -> ()
        | Some callee_arg -> argument_may_be_anything ~callee ~callee_arg
      done;
      List.iteri (fun callee_pos caller_arg ->
          check_argument ~caller ~callee ~callee_pos ~caller_arg)
        args
    | _ -> ()
  in
  Variable.Map.iter (fun caller (decl : Flambda.function_declaration) ->
      Flambda_iterators.iter (check_expr ~caller)
        (fun (_ : Flambda.named) -> ())
        decl.body;
      Variable.Set.iter test_escape
        (* CR-soon mshinwell: we should avoid recomputing this, cache in
           [function_declaration].  See also comment on
           [only_via_symbols] in [Flambda_utils]. *)
        (Flambda.used_variables ~ignore_uses_as_callee:() decl.body))
    decls.funs;
  Variable.Map.iter (fun func_var
        ({ params } : Flambda.function_declaration) ->
      if Variable.Tbl.mem escaping_functions func_var then begin
        List.iter (fun param ->
            argument_may_be_anything ~callee:func_var ~callee_arg:param)
          params
      end)
    decls.funs;
  let result = transitive_closure !relation in
  let not_unchanging =
    Variable.Pair.Map.fold (fun (func, var) set not_unchanging ->
        match set with
        | Anything -> Variable.Set.add var not_unchanging
        | Arguments set ->
          if Variable.Pair.Set.exists (fun (func', var') ->
              Variable.equal func func' && not (Variable.equal var var'))
              set
          then Variable.Set.add var not_unchanging
          else not_unchanging)
      result Variable.Set.empty
  in
  let params = Variable.Map.fold (fun _
        ({ params } : Flambda.function_declaration) set ->
      Variable.Set.union (Variable.Set.of_list params) set)
    decls.funs Variable.Set.empty
  in
  let unchanging = Variable.Set.diff params not_unchanging in
  let aliased_to =
    Variable.Pair.Map.fold (fun (_, var) set aliases ->
        match set with
        | Arguments set
          when Variable.Set.mem var unchanging ->
            Variable.Pair.Set.fold (fun (_, caller_args) aliases ->
                if Variable.Set.mem caller_args unchanging then
                  let alias_set =
                    match Variable.Map.find caller_args aliases with
                    | exception Not_found ->
                      Variable.Set.singleton var
                    | alias_set ->
                      Variable.Set.add var alias_set
                  in
                  Variable.Map.add caller_args alias_set aliases
                else
                  aliases)
              set aliases
        | Anything | Arguments _ -> aliases)
      result Variable.Map.empty
  in
  (* We complete the set of aliases such that there does not miss any
     unchanging param *)
  Variable.Map.of_set (fun var ->
      match Variable.Map.find var aliased_to with
      | exception Not_found -> Variable.Set.empty
      | set -> set)
    unchanging

let pass_name = "unused-arguments"
let () = Clflags.all_passes := pass_name :: !Clflags.all_passes

type argument =
  | Used
  | Argument of Variable.t

let unused_arguments (decls : Flambda.function_declarations) : Variable.Set.t =
  let dump = Clflags.dumped_pass pass_name in
  let used_variables = Variable.Tbl.create 42 in
  let used_variable var = Variable.Tbl.add used_variables var () in
  let param_indexes_by_fun_vars =
    Variable.Map.fold (fun var (decl : Flambda.function_declaration) map ->
        let cid = Closure_id.wrap var in
        Closure_id.Map.add cid (Array.of_list decl.params) map)
      decls.funs Closure_id.Map.empty
  in
  let find_callee_arg ~callee ~callee_pos ~application_expr =
    match Closure_id.Map.find callee param_indexes_by_fun_vars with
    | exception Not_found -> Used (* not a recursive call *)
    | arr ->
      (* Direct calls don't have overapplication *)
      if callee_pos >= Array.length arr then begin
        Misc.fatal_errorf "Invariant_params.unused_arguments: direct calls \
            may not have overapplication: callee %a, application expr: %a, \
            function decls: %a"
          Closure_id.print callee
          Flambda.print application_expr
          Flambda.print_function_declarations decls
      end;
      Argument arr.(callee_pos)
  in
  let check_expr (expr : Flambda.t) =
    match expr with
    | Apply { func; args; kind = Direct callee } ->
      used_variable func;
      if dump then Format.printf "Used as direct function: %a@." Variable.print func;
      List.iteri (fun callee_pos arg ->
          match
            find_callee_arg ~callee ~callee_pos ~application_expr:expr
          with
          | Used ->
            if dump then Format.printf "Used as argument: %a@." Variable.print arg;
            used_variable arg
          | Argument param ->
            if not (Variable.equal arg param) then
              let () =
                if dump then Format.printf "Used as recursive arguments: %a \
                                            (not equal to %a)@."
                    Variable.print arg
                    Variable.print param
              in
              used_variable arg)
        args
    | Apply { func; args; kind = Indirect; _ } ->
      if dump then begin
        Format.printf "Used as indirect function: %a@." Variable.print func;
        List.iter (fun arg ->
            Format.printf "Used as indirect function argument: %a@." Variable.print arg)
        args;
      end;
      used_variable func;
      List.iter used_variable args
    | _ -> ()
  in
  Variable.Map.iter (fun _caller (decl : Flambda.function_declaration) ->
      Flambda_iterators.iter check_expr (fun (_ : Flambda.named) -> ())
        decl.body;
      let free_vars =
        Flambda.free_variables ~ignore_uses_as_callee:()
          ~ignore_uses_as_argument:() decl.body
      in
      Variable.Set.iter used_variable free_vars)
    decls.funs;
  let arguments =
    Variable.Map.fold
      (fun _ decl acc ->
         List.fold_left
           (fun acc param ->
              if Variable.Tbl.mem used_variables param then acc
              else Variable.Set.add param acc)
           acc decl.Flambda.params)
      decls.funs Variable.Set.empty
  in
  if dump then Format.printf "Unused arguments: %a@." Variable.Set.print arguments;
  arguments
