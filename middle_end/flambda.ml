(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*           Xavier Leroy, projet Cristal, INRIA Rocquencourt             *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type let_kind =
  | Immutable
  | Mutable

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  | Int of int
  | Char of char
  | Const_pointer of int

type apply = {
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
}

type assign = {
  being_assigned : Variable.t;
  new_value : Variable.t;
}

type send = {
  kind : Lambda.meth_kind;
  meth : Variable.t;
  obj : Variable.t;
  args : Variable.t list;
  dbg : Debuginfo.t;
}

type project_closure = {
  set_of_closures : Variable.t;
  closure_id : Closure_id.t;
}

type move_within_set_of_closures = {
  closure : Variable.t;
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

type project_var = {
  closure : Variable.t;
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

type t =
  | Var of Variable.t
  | Let of let_kind * Variable.t * named * t
  | Let_rec of (Variable.t * named) list * t
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  | String_switch of Variable.t * (string * t) list * t option
  | Static_raise of Static_exception.t * t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

and named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Predefined_exn of Ident.t
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t

and set_of_closures = {
  function_decls : function_declarations;
  free_vars : Variable.t Variable.Map.t;
  specialised_args : Variable.t Variable.Map.t;
}

and function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  funs : function_declaration Variable.Map.t;
  compilation_unit : Compilation_unit.t;
}

and function_declaration = {
  params : Variable.t list;
  body : t;
  free_variables : Variable.Set.t;
  stub : bool;
  dbg : Debuginfo.t;
}

and switch = {
  numconsts : Ext_types.Int.Set.t;
  consts : (int * t) list;
  numblocks : Ext_types.Int.Set.t;
  blocks : (int * t) list;
  failaction : t option;
}

and for_loop = {
  bound_var : Variable.t;
  from_value : Variable.t;
  to_value : Variable.t;
  direction : Asttypes.direction_flag;
  body : t
}

and constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures  (* [free_vars] must be empty *)
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

type program =
  | Let_symbol of Symbol.t * constant_defining_value * program
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program
  | Import_symbol of Symbol.t * program
  | Initialize_symbol of Symbol.t * Tag.t * t list * program
  | Effect of t * program
  | End of Symbol.t

let fprintf = Format.fprintf
module Int = Ext_types.Int

let rec lam ppf (flam : t) =
  match flam with
  | Var (id) ->
      Variable.print ppf id
  | Apply({func; args; kind}) ->
    let direct ppf () =
      match kind with
      | Indirect -> ()
      | Direct closure_id -> fprintf ppf "*[%a]" Closure_id.print closure_id
    in
    fprintf ppf "@[<2>(apply%a@ %a%a)@]" direct () Variable.print func
      Variable.print_list args
  | Assign { being_assigned; new_value; } ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]"
      Variable.print being_assigned
      Variable.print new_value
  | Send { kind; meth; obj; args; dbg = _; } ->
    let print_args ppf args =
      List.iter (fun l -> fprintf ppf "@ %a" Variable.print l) args
    in
    let kind =
      match kind with
      | Self -> "self"
      | Public -> "public"
      | Cached -> "cached"
    in
    fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind
      Variable.print obj Variable.print meth
      print_args args
  | Proved_unreachable ->
      fprintf ppf "unreachable"
  | Let(_str, id, arg, body) ->
      let rec letbody (ul : t) =
        match ul with
        | Let(str, id, arg, body) ->
            let str = match str with
              | Mutable -> "*"
              | Immutable -> ""
            in
            fprintf ppf "@ @[<2>%a%s@ %a@]" Variable.print id str print_named arg;
            letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]"
        Variable.print id print_named arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Let_rec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<2>%a@ %a@]" Variable.print id print_named l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Switch(larg, sw) ->
      let switch ppf (sw : switch) =
        let spc = ref false in
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
          sw.consts;
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.blocks ;
        begin match sw.failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
        "@[<1>(%s(%i,%i) %a@ @[<v 0>%a@])@]"
        (match sw.failaction with None -> "switch*" | _ -> "switch")
        (Int.Set.cardinal sw.numconsts)
        (Int.Set.cardinal sw.numblocks)
        Variable.print larg switch sw
  | String_switch(arg, cases, default) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" Variable.print arg switch cases
  | Static_raise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %a%a)@]" Static_exception.print i lams ls;
  | Static_catch(i, vars, lbody, lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%a%a)@ %a)@]"
        lam lbody Static_exception.print i
        (fun ppf vars -> match vars with
           | [] -> ()
           | _ ->
               List.iter
                 (fun x -> fprintf ppf " %a" Variable.print x)
                 vars)
        vars
        lam lhandler
  | Try_with(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Variable.print param lam lhandler
  | If_then_else(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ then begin@ %a@ end else begin@ %a@ end)@]"
        Variable.print lcond
        lam lif lam lelse
  | While(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | For { bound_var; from_value; to_value; direction; body; } ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
      Variable.print bound_var Variable.print from_value
      (match direction with
        Asttypes.Upto -> "to" | Asttypes.Downto -> "downto")
      Variable.print to_value lam body
and print_named ppf (named : named) =
  match named with
  | Symbol (symbol) -> Symbol.print ppf symbol
  | Const (cst) -> fprintf ppf "Const(%a)" print_const cst
  | Allocated_const (cst) -> fprintf ppf "Aconst(%a)" Allocated_const.print cst
  | Predefined_exn ident -> fprintf ppf "Predef_exn(%a)" Ident.print ident
  | Project_closure (project_closure) ->
    print_project_closure ppf project_closure
  | Project_var (project_var) -> print_project_var ppf project_var
  | Move_within_set_of_closures (move_within_set_of_closures) ->
    print_move_within_set_of_closures ppf move_within_set_of_closures
  | Set_of_closures (set_of_closures) ->
    print_set_of_closures ppf set_of_closures
  | Prim(prim, args, _) ->
    fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim
      Variable.print_list args
  | Expr expr ->
    fprintf ppf "*%a" lam expr
    (* lam ppf expr *)

and print_function_declaration ppf var (f : function_declaration) =
  let idents ppf =
    List.iter (fprintf ppf "@ %a" Variable.print) in
  fprintf ppf "@ (%a@ =@ fun@[<2>%a@] ->@ @[<2>%a@])"
    Variable.print var idents f.params lam f.body

and print_set_of_closures ppf (set_of_closures : set_of_closures) =
  match set_of_closures with
  | { function_decls; free_vars; specialised_args} ->
    let funs ppf =
      Variable.Map.iter (print_function_declaration ppf)
    in
    let vars ppf =
      Variable.Map.iter (fun id v -> fprintf ppf "@ %a -rename-> %a"
                      Variable.print id Variable.print v) in
    let spec ppf spec_args =
      if not (Variable.Map.is_empty spec_args)
      then begin
        fprintf ppf "@ ";
        Variable.Map.iter (fun id id' -> fprintf ppf "@ %a := %a"
                        Variable.print id Variable.print id')
          spec_args
      end
    in
    fprintf ppf "@[<2>(set_of_closures id=%a %a@ free_vars={%a@ }@ \
        specialised_args={%a})@]"
      Set_of_closures_id.print function_decls.set_of_closures_id
      funs function_decls.funs
      vars free_vars spec specialised_args

and print_project_closure ppf (project_closure : project_closure) =
  fprintf ppf "@[<2>(project_closure@ %a@ from@ %a)@]"
    Closure_id.print project_closure.closure_id
    Variable.print project_closure.set_of_closures

and print_move_within_set_of_closures ppf
      (move_within_set_of_closures : move_within_set_of_closures) =
  fprintf ppf "@[<2>(move_within_set_of_closures@ %a - %a@ %a)@]"
    Closure_id.print move_within_set_of_closures.move_to
    Closure_id.print move_within_set_of_closures.start_from
    Variable.print move_within_set_of_closures.closure

and print_project_var ppf (project_var : project_var) =
  fprintf ppf "@[<2>(project_var@ %a@ from %a=%a)@]"
    Var_within_closure.print project_var.var
    Closure_id.print project_var.closure_id
    Variable.print project_var.closure

and print_const ppf (c : const) =
  match c with
  | Int n -> fprintf ppf "%i" n
  | Char c -> fprintf ppf "%C" c
  | Const_pointer n -> fprintf ppf "%ia" n

let print_function_declarations ppf (fd : function_declarations) =
  let funs ppf =
    Variable.Map.iter (print_function_declaration ppf)
  in
  fprintf ppf "@[<2>(%a)@]" funs fd.funs

let print ppf flam =
  fprintf ppf "%a@." lam flam

let print_function_declaration ppf (var, decl) =
  print_function_declaration ppf var decl

let print_constant_defining_value ppf (const : constant_defining_value) =
  match const with
  | Allocated_const const ->
    fprintf ppf "Allocated_const (%a)" Allocated_const.print const
  | Block (tag, []) -> fprintf ppf "Atom (tag %d)" (Tag.to_int tag)
  | Block (tag, fields) ->
    let print_field ppf (field : constant_defining_value_block_field) =
      match field with
      | Symbol symbol -> Symbol.print ppf symbol
      | Const const -> print_const ppf const
    in
    let print_fields ppf =
      List.iter (fprintf ppf "@ %a" print_field)
    in
    fprintf ppf "Block (tag %d, %a)" (Tag.to_int tag)
      print_fields fields
  | Set_of_closures set_of_closures ->
    fprintf ppf "Set_of_closures (%a)" print_set_of_closures set_of_closures
  | Project_closure (set_of_closures, closure_id) ->
    fprintf ppf "Project_closure (%a, %a)" Symbol.print set_of_closures
      Closure_id.print closure_id

let rec print_program ppf (program : program) =
  match program with
  | Let_symbol (symbol, constant_defining_value, body) ->
    (* CR mshinwell: share with above *)
    let rec letbody (ul : program) =
      match ul with
      | Let_symbol (symbol, constant_defining_value, body) ->
        fprintf ppf "@ @[<2>%a@ %a@]" Symbol.print symbol
          print_constant_defining_value constant_defining_value;
        letbody body
      | _ -> ul
    in
    fprintf ppf "let@ @[<hv 1>(@[<2>%a@ %a@])@]@ "
      Symbol.print symbol
      print_constant_defining_value constant_defining_value;
    let program = letbody body in
    print_program ppf program
  | Let_rec_symbol (defs, program) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter
        (fun (symbol, constant_defining_value) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<2>%a@ %a@]"
             Symbol.print symbol
             print_constant_defining_value constant_defining_value)
        id_arg_list in
    fprintf ppf
      "@[<2>letrec@ (@[<hv 1>%a@])@]@ "
      bindings defs;
    print_program ppf program
  | Import_symbol (symbol, program) ->
    fprintf ppf "@[import_symbol@ %a@]@ " Symbol.print symbol;
    print_program ppf program
  | Initialize_symbol (symbol, tag, fields, program) ->
    fprintf ppf "@[<2>let_global@ @[<hv 1>(@[<2>%a@ %a@ %a@])@]@]@ "
      Symbol.print symbol
      Tag.print tag
      (Format.pp_print_list lam) fields;
    print_program ppf program
  | Effect (expr, program) ->
    fprintf ppf "@[effect @[<hv 1>%a@]@@]@ "
      lam expr;
    print_program ppf program;
  | End root -> fprintf ppf "End %a" Symbol.print root

(* CR mshinwell: this doesn't seem to cope with shadowed identifiers
   properly.  Check the original version.  Why don't we just do the
   subtraction as we pass back over binding points? *)

let iter ?ignore_uses_in_apply ?ignore_uses_in_project_var tree
    ~free_variable ~bound_variable
    ~enter_let ~leave_let_definition ~leave_let_body =
  let rec aux (flam : t) : unit =
    match flam with
    | Var var -> free_variable var
    | Apply { func; args; kind = _; dbg = _} ->
      begin match ignore_uses_in_apply with
      | None ->
        free_variable func
      | Some () -> ()
      end;
      List.iter free_variable args
    | Let (_, var, defining_expr, body) ->
      let acc = enter_let var in
      bound_variable var;
      aux_named defining_expr;
      let acc = leave_let_definition acc in
      aux body;
      leave_let_body acc
    | Let_rec (bindings, body) ->
      List.iter (fun (var, defining_expr) ->
          bound_variable var;
          aux_named defining_expr)
        bindings;
      aux body
    | Switch (scrutinee, switch) ->
      free_variable scrutinee;
      List.iter (fun (_, e) -> aux e) switch.consts;
      List.iter (fun (_, e) -> aux e) switch.blocks;
      Misc.may aux switch.failaction
    | String_switch (scrutinee, cases, failaction) ->
      free_variable scrutinee;
      List.iter (fun (_, e) -> aux e) cases;
      Misc.may aux failaction
    | Static_raise (_, es) ->
      List.iter aux es
    | Static_catch (_, vars, e1, e2) ->
      List.iter bound_variable vars;
      aux e1;
      aux e2
    | Try_with (e1, var, e2) ->
      aux e1;
      bound_variable var;
      aux e2
    | If_then_else (var, e1, e2) ->
      free_variable var;
      aux e1;
      aux e2
    | While (e1, e2) ->
      aux e1;
      aux e2
    | For { bound_var; from_value; to_value; direction = _; body; } ->
      bound_variable bound_var;
      free_variable from_value;
      free_variable to_value;
      aux body
    | Assign { being_assigned; new_value; } ->
      free_variable being_assigned;
      free_variable new_value
    | Send { kind = _; meth; obj; args; dbg = _ } ->
      free_variable meth;
      free_variable obj;
      List.iter free_variable args;
    | Proved_unreachable -> ()
  and aux_named (named : named) =
    match named with
    | Symbol _ | Const _ | Allocated_const _ | Predefined_exn _ -> ()
    | Set_of_closures { free_vars; specialised_args; _ } ->
      (* Sets of closures are, well, closed---except for the specialised
         argument list, which may identify variables currently in scope
         outside of the closure. *)
      Variable.Map.iter (fun _ renamed_to -> free_variable renamed_to)
        free_vars;
      Variable.Map.iter (fun _ var -> free_variable var) specialised_args
    | Project_closure { set_of_closures; closure_id = _ } ->
      free_variable set_of_closures
    | Project_var { closure; closure_id = _; var = _ } ->
      begin match ignore_uses_in_project_var with
      | None -> free_variable closure
      | Some () -> ()
      end
    | Move_within_set_of_closures { closure; start_from = _; move_to = _ } ->
      free_variable closure
    | Prim (_, args, _) -> List.iter free_variable args
    | Expr flam -> aux flam
  in
  aux tree

let free_variables ?ignore_uses_in_apply ?ignore_uses_in_project_var tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let free_variable id = free := Variable.Set.add id !free in
  let bound_variable id = bound := Variable.Set.add id !bound in
  let enter_let _var = () in
  let leave_let_definition () = () in
  let leave_let_body () = () in
  iter ?ignore_uses_in_apply ?ignore_uses_in_project_var tree
    ~free_variable ~bound_variable
    ~enter_let ~leave_let_definition ~leave_let_body;
  Variable.Set.diff !free !bound

let free_variables_named tree =
  let var = Variable.create "dummy" in
  free_variables (Let (Immutable, var, tree, Var var))

let free_variables_by_let ?ignore_uses_in_apply ?ignore_uses_in_project_var tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let map = ref Variable.Map.empty in
  let free_variable id = free := Variable.Set.add id !free in
  let bound_variable id = bound := Variable.Set.add id !bound in
  let enter_let var =
    var
  in
  let leave_let_definition var =
    let current_free = !free in
    let current_bound = !bound in
    free := Variable.Set.empty;
    bound := Variable.Set.empty;
    var, current_free, current_bound
  in
  let leave_let_body (var, previous_free, previous_bound) =
    let new_free = !free in
    let new_bound = !bound in
    let var_really_free = Variable.Set.diff new_free new_bound in
    map := Variable.Map.add var var_really_free !map;
    free := Variable.Set.union new_free previous_free;
    bound := Variable.Set.union new_bound previous_bound
  in
  iter ?ignore_uses_in_apply ?ignore_uses_in_project_var tree
    ~free_variable ~bound_variable
    ~enter_let ~leave_let_definition ~leave_let_body;
  !map

let create_function_declaration ~params ~body ~stub ~dbg
      : function_declaration =
  { params;
    body;
    free_variables = free_variables body;
    stub;
    dbg;
  }

let create_set_of_closures ~function_decls ~free_vars ~specialised_args =
  let all_fun_vars = Variable.Map.keys function_decls.funs in
  let expected_free_vars =
    Variable.Map.fold (fun _fun_var function_decl expected_free_vars ->
        let free_vars =
          Variable.Set.diff function_decl.free_variables
            (Variable.Set.union (Variable.Set.of_list function_decl.params)
              all_fun_vars)
        in
        Variable.Set.union free_vars expected_free_vars)
      function_decls.funs
      Variable.Set.empty
  in
  let free_vars =
    (* CR mshinwell for pchambart: Is this ok, or should we cause an error?
       I tend to think this one is ok, but for specialised_args below, we
       should be strict. *)


    (* CR pchambart: We do not seem to be able to maintain the
       invariant that if a variable is not used inside the closure, it
       is not used outside either. This would be a nice property for
       better dead code elimination during inline_and_simplify, but it
       is not obvious how to ensure that.

       This would be true when the function is known never to have
       been inlined.

       Note that something like that may maybe enforcable in
       inline_and_simplify, but there is no way to do that on other
       passes. *)

    (* Variable.Map.filter (fun inner_var _outer_var -> *)
    (*     Variable.Set.mem inner_var expected_free_vars) *)
    (*   free_vars *)

    free_vars

  in
  let free_vars_domain = Variable.Map.keys free_vars in
  if not (Variable.Set.subset expected_free_vars free_vars_domain) then begin
    Misc.fatal_errorf "create_set_of_closures: [free_vars] mapping of \
        variables bound by the closure(s) is wrong.  (%a, expected to be a \
        subset of %a)@ \n%s\nfunction_decls:@ %a"
      Variable.Set.print expected_free_vars
      Variable.Set.print free_vars_domain
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
      print_function_declarations function_decls
  end;
  let all_params =
    Variable.Map.fold (fun _fun_var function_decl all_params ->
        Variable.Set.union (Variable.Set.of_list function_decl.params)
          all_params)
      function_decls.funs
      Variable.Set.empty
  in
  let spec_args_domain = Variable.Map.keys specialised_args in
  if not (Variable.Set.subset spec_args_domain all_params) then begin
    Misc.fatal_errorf "create_set_of_closures: [specialised_args] \
        maps variable(s) that are not parameters of the given function \
        declarations.  specialised_args domain=%a all_params=%a \n\
        function_decls:@ %a"
      Variable.Set.print spec_args_domain
      Variable.Set.print all_params
      print_function_declarations function_decls
  end;
  { function_decls;
    free_vars;
    specialised_args;
  }

let used_params function_decl =
  Variable.Set.filter
    (fun param -> Variable.Set.mem param function_decl.free_variables)
    (Variable.Set.of_list function_decl.params)

let compare_const (c1:const) (c2:const) =
  match c1, c2 with
  | Int i1, Int i2 -> compare i1 i2
  | Char i1, Char i2 -> compare i1 i2
  | Const_pointer i1, Const_pointer i2 -> compare i1 i2
  | Int _, (Char _ | Const_pointer _) -> -1
  | (Char _ | Const_pointer _), Int _ -> 1
  | Char _, Const_pointer _ -> -1
  | Const_pointer _, Char _ -> 1

let compare_constant_defining_value_block_field
    (c1:constant_defining_value_block_field)
    (c2:constant_defining_value_block_field) =
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
  | Const c1, Const c2 -> compare_const c1 c2
  | Symbol _, Const _ -> -1
  | Const _, Symbol _ -> 1

module Constant_defining_value = struct
  module T = struct
    type t = constant_defining_value

    let compare (t1 : t) (t2 : t) =
      match t1, t2 with
      | Allocated_const c1, Allocated_const c2 ->
        Allocated_const.compare c1 c2
      | Block (tag1, fields1), Block (tag2, fields2) ->
        let c = Tag.compare tag1 tag2 in
        if c <> 0 then c
        else
          Misc.compare_lists compare_constant_defining_value_block_field
            fields1 fields2
      | Set_of_closures set1, Set_of_closures set2 ->
        Set_of_closures_id.compare set1.function_decls.set_of_closures_id
          set2.function_decls.set_of_closures_id
      | Project_closure (set1, closure_id1),
          Project_closure (set2, closure_id2) ->
        let c = Symbol.compare set1 set2 in
        if c <> 0 then c
        else Closure_id.compare closure_id1 closure_id2
      | Allocated_const _, Block _ -> -1
      | Allocated_const _, Set_of_closures _ -> -1
      | Allocated_const _, Project_closure _ -> -1
      | Block _, Allocated_const _ -> 1
      | Block _, Set_of_closures _ -> -1
      | Block _, Project_closure _ -> -1
      | Set_of_closures _, Allocated_const _ -> 1
      | Set_of_closures _, Block _ -> 1
      | Set_of_closures _, Project_closure _ -> -1
      | Project_closure _, Allocated_const _ -> 1
      | Project_closure _, Block _ -> 1
      | Project_closure _, Set_of_closures _ -> 1

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash = Hashtbl.hash

    let print = print_constant_defining_value

    let output o v =
      output_string o (Format.asprintf "%a" print v)

  end

  include T
  module Identifiable = Ext_types.Identifiable.Make(T)
  include Identifiable
end
