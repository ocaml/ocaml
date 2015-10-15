
let rec push_initialisation_down (lam:Lambda.lambda) : Lambda.lambda =
  match lam with
  | Lsequence (
      Lprim (Psetglobalfield pos, [value]),
      Llet (kind, id, def, body)) ->
    (* Assumes that def does not use the initialised global *)
    Llet (kind, id, def,
          push_initialisation_down
            Lambda.(Lsequence (
                Lprim (Psetglobalfield pos, [value]),
                body)))
  | Lprim (Psetglobalfield _, [_]) -> lam
  | Lsequence (
      Lprim (Psetglobalfield _, [_]),
      _ ) ->
    lam
  | _ ->
    Format.eprintf "not handled in push: %a@." Printlambda.lambda lam;
    assert false

let rec get_initialisation_sequence (lam:Lambda.lambda) =
  match lam with
  | Lprim (Psetglobalfield pos, [value]) -> [pos, value]
  | Lsequence (
      Lprim (Psetglobalfield pos, [value]),
      expr ) ->
    (pos, value) :: get_initialisation_sequence expr
  | _ ->
    Format.eprintf "not handled: %a@." Printlambda.lambda lam;
    assert false

let deconstruct_initialisation_pattern (lam:Lambda.lambda) =
  match lam with
  | Lprim (Psetglobalfield pos, [value]) -> [pos, value]
  | Lsequence (
      Lprim (Psetglobalfield pos, [value]),
      expr ) ->
    (pos, value) :: get_initialisation_sequence expr
  | _ -> []

let rec iter_lambda f lam =
  f lam;
  Lambda.iter (iter_lambda f) lam

exception Found of int list

let find_initialisation lam =
  try
    iter_lambda (fun (lam:Lambda.lambda) ->
        match deconstruct_initialisation_pattern lam with
        | [] -> ()
        | l -> raise (Found (List.map fst l)))
      lam;
    None
  with Found n -> Some n

let substitute_initialisation_with_raise cont expr =
  let rec loop (lam:Lambda.lambda) : Lambda.lambda =
    match lam with
    | Lprim (Psetglobalfield _, [_])
    | Lsequence (Lprim (Psetglobalfield _, [_]), _ ) ->
      let lam = push_initialisation_down lam in
      let initialisations = get_initialisation_sequence lam in
      let initialisations =
        (* We sort such that if there are multiple points where the
           initialisation occurs, same variables will be bound to the
           same positions. *)
        List.sort (fun (p1, _) (p2, _) -> compare p1 p2) initialisations
      in
      Lstaticraise (cont, List.map snd initialisations)
    | Llet (kind, id, def, body) ->
      Llet (kind, id, loop def, loop body)
    | Lletrec (defs, body) ->
      Lletrec (List.map (fun (id, def) -> id, loop def) defs, loop body)
    | Lswitch (cond, sw) ->
      let sw =
        { sw with
          sw_consts = List.map (fun (i, lam) -> i, loop lam) sw.sw_consts;
          sw_blocks = List.map (fun (i, lam) -> i, loop lam) sw.sw_blocks;
          sw_failaction = Misc.may_map loop sw.sw_failaction }
      in
      Lswitch (loop cond, sw)
    | Lifthenelse (cond, ifso, ifnot) ->
      Lifthenelse (loop cond, loop ifso, loop ifnot)
    | Ltrywith (body, id, handler) ->
      Ltrywith (loop body, id, loop handler)
    | Lstringswitch (cond, branches, default) ->
      Lstringswitch
        (loop cond,
         List.map (fun (i, lam) -> i, loop lam) branches,
         Misc.may_map loop default)
    | Lstaticcatch (body, exn, handler) ->
      Lstaticcatch (loop body, exn, loop handler)
    | Lsequence (lam1, lam2) ->
      Lsequence (loop lam1, loop lam2)
    | Levent (lam, ev) ->
      Levent (loop lam, ev)
      (* Initialisation shouldn't appear in application argument
         evaluation of function definition, or in while or for *)
    | Lifused _
    | Lprim _
    | Lstaticraise _
    | Lwhile _
    | Lfor _
    | Lassign _
    | Lsend _
    | Lapply _
    | Lfunction _
    | Lconst _
    | Lvar _ -> lam
  in
  loop expr


type initialisation =
  | Field_initialisations of int list
  | Effect

let convert_setglobalfield lam =
  match find_initialisation lam with
  | None ->
    Format.eprintf "effect %a@." Printlambda.lambda lam;
    lam, Effect
  | Some l ->
    let cont = Lambda.next_raise_count () in
    let lam = substitute_initialisation_with_raise cont lam in
    let vars =
      List.map (fun p -> Ident.create ("catch_init_" ^ string_of_int p)) l
    in
    let get : Lambda.lambda =
      match vars with
      | [] -> assert false
      | [v] -> Lvar v
      | l -> Lprim (Pmakeblock (0, Immutable), List.map (fun v -> Lambda.Lvar v) l)
    in
    let lam : Lambda.lambda = Lstaticcatch (lam, (cont, vars), get) in
    Format.eprintf "setfields %a@." Printlambda.lambda lam;
    lam, Field_initialisations l

let sequence_to_list lam =
  let rec aux (lam:Lambda.lambda) acc =
    match lam with
    | Lsequence (lam1, lam2) ->
      let acc = aux lam2 acc in
      aux lam1 acc
    | Llet (kind, id, def, Lsequence (lam1, lam2)) ->
      (* Quadratic: we may need to find something cleaner *)
      if Lambda.IdentSet.mem id (Lambda.free_variables lam2)
      then lam :: acc
      else
        Lambda.Llet (kind, id, def, lam1) ::
        aux lam2 acc
    | lam ->
      lam :: acc
  in
  aux lam []

let split_module_initialization (lam:Lambda.lambda)
  : (Lambda.lambda * initialisation) list =
  let splited = sequence_to_list lam in
  Format.eprintf "splited @.%a@."
    (Format.pp_print_list Printlambda.lambda)
    splited;
  List.map convert_setglobalfield splited
