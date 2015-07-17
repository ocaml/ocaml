
let sets_of_closures ({ expr; set_of_closures_map } : Lift_constants.result) =
  let l = ref [] in
  let add_set_of_closures : Flambda.named -> unit = function
    | Set_of_closures set_of_closure ->
      l := set_of_closure :: !l
    | _ -> ()
  in
  Flambda_iterators.iter_named add_set_of_closures expr;
  Symbol.Map.iter (fun _ set_of_closures ->
      Flambda_iterators.iter_named_on_named add_set_of_closures
        (Set_of_closures set_of_closures))
    set_of_closures_map;
  !l

let set_of_closures_map lifted_flambda =
  sets_of_closures lifted_flambda
  |> List.map (fun set -> set.Flambda.function_decls.set_of_closures_id, set)
  |> Set_of_closures_id.Map.of_list

let constants_set_of_closures_id_set ({ set_of_closures_map } : Lift_constants.result) =
  Symbol.Map.fold
    (fun _ { Flambda.function_decls } set ->
       Set_of_closures_id.Set.add function_decls.set_of_closures_id set)
    set_of_closures_map Set_of_closures_id.Set.empty
