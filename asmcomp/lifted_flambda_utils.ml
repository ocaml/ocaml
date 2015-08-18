
(* let sets_of_closures ({ expr; set_of_closures_map } : Lift_constants.result) = *)
(*   let l = ref [] in *)
(*   let add_set_of_closures : Flambda.named -> unit = function *)
(*     | Set_of_closures set_of_closures -> *)
(*       (\* A set of closures should either exist as a non-constant version in *)
(*          the main expression, or as a constant version in the *)
(*          [set_of_closures_map], but never as both. *\) *)
(*       if (List.exists ( *)
(*           fun (set_of_closures' : Flambda.set_of_closures) -> *)
(*             Set_of_closures_id.equal *)
(*               set_of_closures.function_decls.set_of_closures_id *)
(*               set_of_closures'.function_decls.set_of_closures_id) *)
(*         !l) *)
(*       then begin *)
(*         Misc.fatal_errorf "Lifted_flambda_utils.sets_of_closures: set of \ *)
(*             closures ID %a appears both in the expression and in the \ *)
(*             symbol-to-constant-set-of-closures map" *)
(*           Set_of_closures_id.print *)
(*           set_of_closures.function_decls.set_of_closures_id *)
(*       end; *)
(*       l := set_of_closures :: !l *)
(*     | _ -> () *)
(*   in *)
(*   Flambda_iterators.iter_named add_set_of_closures expr; *)
(*   Symbol.Map.iter (fun _ set_of_closures -> *)
(*       Flambda_iterators.iter_named_on_named add_set_of_closures *)
(*         (Set_of_closures set_of_closures)) *)
(*     set_of_closures_map; *)
(*   !l *)

(* let set_of_closures_map lifted_flambda = *)
(*   sets_of_closures lifted_flambda *)
(*   |> List.map (fun set -> set.Flambda.function_decls.set_of_closures_id, set) *)
(*   |> Set_of_closures_id.Map.of_list *)

(* let constants_set_of_closures_id_set ({ set_of_closures_map } : Lift_constants.result) = *)
(*   Symbol.Map.fold *)
(*     (fun _ { Flambda.function_decls } set -> *)
(*        Set_of_closures_id.Set.add function_decls.set_of_closures_id set) *)
(*     set_of_closures_map Set_of_closures_id.Set.empty *)


let sets_of_closures _ =
  failwith "TODO"
let set_of_closures_map _ =
  failwith "TODO"
let constants_set_of_closures_id_set _ =
  failwith "TODO"
