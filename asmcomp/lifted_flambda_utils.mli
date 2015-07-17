
(** All declared sets of closures *)
val sets_of_closures : Lift_constants.result -> Flambda.set_of_closures list

val set_of_closures_map : Lift_constants.result -> Flambda.set_of_closures Set_of_closures_id.Map.t

val constants_set_of_closures_id_set : Lift_constants.result -> Set_of_closures_id.Set.t
