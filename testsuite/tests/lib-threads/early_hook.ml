let f = ref ignore

let () = Domain.at_each_spawn (fun () -> !f ())
