(* TEST
   modules = "stubs.c"
*)

external init_skiplist : unit -> unit = "init_skiplist"
external hammer_skiplist : int -> unit = "hammer_skiplist"

let () =
   init_skiplist ();
   let domains_list = List.init 4 (fun i -> Domain.spawn (fun () -> hammer_skiplist i)) in
      ignore(List.iter Domain.join domains_list)
