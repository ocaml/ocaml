(* TEST *)

(* Tests that an exception in the alloc_minor callback propagates
   correctly to the top level. *)

exception MyExc of string

module MP = Gc.Memprof

let _ =

try
   let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
                  { MP.null_tracker with
                      alloc_minor =
                        fun _ -> raise (MyExc "alloc_minor callback") } in
     (ignore (Sys.opaque_identity (ref (ref 42)));
      MP.stop ())
with
  MyExc s -> (MP.stop();
              Printf.printf "Exception from %s.\n" s)
