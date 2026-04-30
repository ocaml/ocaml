(* TEST *)
open Printf
open Bigarray

let () =
  let ba = Array1.create int c_layout 0 in
  match reshape (genarray_of_array1 ba) (Array.init 8 (fun _ -> 1 lsl 16)) with
  | ba ->
     printf "out of bounds read: %d\n"
       (Genarray.get ba (Array.init 8 (fun _ -> 10)))
  | exception exn ->
     printf "ok: %s\n" (Printexc.to_string exn)
