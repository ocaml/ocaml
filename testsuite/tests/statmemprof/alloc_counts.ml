(* TEST *)
module MP = Gc.Memprof

let allocs_by_memprof f =
  let minor = ref 0 in
  let major = ref 0 in
  let alloc_minor (info : MP.allocation) =
    minor := !minor + info.n_samples;
    None in
  let alloc_major (info : MP.allocation) =
    major := !major + info.n_samples;
    None in
  MP.start ~sampling_rate:1. ({MP.null_tracker with alloc_minor; alloc_major});
  match Sys.opaque_identity f () with
  | _ -> MP.stop (); (!minor, !major)
  | exception e -> MP.stop (); raise e

let allocs_by_counters f =
  let minor1, prom1, major1 = Gc.counters () in
  let minor2, prom2, major2 = Gc.counters () in
  ignore (Sys.opaque_identity f ());
  let minor3, prom3, major3 = Gc.counters () in
  let minor =
    minor3 -. minor2      (* allocations *)
    -. (minor2 -. minor1) (* Gc.counters overhead *)
  in
  let prom =
    prom3 -. prom2 -. (prom2 -. prom1) in
  let major =
    major3 -. major2 -. (major2 -. major1) in
  int_of_float minor,
  int_of_float (major -. prom)

let compare name f =
  let mp_minor, mp_major = allocs_by_memprof f in
  let ct_minor, ct_major = allocs_by_counters f in
  if mp_minor <> ct_minor || mp_major <> ct_major then
    Printf.printf "%20s: minor: %d / %d; major: %d / %d\n"
      name ct_minor mp_minor ct_major mp_major

let many f =
  fun () ->
  for i = 1 to 10 do
    ignore (Sys.opaque_identity f ())
  done

let () =
  compare "ref" (many (fun () -> ref (ref (ref 42))));
  compare "short array" (many (fun () -> Array.make 10 'a'));
  compare "long array" (many (fun () -> Array.make 1000 'a'));
  compare "curried closure" (many (fun () -> fun a b -> a + b));
  compare "marshalling" (many (fun () ->
    Marshal.from_string (Marshal.to_string (ref (ref (ref 42))) []) 0))
