(* TEST
   * bytecode
   * native
   * native
     ocamlopt_flags = "-compact"
*)

type mut2 = { mutable p: int; mutable q:int }
type mut3 = { mutable s: int; mutable t:int; mutable u:int }

type mut_record =
  { mutable a : int;
    mutable b : int;
    mutable c : int;
    mutable d : int;
    mutable e : int;
    mutable f : int; }

let go () =
  let pre_before = Gc.minor_words () in
  let before = Gc.minor_words () in
  let alloc_per_minor_words = int_of_float (before -. pre_before) in
  if Sys.backend_type = Sys.Native then assert (alloc_per_minor_words = 0);
  let allocs = ref alloc_per_minor_words in
  let n = 1_000_000 in
  for i = 1 to n do
    Sys.opaque_identity (ref i)
    |> ignore;
    allocs := !allocs + 2;
  done;
  for i = 1 to n do
    Sys.opaque_identity { p = i; q = i }
    |> ignore;
    allocs := !allocs + 3;
  done;
  for i = 1 to n do
    Sys.opaque_identity { s = i; t = i; u = i }
    |> ignore;
    allocs := !allocs + 4;
  done;
  for i = 1 to n do
    Sys.opaque_identity { a = i; b = i; c = i; d = i; e = i; f = i }
    |> ignore;
    allocs := !allocs + 7;
    if i mod (n/3) == 0 then Gc.full_major ();
  done;
  for i = 1 to n do
    Sys.opaque_identity (Array.make 8 i)
    |> ignore;
    allocs := !allocs + 9;
    if i mod (n/3) == 0 then Gc.compact ();
  done;
  let after = Gc.minor_words () in
  let measured_allocs = int_of_float (after -. before) - alloc_per_minor_words in
  Printf.printf "%d\n" (measured_allocs - !allocs)

let () = go ()
