(* TEST *)

open Gc.Memprof

let bigstring_create sz =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout sz

let keep = ref []

let test sampling_rate =
  let size = 256 in
  let iters = 100_000 in
  let size_words = size / (Sys.word_size / 8) in
  let alloc = ref 0 and collect = ref 0 and promote = ref 0 in
  let tracker =
    { null_tracker with
      alloc_minor = (fun info ->
        if info.source <> Custom then None
        else begin
          alloc := !alloc + info.n_samples;
          Some info.n_samples
        end);
      promote = (fun ns ->
        promote := !promote + ns; None);
      dealloc_minor = (fun ns ->
        collect := !collect + ns) } in
  start ~sampling_rate tracker;
  for i = 1 to iters do
    let str = Sys.opaque_identity bigstring_create size in
    if i mod 10 = 0 then keep := str :: !keep
  done;
  keep := [];
  Gc.full_major ();
  stop ();
  assert (!alloc = !promote + !collect);
  let iters = float_of_int iters and size_words = float_of_int size_words in
  (* see comballoc.ml for notes on precision *)
  Printf.printf "%.2f %.1f\n"
    ((float_of_int !alloc /. iters) /. size_words)
    ((float_of_int !promote /. iters) /. size_words *. 10.)


let () =
  [0.01; 0.5; 0.17] |> List.iter test
