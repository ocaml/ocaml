(* TEST
 modules = "bigarray_stubs.c";
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)
module MP = Gc.Memprof

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external static_bigstring : unit -> bigstring = "static_bigstring"
external new_bigstring : unit -> bigstring = "new_bigstring"
external malloc_bigstring : unit -> bigstring = "malloc_bigstring"

let bigstring_create sz : bigstring =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout sz

let keep = ref []

let test () =
  let custom_words = ref 0 in
  let mmapped_words = ref 0 in
  let tmp_filename = Filename.temp_file "custom_test" ".dat" in
  let update words_ref size =
    words_ref := !words_ref + size * Sys.word_size/8
  in
  let alloc (info : MP.allocation) =
    match info.source with
    | Custom ->
      update custom_words info.size;
      Some (info.source, info.size)
    | Map_file ->
      update mmapped_words info.size;
      Some (info.source, info.size)
    | Normal | Marshal ->
      None
  in
  let dealloc (source, size) =
    match (source : MP.allocation_source) with
    | Custom -> update custom_words (-size)
    | Map_file -> update mmapped_words (-size)
    | Normal | Marshal -> ()
  in
  let tracker : _ MP.tracker =
    { alloc_minor = alloc;
      alloc_major = alloc;
      promote = (fun x -> Some x);
      dealloc_minor = dealloc;
      dealloc_major = dealloc }
  in
  let _:MP.t = MP.start ~sampling_rate:1. tracker in
  let log s =
    Printf.printf "%20s: %d custom bytes, %d mmapped bytes\n%!"
      s
      !custom_words
      !mmapped_words
  in
  let[@inline never] test_tail () =
    (* This is a separate tail-called function, to ensure
       that [str] is out of scope even on bytecode builds *)
    keep := [];
    Gc.full_major ();
    log "gc"
  in
  let test msg str =
    Sys.poll_actions ();
    log msg;
    keep := [str];
    (* sub and slice should not count as allocations *)
    keep := Bigarray.Array1.sub str 1000 1000 :: !keep;
    log "sub";
    Gc.full_major ();
    keep := Bigarray.Array1.sub str 1000 1000 :: !keep;
    log "slice";
    (test_tail[@tailcall]) ()
  in
  test "Allocation" (bigstring_create 5000);

  let map_len = 64 * 1024 in
  Unix.truncate tmp_filename map_len;
  let fd = Unix.openfile tmp_filename [O_RDONLY] 0o600 in
  test "Unix.map_file"
    (Unix.map_file fd Bigarray.char Bigarray.c_layout false [| map_len |]
     |> Bigarray.array1_of_genarray);
  Unix.close fd;

  (* Externally managed memory, should not be tracked *)
  test "CAML_BA_EXTERNAL" (static_bigstring ());

  (* Runtime-allocated memory, should be tracked *)
  test "ba_alloc NULL" (new_bigstring ());

  (* User-allocated yet GC-managed memory, should be tracked *)
  test "CAML_BA_MANAGED" (malloc_bigstring ());

  MP.stop ();
  Sys.remove tmp_filename;
  assert (!custom_words = 0)


let () = test ()
