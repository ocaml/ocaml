(* TEST
 include runtime_events;
 include unix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)

  let runtime_begin _ _ _ = ()
  let runtime_end _ _ _ = ()
  let runtime_counter _ _ _ _ = ()
  let alloc _ _ _ = ()
  let lifecycle _ _ _ _ = ()
  let lost_events _ _ = ()
  let callbacks = Runtime_events.Callbacks.create
    ~runtime_begin ~runtime_end ~runtime_counter ~alloc ~lifecycle ~lost_events ()

  let parse path_pid =
    let cursor =
        Runtime_events.create_cursor path_pid in
    let finally () = Runtime_events.free_cursor cursor in
    Fun.protect ~finally @@ fun () ->
    Runtime_events.read_poll cursor callbacks None

  let parse_corrupted path_pid =
    try let (_:int) = parse path_pid in ()
    with Failure _ | Invalid_argument _ ->
      (* parsing corrupted rings, raises exceptions,
         this is expected *)
      ()

  let buf = Bytes.create (8 * 8)

  let with_ring ring_file f =
    let fd = Unix.openfile ring_file [Unix.O_RDWR] 0 in
    let finally () = Unix.close fd in
    Fun.protect ~finally @@ fun () ->
    let size = Int64.to_int Unix.LargeFile.((fstat fd).st_size) in
    let n = Unix.read fd buf 0 (Bytes.length buf) in
    assert (n = Bytes.length buf);
    let version = Bytes.get_int64_ne buf 0 in
    assert (version = 1L);
    (* this needs to be updated if on-disk layout changes *)
    let data_offset = Bytes.get_int64_ne buf (6*8) in

    let write_event_header is_runtime event_type event_id event_length =
      let (<<:) i n = Int64.(shift_left (of_int i) n) and (|:) = Int64.logor in
      (* see runtime_events.h *)
      let event_header =
        (event_length <<: 54) |:
        (is_runtime <<: 53) |:
        (event_type <<: 49) |:
        (event_id <<: 36)
      in
      Bytes.set_int64_ne buf 0 event_header;
      let n = Unix.LargeFile.lseek fd data_offset Unix.SEEK_SET in
      assert (n = data_offset);
      let n = Unix.write fd buf 0 (Bytes.length buf) in
      assert (n = Bytes.length buf)
    in

    let write_metadata_header offset value =
      let offset = Int64.of_int offset in
      let n = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
      assert (n = offset);
      Bytes.set_int64_ne buf 0 value;
      let n = Unix.write fd buf 0 (Bytes.length buf) in
      assert (n = Bytes.length buf)
    in

    f ~size ~write_event_header ~write_metadata_header

  (* this tests the preservation of ring buffers after termination *)

  let () =
    (* start runtime_events now to avoid a race *)
    let parent_cwd = Sys.getcwd () in
    let child_pid = Unix.fork () in
    if child_pid == 0 then begin
      (* we are in the child, so start Runtime_events *)
      Runtime_events.start ();
      (* this creates a ring buffer. Now exit. *)
    end else begin
      (* now wait for our child to finish *)
      Unix.wait () |> ignore;
      (* child has finished. We now have a valid ring *)
      let ring_file =
          Filename.concat parent_cwd (string_of_int child_pid ^ ".events")
      and path_pid = Some (parent_cwd, child_pid);
         in
      let finally () = Unix.unlink ring_file in
      Fun.protect ~finally @@ fun () ->
      with_ring ring_file @@ fun ~size ~write_event_header ~write_metadata_header ->
      (* we must succeed parsing it as is *)
      let n = parse path_pid in
      assert (n > 0);
      let original = Bytes.to_string buf in

      (* now overwrite various fields, corrupting the ring,
         and check that we don't crash (raising exceptions is fine).
       *)
      for offset = 8 downto 0 do
        [0L; size * 3/4 |> Int64.of_int; size * 2 |> Int64.of_int;
         Int64.max_int; Int64.min_int; Int64.(shift_right_logical max_int 1)
        ] |> List.iter @@ fun value ->
        write_metadata_header (8 * offset) value;
        parse_corrupted path_pid;
        (* restore original, we only corrupt and test one offset at a time,
           otherwise we may not find missing bounds checks if we exit early
           due to bounds error on an earlier offset
         *)
        Bytes.blit_string original 0 buf 0 (Bytes.length buf);
      done;
      (* restore metadata header, so we have a valid ring again *)
      write_metadata_header 0 1L (* version *);

      for is_runtime = 0 to 1 do
        for event_type = 0 to 15 (* event type is 4 bits *) do
          for event_id = 0 to 64 (* event_id is 13 bits, but not all used yet *) do
            for length = 0 to 3 (* short lengths trigger uninit read bugs *) do
                (* modify just 1 event in the otherwise valid ring *)
                write_event_header is_runtime event_type event_id length;
                (* parse ring *)
                parse_corrupted path_pid;
            done
          done
        done;
      done;
    end
