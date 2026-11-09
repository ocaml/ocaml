(* TEST
 include runtime_events;
 include unix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 hasunix;
 not target-windows;
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
    let _ : int = Unix.alarm 1 in
    let finally () = let _ : int = Unix.alarm 0 in ()
    in
    Fun.protect ~finally @@ fun () ->
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
    let original = Bytes.to_string buf in
    let version = Bytes.get_int64_ne buf 0 in
    assert (version = 1L);
    (* this needs to be updated if on-disk layout changes *)
    let ring_size_bytes = Bytes.get_int64_ne buf (3*8) in
    let header_offset = Bytes.get_int64_ne buf (5*8) in
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

    let read_header () =
      let n = Unix.LargeFile.lseek fd header_offset Unix.SEEK_SET in
      assert (n = header_offset);
      let n = Unix.read fd buf 0 16 in
      assert (n = 16);
      Bytes.get_int64_ne buf 0, Bytes.get_int64_ne buf 8
    in

    let write_header head tail =
      let n = Unix.LargeFile.lseek fd header_offset Unix.SEEK_SET in
      assert (n = header_offset);
      Bytes.set_int64_ne buf 0 head;
      Bytes.set_int64_ne buf 8 tail;
      let n = Unix.write fd buf 0 16 in
      assert (n = 16)
    in

    let write_metadata_header offset1 value1 offset2 value2 =
      let n = Unix.LargeFile.lseek fd 0L Unix.SEEK_SET in
      assert (n = 0L);
      Bytes.set_int64_ne buf offset1 value1;
      Bytes.set_int64_ne buf offset2 value2;
      let n = Unix.write fd buf 0 (Bytes.length buf) in
      assert (n = Bytes.length buf)
    in

    f ~empty:false ~size ~original ~read_header ~write_header ~write_event_header ~write_metadata_header;

    (* now overwrite all events with padding records, and repeat the test *)
    for i = 0 to 7 do
      Bytes.set_int64_ne buf (i*8) (Int64.shift_left 2L 54)
    done;
    let n = Unix.LargeFile.lseek fd data_offset Unix.SEEK_SET in
    assert (n = data_offset);
    for _ = 1 to Int64.to_int ring_size_bytes / (Bytes.length buf) do
      let n = Unix.write fd buf 0 (Bytes.length buf) in
      assert (n = Bytes.length buf)
    done;

    f ~empty:true ~size ~original ~read_header ~write_header ~write_event_header ~write_metadata_header


  (* this tests the preservation of ring buffers after termination *)

  let () =
    let _ : Sys.signal_behavior =
      let timeout _ =
        prerr_endline "TIMEOUT";
        exit 126
      in
      Sys.signal Sys.sigalrm Sys.(Signal_handle timeout)
    in
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
      with_ring ring_file @@ fun ~empty ~size ~original ~read_header ~write_header ~write_event_header ~write_metadata_header ->
      (* we must succeed parsing it as is *)
      let n = parse path_pid in
      if empty then
        assert (n = 0)
      else
        assert (n > 0);

      let corners =
        [0L; size * 3/4 |> Int64.of_int; size * 2 |> Int64.of_int;
         size - 8 |> Int64.of_int;
         Int64.max_int; Int64.min_int; Int64.(shift_right_logical max_int 1)
        ]
      in
      let orig_head, orig_tail = read_header () in
      (* now overwrite the ring head/tail positions *)
      let () = corners |> List.iter @@ fun value ->
        [orig_head, Int64.add orig_head value
        ;orig_head, Int64.add orig_tail value
        ;0L, value
        ;value, orig_tail
        ;Int64.add orig_head value, Int64.add orig_tail value
        ] |> List.iter @@ fun (head, tail) ->
           write_header head tail;
           parse_corrupted path_pid;
      in
      (* restore original positions *)
      write_header orig_head orig_tail;
      Bytes.blit_string original 0 buf 0 (Bytes.length buf);

      (* now overwrite various fields, corrupting the ring,
         and check that we don't crash (raising exceptions is fine).
       *)
      for offset1 = 7 downto 0 do
        for offset2 = offset1 downto 0 do
          corners |> List.iter @@ fun value1 ->
          corners |> List.iter @@ fun value2 ->
          write_metadata_header (8 * offset1) value1 (8 * offset2) value2;
          parse_corrupted path_pid;
          (* restore original, we only corrupt and test one and two offset at a time,
            otherwise we may not find missing bounds checks if we exit early
            due to bounds error on an earlier offset
          *)
          Bytes.blit_string original 0 buf 0 (Bytes.length buf);
        done
      done;
      Bytes.blit_string original 0 buf 0 (Bytes.length buf);
      (* restore metadata header, so we have a valid ring again *)
      write_metadata_header 0 1L 0 1L(* version *);

      let n' = parse path_pid in
      assert (n' = n);

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
