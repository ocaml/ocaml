(* TEST
*)

(* UTF codec tests *)

let fold_uchars f acc =
  let rec loop f acc u =
    let acc = f acc u in
    if Uchar.equal u Uchar.max then acc else loop f acc (Uchar.succ u)
  in
  loop f acc Uchar.min

(* This tests that we encode and decode each character according
   to its specification. *)

let utf_8_spec =
  (* UTF-8 byte sequences, cf. table 3.7 Unicode 14. *)
  [(0x0000,0x007F),     [|(0x00,0x7F)|];
   (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
   (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
   (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
   (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
   (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
   (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]

let utf_16be_spec =
  (* UTF-16BE byte sequences, derived from table 3.5 Unicode 14. *)
  [(0x0000,0xD7FF),    [|(0x00,0xD7); (0x00,0xFF)|];
   (0xE000,0xFFFF),    [|(0xE0,0xFF); (0x00,0xFF)|];
   (0x10000,0x10FFFF), [|(0xD8,0xDB); (0x00,0xFF); (0xDC,0xDF); (0x00,0xFF)|]]

let uchar_map_of_spec spec =
  (* array mapping Uchar.t as ints to byte sequences according to [spec]. *)
  let map = Array.make ((Uchar.to_int Uchar.max) + 1) Bytes.empty in
  let add_range ((umin, umax), bytes) =
    let len = Array.length bytes in
    let bmin i = if i < len then fst bytes.(i) else max_int in
    let bmax i = if i < len then snd bytes.(i) else min_int in
    let uchar = ref umin in
    let buf = Bytes.create len in
    let add len' = match len = len' with
    | false -> ()
    | true -> map.(!uchar) <- Bytes.copy buf; incr uchar
    in
    for b0 = bmin 0 to bmax 0 do Bytes.set_uint8 buf 0 b0;
      for b1 = bmin 1 to bmax 1 do Bytes.set_uint8 buf 1 b1;
        for b2 = bmin 2 to bmax 2 do Bytes.set_uint8 buf 2 b2;
          for b3 = bmin 3 to bmax 3 do Bytes.set_uint8 buf 3 b3; add 4
          done; add 3;
        done; add 2;
      done; add 1;
    done; assert (!uchar - 1 = umax)
  in
  List.iter add_range spec;
  map

let uchar_map_get u map = map.(Uchar.to_int u)
let utf_8 = uchar_map_of_spec utf_8_spec
let utf_16be = uchar_map_of_spec utf_16be_spec
let utf_16le =
  let swap u b =
    let len = Bytes.length b in
    if len = 0 then () else
    for i = 0 to Bytes.length b / 2 - 1 do
      let j = i * 2 in
      Bytes.set_uint16_le b j (Bytes.get_uint16_be b j);
    done;
  in
  let map = Array.map Bytes.copy utf_16be in
  Array.iteri swap map; map

let test_utf utf utf_len get_utf set_utf utf_is_valid =
  (* Test codec and validation of each Uchar.t against the spec. *)
  let f () u =
    let utf_len = utf_len u in
    let buf = Bytes.create utf_len in
    assert (set_utf buf 0 u = utf_len);
    assert (Bytes.equal buf (uchar_map_get u utf));
    assert (Bytes.equal buf (uchar_map_get u utf));
    let dec = get_utf buf 0 in
    assert (Uchar.utf_decode_is_valid dec);
    assert (Uchar.utf_decode_length dec = utf_len);
    assert (Uchar.equal (Uchar.utf_decode_uchar dec) u);
    assert (utf_is_valid buf);
    ()
  in
  fold_uchars f ()

let () =
  test_utf utf_8 Uchar.utf_8_byte_length
    Bytes.get_utf_8_uchar Bytes.set_utf_8_uchar Bytes.is_valid_utf_8

let () =
  test_utf utf_16be Uchar.utf_16_byte_length
    Bytes.get_utf_16be_uchar Bytes.set_utf_16be_uchar Bytes.is_valid_utf_16be

let () =
  test_utf utf_16le Uchar.utf_16_byte_length
    Bytes.get_utf_16le_uchar Bytes.set_utf_16le_uchar Bytes.is_valid_utf_16le

let () =
  (* Test out of bounds *)
  let raises f = assert (try f (); false with Invalid_argument _ -> true) in
  (raises @@ fun () -> Bytes.get_utf_8_uchar Bytes.empty 0);
  (raises @@ fun () -> Bytes.set_utf_8_uchar Bytes.empty 0 Uchar.min);
  (raises @@ fun () -> Bytes.get_utf_16le_uchar Bytes.empty 0);
  (raises @@ fun () -> Bytes.set_utf_16le_uchar Bytes.empty 0 Uchar.min);
  (raises @@ fun () -> Bytes.get_utf_16be_uchar Bytes.empty 0);
  (raises @@ fun () -> Bytes.set_utf_16be_uchar Bytes.empty 0 Uchar.min);
  ()

let () =
  (* Test lack of space encodes *)
  let b = Bytes.make 1 '\xab' in
  assert (Bytes.set_utf_8_uchar b 0 Uchar.max = 0 && Bytes.get b 0 = '\xab');
  assert (Bytes.set_utf_16be_uchar b 0 Uchar.max = 0 && Bytes.get b 0 = '\xab');
  assert (Bytes.set_utf_16le_uchar b 0 Uchar.max = 0 && Bytes.get b 0 = '\xab');
  ()

let () =
  (* Test bug found during review *)
  let b = Bytes.create 2 in
  let () = Bytes.set_uint8 b 0 0xC3 in
  let () = Bytes.set_uint8 b 1 0x00 in
  assert (not (Bytes.is_valid_utf_8 b))

let () =
  (* Test used bytes and replacement according to WHATWG recommendation.
     This is just a recommendation.
     These examples are from TUS p. 126-127 Unicode 14  *)
  let b = Bytes.of_string "\xC0\xAF\xE0\x80\xBF\xF0\x81\x82\x41" in
  let ok i = i = Bytes.length b - 1 in
  for i = 0 to Bytes.length b - 1 do
    let dec = Bytes.get_utf_8_uchar b i in
    if not (ok i) then begin
      assert (Uchar.utf_decode_is_valid dec = false);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec) Uchar.rep)
    end else begin
      assert (Uchar.utf_decode_is_valid dec = true);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec) (Uchar.of_int 0x0041))
    end
  done;
  let b = Bytes.of_string "\xED\xA0\x80\xED\xBF\xBF\xED\xAF\x41" in
  let ok i = i = Bytes.length b - 1 in
  for i = 0 to Bytes.length b - 1 do
    let dec = Bytes.get_utf_8_uchar b i in
    if not (ok i) then begin
      assert (Uchar.utf_decode_is_valid dec = false);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec) Uchar.rep)
    end else begin
      assert (Uchar.utf_decode_is_valid dec = true);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec) (Uchar.of_int 0x0041))
    end
  done;
  let b = Bytes.of_string "\xF4\x91\x92\x93\xFF\x41\x80\xBF\x42" in
  let ok i = i = 5 || i = 8 in
  for i = 0 to Bytes.length b - 1 do
    let dec = Bytes.get_utf_8_uchar b i in
    if not (ok i) then begin
      assert (Uchar.utf_decode_is_valid dec = false);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec) Uchar.rep)
    end else begin
      assert (Uchar.utf_decode_is_valid dec = true);
      assert (Uchar.utf_decode_length dec = 1);
      assert (Uchar.equal (Uchar.utf_decode_uchar dec)
                (Uchar.of_char (Bytes.get b i)))
    end
  done;
  let b = Bytes.of_string "\xE1\x80\xE2\xF0\x91\x92\xF1\xBF\x41" in
  let d0 = Bytes.get_utf_8_uchar b 0 in
  assert (Uchar.utf_decode_is_valid d0 = false);
  assert (Uchar.utf_decode_length d0 = 2);
  assert (Uchar.equal (Uchar.utf_decode_uchar d0) Uchar.rep);
  let d2 = Bytes.get_utf_8_uchar b 2 in
  assert (Uchar.utf_decode_is_valid d2 = false);
  assert (Uchar.utf_decode_length d2 = 1);
  assert (Uchar.equal (Uchar.utf_decode_uchar d2) Uchar.rep);
  let d3 = Bytes.get_utf_8_uchar b 3 in
  assert (Uchar.utf_decode_is_valid d3 = false);
  assert (Uchar.utf_decode_length d3 = 3);
  assert (Uchar.equal (Uchar.utf_decode_uchar d3) Uchar.rep);
  let d6 = Bytes.get_utf_8_uchar b 6 in
  assert (Uchar.utf_decode_is_valid d6 = false);
  assert (Uchar.utf_decode_length d6 = 2);
  assert (Uchar.equal (Uchar.utf_decode_uchar d6) Uchar.rep);
  let d8 = Bytes.get_utf_8_uchar b 8 in
  assert (Uchar.utf_decode_length d8 = 1);
  assert (Uchar.equal (Uchar.utf_decode_uchar d8) (Uchar.of_int 0x0041));
  ()

let () = Printf.printf "All UTF tests passed!\n"

(* This is a very long test added here for reference just in case. It
   is not run.

   It assumes the good encoding and decodes have been checked by test_utf
   above. It exhaustively tests all 1-4 bytes invalid sequences for decodes.
   This ensures we do not decode invalid sequence to uchars. *)

let test_invalid_decodes () =
  let module Sset = Set.Make (String) in
  let utf_8_encs, utf_16be_encs, utf_16le_encs =
    Printf.printf "Building encoding sequence sets\n%!";
    let add (set8, set16be, set16le) u =
      let s = Bytes.unsafe_to_string in
      let e8 = Bytes.create (Uchar.utf_8_byte_length u) in
      let e16be = Bytes.create (Uchar.utf_16_byte_length u) in
      let e16le = Bytes.create (Uchar.utf_16_byte_length u) in
      ignore (Bytes.set_utf_8_uchar e8 0 u);
      ignore (Bytes.set_utf_16be_uchar e16be 0 u);
      ignore (Bytes.set_utf_16le_uchar e16le 0 u);
      Sset.add (s e8) set8,
      Sset.add (s e16be) set16be,
      Sset.add (s e16le) set16le
    in
    fold_uchars add (Sset.empty, Sset.empty, Sset.empty)
  in
  let test_seqs utf utf_encs get_utf_char is_valid_utf =
    let test seq =
      let dec = get_utf_char seq 0 in
      let valid = Uchar.utf_decode_is_valid dec in
      let is_valid = is_valid_utf seq in
      let is_enc = Sset.mem (Bytes.unsafe_to_string seq) utf_encs in
      if not ((valid && is_enc) || (not valid && not is_enc)) ||
         not ((is_valid && is_enc) || (not is_valid && not is_enc))
      then begin
        for i = 0 to Bytes.length seq - 1 do
          Printf.printf "%02X " (Bytes.get_uint8 seq i);
        done;
        Printf.printf "valid: %b is_encoding: %b decode: U+%04X\n is_valid:%b"
          valid is_enc (Uchar.to_int (Uchar.utf_decode_uchar dec)) is_valid;
        assert false
      end;
      valid
    in
    let[@inline] set buf i b = Bytes.unsafe_set buf i (Char.unsafe_chr b) in
    let s1 = Bytes.create 1 and s2 = Bytes.create 2
    and s3 = Bytes.create 3 and s4 = Bytes.create 4 in
    Printf.printf "Testing %s invalid decodes...\n%!" utf;
    for b0 = 0x00 to 0xFF do
      set s1 0 b0;
      if test s1 then ((* this prefix decoded, stop here *)) else begin
        set s2 0 b0;
        for b1 = 0x00 to 0xFF do
          set s2 1 b1;
          if test s2 then ((* this prefix decoded, stop here *)) else begin
            set s3 0 b0;
            set s3 1 b1;
            for b2 = 0x00 to 0xFF do
              set s3 2 b2;
              if test s3 then ((* this prefix decoded, stop here *)) else begin
                set s4 0 b0;
                set s4 1 b1;
                set s4 2 b2;
                for b3 = 0x00 to 0xFF do set s4 3 b3; ignore (test s4) done;
              end
            done;
          end
        done;
      end
    done
  in
  test_seqs "UTF-8" utf_8_encs Bytes.get_utf_8_uchar Bytes.is_valid_utf_8;
  test_seqs "UTF-16BE"
    utf_16be_encs Bytes.get_utf_16be_uchar Bytes.is_valid_utf_16be;
  test_seqs "UTF-16LE" utf_16le_encs Bytes.get_utf_16le_uchar
    Bytes.is_valid_utf_16le;
  ()
