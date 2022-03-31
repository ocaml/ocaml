(* TEST
*)

module H = Digest.BLAKE2_512

let test msg hh =
  assert (H.(equal (string msg) (from_hex hh)))

let _ =
  test ""
       "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419\
        d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce";
  test "abc"
       "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1\
        7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923";
  test "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno\
        ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
       "ce741ac5930fe346811175c5227bb7bfcd47f42612fae46c0809514f9e0e3a11\
        ee1773287147cdeaeedff50709aa716341fe65240f4ad6777d6bfaf9726e5e52";
  ()

let testfile wlen rlen =
  let data = String.init wlen Char.unsafe_chr in
  Out_channel.with_open_bin "data.tmp"
    (fun oc -> Out_channel.output_string oc data);
  let h1 = H.file "data.tmp" in
  assert (H.equal h1 (H.string data));
  let h2 =
    In_channel.with_open_bin "data.tmp"
      (fun ic -> H.channel ic rlen) in
  assert (H.equal h2 (H.substring data 0 rlen));
  Sys.remove "data.tmp"

let _ =
  testfile 100 99;
  testfile 5000 100;
  testfile 100_000 10_000
