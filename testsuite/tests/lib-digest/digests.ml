(* TEST
*)

module Test(H: Digest.S) = struct

  let string (msg, hh) =
    assert (H.(equal (string msg) (of_hex hh)))

  let file wlen rlen =
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

  let run_tests tests =
    List.iter string tests;
    file 100 99;
    file 100_000 10_000
end

(* Test inputs *)

let in1 = ""
let in2 = "a"
let in3 = "abc"
let in4 = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno\
           ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
let in5 = String.make 100_000 'a'

(* Test vectors *)

module TestMD5 = Test(Digest.MD5)
let _ = TestMD5.run_tests
  [in1, "d41d8cd98f00b204e9800998ecf8427e";
   in2, "0cc175b9c0f1b6a831c399e269772661";
   in3, "900150983cd24fb0d6963f7d28e17f72";
   in4, "03dd8807a93175fb062dfb55dc7d359c";
   in5, "1af6d6f2f682f76f80e606aeaaee1680"]

module TestBLAKE512 = Test(Digest.BLAKE512)
let _ = TestBLAKE512.run_tests
  [in1, "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419\
     d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce";
   in2, "333fcb4ee1aa7c115355ec66ceac917c8bfd815bf7587d325aec1864edd24e34\
     d5abe2c6b1b5ee3face62fed78dbef802f2a85cb91d455a8f5249d330853cb3c";
   in3, "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1\
     7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923";
   in4, "ce741ac5930fe346811175c5227bb7bfcd47f42612fae46c0809514f9e0e3a11\
     ee1773287147cdeaeedff50709aa716341fe65240f4ad6777d6bfaf9726e5e52";
   in5, "fe89a110a412012e7cc5c0e05b03b48a6b9d0ba108187826c5ac82ce7aa45e7e\
     31b054979ec8ca5acd0bcc85f379d848f90f9d1593358cba8d88c7cd94ea8eee"]

module TestBLAKE256 = Test(Digest.BLAKE256)
let _ = TestBLAKE256.run_tests
  [in1, "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
   in2, "8928aae63c84d87ea098564d1e03ad813f107add474e56aedd286349c0c03ea4";
   in3, "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319";
   in4, "90a0bcf5e5a67ac1578c2754617994cfc248109275a809a0721feebd1e918738";
   in5, "b717c86cf745507ec5373f12f21350eb8550039b4263f7ba6e8df9030b5673c6"]

module TestBLAKE128 = Test(Digest.BLAKE128)
let _ = TestBLAKE128.run_tests
  [in1, "cae66941d9efbd404e4d88758ea67670";
   in2, "27c35e6e9373877f29e562464e46497e";
   in3, "cf4ab791c62b8d2b2109c90275287816";
   in4, "8fa81cd08c10a6e4dd94583e6fb48c2f";
   in5, "5c4b4b762807b3290e7eee0aa9b18655"]
