(* TEST
*)

(* Testing our SIP-based hash function against the reference SIP-1-3. *)

open Printf

external sip13 : int -> string -> int = "caml_hash_string_sip13"

(* Test vectors are strings ["\000\001\002..."] of length 0 to 63. *)

let test (len, seed, expected) =
  let s = String.init len (fun i -> Char.chr i) in
  let h = sip13 seed s in
  if Int64.(logand expected 0x3FFFFFFFL <> of_int h) then
    printf "Error: len %d, seed %d, expected %Lx, got %x\n"
           len seed expected h

(* 64-bit hashes obtained with the SIP-1-3 reference implementation
   from https://github.com/veorq/SipHash *)

let test_vectors = [
0, 0, 0xd1fba762150c532cL;
1, 0, 0x68a914128e01e473L;
2, 0, 0x10bac45c41e3669L;
3, 0, 0x4d4c9a4a8ef6e0adL;
4, 0, 0x7cc43f98813e4dbdL;
5, 0, 0x5abe2169dff36275L;
6, 0, 0xe3c25f87624f1cdbL;
7, 0, 0x2f098ab0c751325aL;
8, 0, 0xead411e67ebe2eeaL;
9, 0, 0x75927f9d95124362L;
10, 0, 0xaf9f77a65ab51a1dL;
11, 0, 0xfe64ce8b6617fcffL;
12, 0, 0xa6baf4fb0f9fe1c2L;
13, 0, 0xa0cf3211850f8e0dL;
14, 0, 0x7f86049379fbfe67L;
15, 0, 0xf30eb725bb91c9eaL;
16, 0, 0x8972188433a5c5b7L;
17, 0, 0x4883c49a2c009c1dL;
18, 0, 0xadc2c0b63044067dL;
19, 0, 0x91f4a3329f87d19L;
20, 0, 0x639e355ae68c0100L;
21, 0, 0xb17bef2cb5213239L;
22, 0, 0xf8aeb1066236ccfeL;
23, 0, 0x37332b1389daa4ffL;
24, 0, 0x31185a47af932f3aL;
25, 0, 0xebf352e82ae23e2fL;
26, 0, 0xed36ff242fe478b2L;
27, 0, 0x2454d97d299299dfL;
28, 0, 0x8cd57fe8130d0ac7L;
29, 0, 0xbe98eb2faf0a0e85L;
30, 0, 0xd427c34ee09470c6L;
31, 0, 0x169739443111d49bL;
32, 0, 0x31ef8061c910629bL;
33, 0, 0x7fb71d24dfa4c9f6L;
34, 0, 0x92771f49e836d389L;
35, 0, 0x3c393cf6f0853bc3L;
36, 0, 0xac77921fcc642bd3L;
37, 0, 0x422a9751192ff266L;
38, 0, 0xc680ae8a8c584ddfL;
39, 0, 0x6c76b6d34bb6d26bL;
40, 0, 0x95bc321ab41d8206L;
41, 0, 0x908a3a97a6d37669L;
42, 0, 0xfcaf3c64874dd0b4L;
43, 0, 0xf689479b32b84a44L;
44, 0, 0xd857641079efd1fdL;
45, 0, 0x8b581578722633f7L;
46, 0, 0x83cb09d35ea7bcd4L;
47, 0, 0x8485f7f5ae725a0dL;
48, 0, 0xfe1f2445ba50237fL;
49, 0, 0x23cece041d7866b0L;
50, 0, 0x416687480b336c09L;
51, 0, 0xf15960da7e7f3892L;
52, 0, 0xded6177b9826835aL;
53, 0, 0xf80dae51668be6beL;
54, 0, 0x17ecc6ee6647df18L;
55, 0, 0x5220d642b4a52110L;
56, 0, 0xa4f0cf4ba4adc016L;
57, 0, 0xe5125bb26d1f2976L;
58, 0, 0x851b0e75f580d256L;
59, 0, 0x12605250a37e39acL;
60, 0, 0xd95a713568daebf1L;
61, 0, 0xcdcf4a42ec0dd605L;
62, 0, 0xa5c708712b617164L;
63, 0, 0x385d3e39e5f37359L;
0, 1, 0xc44a0ebf4e962581L;
1, 1, 0xc7f8837cdc05230bL;
2, 1, 0xbfeff0bc8a414c08L;
3, 1, 0xd8c82d87fa4286e4L;
4, 1, 0x5d51da6e88a607e0L;
5, 1, 0x904197b89cffef85L;
6, 1, 0x5495a918e3423106L;
7, 1, 0x9ce896d47e204af6L;
8, 1, 0x51738227823e2309L;
9, 1, 0xa9d59ba0fefc9497L;
10, 1, 0xc56750690b3345efL;
11, 1, 0xdccd724817e07d34L;
12, 1, 0x292cdaf7e383c05aL;
13, 1, 0xf1274d8b4ae1246eL;
14, 1, 0x768cb93004a0efa8L;
15, 1, 0x535280d545ebc696L;
16, 1, 0xde2f7768ba893d84L;
17, 1, 0xa5f1ec80df8761a5L;
18, 1, 0x1427ef7862a19171L;
19, 1, 0x655a0dc22d8b90a6L;
20, 1, 0xd0c112e9e4038c40L;
21, 1, 0x24d633ab5ba1b7e9L;
22, 1, 0x99b06c142bcf8b5fL;
23, 1, 0x18feebfcbb0326b3L;
24, 1, 0xf5e408152c880545L;
25, 1, 0x6015f851bac89dc1L;
26, 1, 0xac33279dc38a017eL;
27, 1, 0x9bc7412bb2eb7394L;
28, 1, 0x5ffa8eddba0091e6L;
29, 1, 0xdffde98f2c745b79L;
30, 1, 0x78db18c79a12f099L;
31, 1, 0x446a5b127a38bf2eL;
32, 1, 0xded378bdce3e8ed5L;
33, 1, 0xfcca56c93ac0918cL;
34, 1, 0x5b39d7994e4f784cL;
35, 1, 0x1a0a1f9156633641L;
36, 1, 0x5d8abd3827b86fcbL;
37, 1, 0xf13609be742294abL;
38, 1, 0x577add0b75d2c331L;
39, 1, 0xbfde1b9768516babL;
40, 1, 0x5f4ed25d19cdcd56L;
41, 1, 0xf1b601b270258d7L;
42, 1, 0xb026423c6c066fbcL;
43, 1, 0xc1876b1f6b36efb7L;
44, 1, 0x570442373a1298bdL;
45, 1, 0x1d320e1f7e5bbe92L;
46, 1, 0x527dc0ab084853dfL;
47, 1, 0x6ef49aecf842b256L;
48, 1, 0x196e65e9df13f340L;
49, 1, 0x3163970e2c2a3cecL;
50, 1, 0xc5e84ae1e8e70d1cL;
51, 1, 0x12c3d0a742f81cb5L;
52, 1, 0xec779699cfb6dc88L;
53, 1, 0x1390ce0e757826d4L;
54, 1, 0xcffe7f1d95d46cdbL;
55, 1, 0x81739547885752ffL;
56, 1, 0x399b320e8f24805fL;
57, 1, 0xc01aee30b0649549L;
58, 1, 0xfd6325afbed1d417L;
59, 1, 0xbdc54acabc306afdL;
60, 1, 0x19a4b0f98334c46L;
61, 1, 0xba1a669aaa372afdL;
62, 1, 0x828e134835b63734L;
63, 1, 0xa35bf11b84b78be0L
]

let _ =
  List.iter test test_vectors
