(* TEST *)

(* modified glibc's fma() tests *)

let string_of_fpclass = function
| Float.FP_normal -> "normal"
| FP_subnormal -> "subnormal"
| FP_zero -> "zero"
| FP_infinite -> "infinite"
| FP_nan -> "nan"

let error l x y z r c =
  Printf.fprintf stdout
                 "%s FAIL!\tfma (%h, %h, %h) returned %h (%s) instead of %h.\n"
                 l x y z c (string_of_fpclass (Float.classify_float c))
                 (List.hd r)

let success l =
  Printf.fprintf stdout "%s OK!\n" l

let fma_test l x y z r =
  let c = Float.fma x y z in
  if List.exists (fun i -> i = c) r
  then success l
  else error l x y z r c

(* test case description:

  (string * float * float * float * float list)
   |        |       |       |       |
   id       |       |       |       IEEE compliant result in head,
            |       |       |       or, accepted fma emulation approximation
            |       |       |       results in tail (if any)
            |       |       |
            x       y       z   -> operands as in fma x y z
 *)
let _ =
  let cases = [
      ("001", 0x1p+0, 0x2p+0, 0x3p+0, [0x5p+0]);
      ("002", 0x1.4p+0, 0xcp-4, 0x1p-4, [0x1p+0]);
      ("003", 0x0p+0, 0x0p+0, 0x0p+0, [0x0p+0]);
      ("004", 0x0p+0, 0x0p+0, ~-.0x0p+0, [0x0p+0]);
      ("005", 0x0p+0, ~-.0x0p+0, 0x0p+0, [0x0p+0]);
      ("006", 0x0p+0, ~-.0x0p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("007", ~-.0x0p+0, 0x0p+0, 0x0p+0, [0x0p+0]);
      ("008", ~-.0x0p+0, 0x0p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("009", ~-.0x0p+0, ~-.0x0p+0, 0x0p+0, [0x0p+0]);
      ("010", ~-.0x0p+0, ~-.0x0p+0, ~-.0x0p+0, [0x0p+0]);
      ("011", 0x1p+0, 0x0p+0, 0x0p+0, [0x0p+0]);
      ("012", 0x1p+0, 0x0p+0, ~-.0x0p+0, [0x0p+0]);
      ("013", 0x1p+0, ~-.0x0p+0, 0x0p+0, [0x0p+0]);
      ("014", 0x1p+0, ~-.0x0p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("015", ~-.0x1p+0, 0x0p+0, 0x0p+0, [0x0p+0]);
      ("016", ~-.0x1p+0, 0x0p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("017", ~-.0x1p+0, ~-.0x0p+0, 0x0p+0, [0x0p+0]);
      ("018", ~-.0x1p+0, ~-.0x0p+0, ~-.0x0p+0, [0x0p+0]);
      ("019", 0x0p+0, 0x1p+0, 0x0p+0, [0x0p+0]);
      ("020", 0x0p+0, 0x1p+0, ~-.0x0p+0, [0x0p+0]);
      ("021", 0x0p+0, ~-.0x1p+0, 0x0p+0, [0x0p+0]);
      ("022", 0x0p+0, ~-.0x1p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("023", ~-.0x0p+0, 0x1p+0, 0x0p+0, [0x0p+0]);
      ("024", ~-.0x0p+0, 0x1p+0, ~-.0x0p+0, [~-.0x0p+0]);
      ("025", ~-.0x0p+0, ~-.0x1p+0, 0x0p+0, [0x0p+0]);
      ("026", ~-.0x0p+0, ~-.0x1p+0, ~-.0x0p+0, [0x0p+0]);
      ("027", 0x1p+0, 0x1p+0, ~-.0x1p+0, [0x0p+0]);
      ("028", 0x1p+0, ~-.0x1p+0, 0x1p+0, [0x0p+0]);
      ("029", ~-.0x1p+0, 0x1p+0, 0x1p+0, [0x0p+0]);
      ("030", ~-.0x1p+0, ~-.0x1p+0, ~-.0x1p+0, [0x0p+0]);
      ("031", 0x0p+0, 0x0p+0, 0x1p+0, [0x1p+0]);
      ("032", 0x0p+0, 0x0p+0, 0x2p+0, [0x2p+0]);
      ("033", 0x0p+0, 0x0p+0, 0xf.fffffp+124, [0xf.fffffp+124]);
      ("034", 0x0p+0, 0x0p+0, 0xf.ffffffffffff8p+1020,
       [0xf.ffffffffffff8p+1020]);
      ("035", 0x0p+0, 0x1p+0, 0x1p+0, [0x1p+0]);
      ("036", 0x1p+0, 0x0p+0, 0x1p+0, [0x1p+0]);
      ("037", 0x0p+0, 0x1p+0, 0x2p+0, [0x2p+0]);
      ("038", 0x1p+0, 0x0p+0, 0x2p+0, [0x2p+0]);
      ("039", 0x0p+0, 0x1p+0, 0xf.fffffp+124, [0xf.fffffp+124]);
      ("040", 0x0p+0, 0x1p+0, 0xf.ffffffffffff8p+1020,
       [0xf.ffffffffffff8p+1020]);
      ("041", 0x1p+0, 0x0p+0, 0xf.fffffp+124, [0xf.fffffp+124]);
      ("042", 0x1p+0, 0x0p+0, 0xf.ffffffffffff8p+1020,
       [0xf.ffffffffffff8p+1020]);
      ("043", 0x4p-128, 0x4p-128, 0x0p+0, [0x1p-252]);
      ("044", 0x4p-128, 0x4p-1024, 0x0p+0, [0x0p+0]);
      ("045", 0x4p-128, 0x8p-972, 0x0p+0, [0x0p+0]);
      ("046", 0x4p-1024, 0x4p-128, 0x0p+0, [0x0p+0]);
      ("047", 0x4p-1024, 0x4p-1024, 0x0p+0, [0x0p+0]);
      ("048", 0x4p-1024, 0x8p-972, 0x0p+0, [0x0p+0]);
      ("049", 0x8p-972, 0x4p-128, 0x0p+0, [0x0p+0]);
      ("050", 0x8p-972, 0x4p-1024, 0x0p+0, [0x0p+0]);
      ("051", 0x8p-972, 0x8p-972, 0x0p+0, [0x0p+0]);
      ("052", 0x4p-128, 0x4p-128, ~-.0x0p+0, [0x1p-252]);
      ("053", 0x4p-128, 0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("054", 0x4p-128, 0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("055", 0x4p-1024, 0x4p-128, ~-.0x0p+0, [0x0p+0]);
      ("056", 0x4p-1024, 0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("057", 0x4p-1024, 0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("058", 0x8p-972, 0x4p-128, ~-.0x0p+0, [0x0p+0]);
      ("059", 0x8p-972, 0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("060", 0x8p-972, 0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("061", 0x4p-128, ~-.0x4p-128, 0x0p+0, [~-.0x1p-252]);
      ("062", 0x4p-128, ~-.0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("063", 0x4p-128, ~-.0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("064", 0x4p-1024, ~-.0x4p-128, 0x0p+0, [~-.0x0p+0]);
      ("065", 0x4p-1024, ~-.0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("066", 0x4p-1024, ~-.0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("067", 0x8p-972, ~-.0x4p-128, 0x0p+0, [~-.0x0p+0]);
      ("068", 0x8p-972, ~-.0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("069", 0x8p-972, ~-.0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("070", 0x4p-128, ~-.0x4p-128, ~-.0x0p+0, [~-.0x1p-252]);
      ("071", 0x4p-128, ~-.0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("072", 0x4p-128, ~-.0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("073", 0x4p-1024, ~-.0x4p-128, ~-.0x0p+0, [~-.0x0p+0]);
      ("074", 0x4p-1024, ~-.0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("075", 0x4p-1024, ~-.0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("076", 0x8p-972, ~-.0x4p-128, ~-.0x0p+0, [~-.0x0p+0]);
      ("077", 0x8p-972, ~-.0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("078", 0x8p-972, ~-.0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("079", ~-.0x4p-128, 0x4p-128, 0x0p+0, [~-.0x1p-252]);
      ("080", ~-.0x4p-128, 0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("081", ~-.0x4p-128, 0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("082", ~-.0x4p-1024, 0x4p-128, 0x0p+0, [~-.0x0p+0]);
      ("083", ~-.0x4p-1024, 0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("084", ~-.0x4p-1024, 0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("085", ~-.0x8p-972, 0x4p-128, 0x0p+0, [~-.0x0p+0]);
      ("086", ~-.0x8p-972, 0x4p-1024, 0x0p+0, [~-.0x0p+0]);
      ("087", ~-.0x8p-972, 0x8p-972, 0x0p+0, [~-.0x0p+0]);
      ("088", ~-.0x4p-128, 0x4p-128, ~-.0x0p+0, [~-.0x1p-252]);
      ("089", ~-.0x4p-128, 0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("090", ~-.0x4p-128, 0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("091", ~-.0x4p-1024, 0x4p-128, ~-.0x0p+0, [~-.0x0p+0]);
      ("092", ~-.0x4p-1024, 0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("093", ~-.0x4p-1024, 0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("094", ~-.0x8p-972, 0x4p-128, ~-.0x0p+0, [~-.0x0p+0]);
      ("095", ~-.0x8p-972, 0x4p-1024, ~-.0x0p+0, [~-.0x0p+0]);
      ("096", ~-.0x8p-972, 0x8p-972, ~-.0x0p+0, [~-.0x0p+0]);
      ("097", ~-.0x4p-128, ~-.0x4p-128, 0x0p+0, [0x1p-252]);
      ("098", ~-.0x4p-128, ~-.0x4p-1024, 0x0p+0, [0x0p+0]);
      ("099", ~-.0x4p-128, ~-.0x8p-972, 0x0p+0, [0x0p+0]);
      ("100", ~-.0x4p-1024, ~-.0x4p-128, 0x0p+0, [0x0p+0]);
      ("101", ~-.0x4p-1024, ~-.0x4p-1024, 0x0p+0, [0x0p+0]);
      ("102", ~-.0x4p-1024, ~-.0x8p-972, 0x0p+0, [0x0p+0]);
      ("103", ~-.0x8p-972, ~-.0x4p-128, 0x0p+0, [0x0p+0]);
      ("104", ~-.0x8p-972, ~-.0x4p-1024, 0x0p+0, [0x0p+0]);
      ("105", ~-.0x8p-972, ~-.0x8p-972, 0x0p+0, [0x0p+0]);
      ("106", ~-.0x4p-128, ~-.0x4p-128, ~-.0x0p+0, [0x1p-252]);
      ("107", ~-.0x4p-128, ~-.0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("108", ~-.0x4p-128, ~-.0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("109", ~-.0x4p-1024, ~-.0x4p-128, ~-.0x0p+0, [0x0p+0]);
      ("110", ~-.0x4p-1024, ~-.0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("111", ~-.0x4p-1024, ~-.0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("112", ~-.0x8p-972, ~-.0x4p-128, ~-.0x0p+0, [0x0p+0]);
      ("113", ~-.0x8p-972, ~-.0x4p-1024, ~-.0x0p+0, [0x0p+0]);
      ("114", ~-.0x8p-972, ~-.0x8p-972, ~-.0x0p+0, [0x0p+0]);
      ("115", 0xf.fffffp+124, 0xf.fffffp+124, 0x4p-128, [0xf.ffffe000001p+252]);
      ("116", 0xf.fffffp+124, 0xf.fffffp+124, 0x4p-1024,
       [0xf.ffffe000001p+252]);
      ("117", 0xf.fffffp+124, 0xf.fffffp+124, 0x8p-972, [0xf.ffffe000001p+252]);
      ("118", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x4p-128, [infinity]);
      ("119", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x4p-1024, [infinity]);
      ("120", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x8p-972, [infinity]);
      ("121", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x4p-128, [infinity]);
      ("122", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x4p-1024, [infinity]);
      ("123", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x8p-972, [infinity]);
      ("124", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x4p-128,
       [infinity]);
      ("125", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x4p-1024,
       [infinity]);
      ("126", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x8p-972,
       [infinity]);
      ("127", 0xf.fffffp+124, 0xf.fffffp+124, ~-.0x4p-128,
       [0xf.ffffe000001p+252]);
      ("128", 0xf.fffffp+124, 0xf.fffffp+124, ~-.0x4p-1024,
       [0xf.ffffe000001p+252]);
      ("129", 0xf.fffffp+124, 0xf.fffffp+124, ~-.0x8p-972,
       [0xf.ffffe000001p+252]);
      ("130", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x4p-128, [infinity]);
      ("131", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [infinity]);
      ("132", 0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x8p-972, [infinity]);
      ("133", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x4p-128, [infinity]);
      ("134", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x4p-1024,
       [infinity]);
      ("135", 0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x8p-972, [infinity]);
      ("136", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [infinity]);
      ("137", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [infinity]);
      ("138", 0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [infinity]);
      ("139", 0xf.fffffp+124, ~-.0xf.fffffp+124, 0x4p-128,
       [~-.0xf.ffffe000001p+252]);
      ("140", 0xf.fffffp+124, ~-.0xf.fffffp+124, 0x4p-1024,
       [~-.0xf.ffffe000001p+252]);
      ("141", 0xf.fffffp+124, ~-.0xf.fffffp+124, 0x8p-972,
       [~-.0xf.ffffe000001p+252]);
      ("142", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x4p-128,
       [~-.infinity]);
      ("143", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x4p-1024,
       [~-.infinity]);
      ("144", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x8p-972,
       [~-.infinity]);
      ("145", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x4p-128,
       [~-.infinity]);
      ("146", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x4p-1024,
       [~-.infinity]);
      ("147", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x8p-972,
       [~-.infinity]);
      ("148", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x4p-128,
       [~-.infinity]);
      ("149", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x4p-1024,
       [~-.infinity]);
      ("150", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x8p-972,
       [~-.infinity]);
      ("151", 0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x4p-128,
       [~-.0xf.ffffe000001p+252]);
      ("152", 0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x4p-1024,
       [~-.0xf.ffffe000001p+252]);
      ("153", 0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x8p-972,
       [~-.0xf.ffffe000001p+252]);
      ("154", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [~-.infinity]);
      ("155", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [~-.infinity]);
      ("156", 0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [~-.infinity]);
      ("157", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x4p-128,
       [~-.infinity]);
      ("158", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x4p-1024,
       [~-.infinity]);
      ("159", 0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x8p-972,
       [~-.infinity]);
      ("160", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [~-.infinity]);
      ("161", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [~-.infinity]);
      ("162", 0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [~-.infinity]);
      ("163", ~-.0xf.fffffp+124, 0xf.fffffp+124, 0x4p-128,
       [~-.0xf.ffffe000001p+252]);
      ("164", ~-.0xf.fffffp+124, 0xf.fffffp+124, 0x4p-1024,
       [~-.0xf.ffffe000001p+252]);
      ("165", ~-.0xf.fffffp+124, 0xf.fffffp+124, 0x8p-972,
       [~-.0xf.ffffe000001p+252]);
      ("166", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x4p-128,
       [~-.infinity]);
      ("167", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x4p-1024,
       [~-.infinity]);
      ("168", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, 0x8p-972,
       [~-.infinity]);
      ("169", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x4p-128,
       [~-.infinity]);
      ("170", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x4p-1024,
       [~-.infinity]);
      ("171", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, 0x8p-972,
       [~-.infinity]);
      ("172", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x4p-128,
       [~-.infinity]);
      ("173", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x4p-1024,
       [~-.infinity]);
      ("174", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, 0x8p-972,
       [~-.infinity]);
      ("175", ~-.0xf.fffffp+124, 0xf.fffffp+124, ~-.0x4p-128,
       [~-.0xf.ffffe000001p+252]);
      ("176", ~-.0xf.fffffp+124, 0xf.fffffp+124, ~-.0x4p-1024,
       [~-.0xf.ffffe000001p+252]);
      ("177", ~-.0xf.fffffp+124, 0xf.fffffp+124, ~-.0x8p-972,
       [~-.0xf.ffffe000001p+252]);
      ("178", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [~-.infinity]);
      ("179", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [~-.infinity]);
      ("180", ~-.0xf.fffffp+124, 0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [~-.infinity]);
      ("181", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x4p-128,
       [~-.infinity]);
      ("182", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x4p-1024,
       [~-.infinity]);
      ("183", ~-.0xf.ffffffffffff8p+1020, 0xf.fffffp+124, ~-.0x8p-972,
       [~-.infinity]);
      ("184", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [~-.infinity]);
      ("185", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [~-.infinity]);
      ("186", ~-.0xf.ffffffffffff8p+1020, 0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [~-.infinity]);
      ("187", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, 0x4p-128,
       [0xf.ffffe000001p+252]);
      ("188", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, 0x4p-1024,
       [0xf.ffffe000001p+252]);
      ("189", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, 0x8p-972,
       [0xf.ffffe000001p+252]);
      ("190", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x4p-128,
       [infinity]);
      ("191", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x4p-1024,
       [infinity]);
      ("192", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, 0x8p-972,
       [infinity]);
      ("193", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x4p-128,
       [infinity]);
      ("194", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x4p-1024,
       [infinity]);
      ("195", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, 0x8p-972,
       [infinity]);
      ("196", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x4p-128,
       [infinity]);
      ("197", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x4p-1024,
       [infinity]);
      ("198", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020, 0x8p-972,
       [infinity]);
      ("199", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x4p-128,
       [0xf.ffffe000001p+252]);
      ("200", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x4p-1024,
       [0xf.ffffe000001p+252]);
      ("201", ~-.0xf.fffffp+124, ~-.0xf.fffffp+124, ~-.0x8p-972,
       [0xf.ffffe000001p+252]);
      ("202", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-128,
       [infinity]);
      ("203", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x4p-1024,
       [infinity]);
      ("204", ~-.0xf.fffffp+124, ~-.0xf.ffffffffffff8p+1020, ~-.0x8p-972,
       [infinity]);
      ("205", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x4p-128,
       [infinity]);
      ("206", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x4p-1024,
       [infinity]);
      ("207", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.fffffp+124, ~-.0x8p-972,
       [infinity]);
      ("208", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020,
       ~-.0x4p-128, [infinity]);
      ("209", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020,
       ~-.0x4p-1024, [infinity]);
      ("210", ~-.0xf.ffffffffffff8p+1020, ~-.0xf.ffffffffffff8p+1020,
       ~-.0x8p-972, [infinity]);
      ("211", 0x2.fffp+12, 0x1.000002p+0, 0x1.ffffp-24, [0x2.fff006p+12]);
      ("212", 0x1.fffp+0, 0x1.00001p+0, ~-.0x1.fffp+0, [0x1.fffp-20]);
      ("213", 0xc.d5e6fp+124, 0x2.6af378p-128, ~-.0x1.f08948p+0,
       [0xd.da108p-28]);
      ("214", 0x1.9abcdep+100, 0x2.6af378p-128, ~-.0x3.e1129p-28,
       [0x1.bb421p-52]);
      ("215", 0xf.fffffp+124, 0x1.001p+0, ~-.0xf.fffffp+124, [0xf.fffffp+112]);
      ("216", ~-.0xf.fffffp+124, 0x1.fffffep+0, 0xf.fffffp+124,
       [~-.0xf.ffffd000002p+124]);
      ("217", 0xf.fffffp+124, 0x2p+0, ~-.0xf.fffffp+124, [0xf.fffffp+124]);
      ("218", 0x5p-128, 0x8.00002p-4, 0x1p-128, [0x3.80000ap-128]);
      ("219", ~-.0x5p-128, 0x8.00002p-4, ~-.0x1p-128, [~-.0x3.80000ap-128]);
      ("220", 0x7.ffffep-128, 0x8.00001p-4, 0x8p-152, [0x3.ffffffffffep-128]);
      ("221", ~-.0x7.ffffep-128, 0x8.00001p-4, ~-.0x8p-152,
       [~-.0x3.ffffffffffep-128]);
      ("222", 0x8p-152, 0x8p-4, 0x3.fffff8p-128, [0x3.fffffcp-128]);
      ("223", ~-.0x8p-152, 0x8p-4, ~-.0x3.fffff8p-128, [~-.0x3.fffffcp-128]);
      ("224", 0x8p-152, 0x8.8p-4, 0x3.fffff8p-128, [0x3.fffffc4p-128]);
      ("225", ~-.0x8p-152, 0x8.8p-4, ~-.0x3.fffff8p-128, [~-.0x3.fffffc4p-128]);
      ("226", 0x8p-152, 0x8p-152, 0x8p+124, [0x8p+124]);
      ("227", 0x8p-152, ~-.0x8p-152, 0x8p+124, [0x8p+124]);
      ("228", 0x8p-152, 0x8p-152, ~-.0x8p+124, [~-.0x8p+124]);
      ("229", 0x8p-152, ~-.0x8p-152, ~-.0x8p+124, [~-.0x8p+124]);
      ("230", 0x8p-152, 0x8p-152, 0x4p-128, [0x4p-128]);
      ("231", 0x8p-152, ~-.0x8p-152, 0x4p-128, [0x4p-128]);
      ("232", 0x8p-152, 0x8p-152, ~-.0x4p-128, [~-.0x4p-128]);
      ("233", 0x8p-152, ~-.0x8p-152, ~-.0x4p-128, [~-.0x4p-128]);
      ("234", 0x8p-152, 0x8p-152, 0x3.fffff8p-128, [0x3.fffff8p-128]);
      ("235", 0x8p-152, ~-.0x8p-152, 0x3.fffff8p-128, [0x3.fffff8p-128]);
      ("236", 0x8p-152, 0x8p-152, ~-.0x3.fffff8p-128, [~-.0x3.fffff8p-128]);
      ("237", 0x8p-152, ~-.0x8p-152, ~-.0x3.fffff8p-128, [~-.0x3.fffff8p-128]);
      ("238", 0x8p-152, 0x8p-152, 0x8p-152, [0x8p-152]);
      ("239", 0x8p-152, ~-.0x8p-152, 0x8p-152, [0x8p-152]);
      ("240", 0x8p-152, 0x8p-152, ~-.0x8p-152, [~-.0x8p-152]);
      ("241", 0x8p-152, ~-.0x8p-152, ~-.0x8p-152, [~-.0x8p-152]);
      ("242", 0xf.ffp-4, 0xf.ffp-4, ~-.0xf.fep-4, [0x1p-24]);
      ("243", 0xf.ffp-4, ~-.0xf.ffp-4, 0xf.fep-4, [~-.0x1p-24]);
      ("244", ~-.0xf.ffp-4, 0xf.ffp-4, 0xf.fep-4, [~-.0x1p-24]);
      ("245", ~-.0xf.ffp-4, ~-.0xf.ffp-4, ~-.0xf.fep-4, [0x1p-24]);
      ("246", 0x4.000008p-128, 0x4.000008p-28, 0x8p+124, [0x8p+124]);
      ("247", 0x4.000008p-128, ~-.0x4.000008p-28, 0x8p+124, [0x8p+124]);
      ("248", 0x4.000008p-128, 0x4.000008p-28, ~-.0x8p+124, [~-.0x8p+124]);
      ("249", 0x4.000008p-128, ~-.0x4.000008p-28, ~-.0x8p+124, [~-.0x8p+124]);
      ("250", 0x4.000008p-128, 0x4.000008p-28, 0x8p+100, [0x8p+100]);
      ("251", 0x4.000008p-128, ~-.0x4.000008p-28, 0x8p+100, [0x8p+100]);
      ("252", 0x4.000008p-128, 0x4.000008p-28, ~-.0x8p+100, [~-.0x8p+100]);
      ("253", 0x4.000008p-128, ~-.0x4.000008p-28, ~-.0x8p+100, [~-.0x8p+100]);
      ("254", 0x2.fep+12, 0x1.0000000000001p+0, 0x1.ffep-48,
       [0x2.fe00000000002p+12; 0x1.7f00000000002p+13]);
      ("255", 0x1.fffp+0, 0x1.0000000000001p+0, ~-.0x1.fffp+0,
       [0x1.fffp-52; 0x1p-51]);
      ("256", 0x1.0000002p+0, 0xf.fffffep-4, 0x1p-300, [0x1p+0]);
      ("257", 0x1.0000002p+0, 0xf.fffffep-4, ~-.0x1p-300,
       [0xf.ffffffffffff8p-4; 0x1p+0]);
      ("258", 0xe.f56df7797f768p+1020, 0x3.7ab6fbbcbfbb4p-1024,
       ~-.0x3.40bf1803497f6p+0,
       [0x8.4c4b43de4ed2p-56; 0x1.095f287bc9da4p-53; 0x1.098p-53]);
      ("259", 0x1.deadbeef2feedp+900, 0x3.7ab6fbbcbfbb4p-1024,
       ~-.0x6.817e300692fecp-124,
       [0x1.0989687bc9da4p-176; 0x1.095f287bc9da4p-176; 0x1.098p-176]);
      ("260", 0xf.ffffffffffff8p+1020, 0x1.001p+0,
       ~-.0xf.ffffffffffff8p+1020, [0xf.ffffffffffff8p+1008; 0x1p+1012]);
      ("261", ~-.0xf.ffffffffffff8p+1020, 0x1.fffffffffffffp+0,
       0xf.ffffffffffff8p+1020, [~-.0xf.fffffffffffe8p+1020]);
      ("262", 0xf.ffffffffffff8p+1020, 0x2p+0, ~-.0xf.ffffffffffff8p+1020,
       [0xf.ffffffffffff8p+1020]);
      ("263", 0x5.a827999fcef3p-540, 0x5.a827999fcef3p-540, 0x0p+0, [0x0p+0]);
      ("264", 0x3.bd5b7dde5fddap-496, 0x3.bd5b7dde5fddap-496,
       ~-.0xd.fc352bc352bap-992,
       [0x1.0989687cp-1044; 0x0.000004277ca1fp-1022; 0x0.00000428p-1022]);
      ("265", 0x3.bd5b7dde5fddap-504, 0x3.bd5b7dde5fddap-504,
       ~-.0xd.fc352bc352bap-1008,
       [0x1.0988p-1060; 0x0.0000000004278p-1022; 0x0.000000000428p-1022]);
      ("266", 0x8p-540, 0x4p-540, 0x4p-1076, [0x8p-1076]);
      ("267", 0x1.7fffff8p-968, 0x4p-108, 0x4p-1048,
       [0x4.0000004p-1048; 0x0.0000010000002p-1022]);
      ("268", 0x2.8000008p-968, 0x4p-108, 0x4p-1048,
       [0x4.000000cp-1048; 0x0.0000010000002p-1022]);
      ("269", 0x2.8p-968, ~-.0x4p-108, ~-.0x4p-1048, [~-.0x4.0000008p-1048]);
      ("270", ~-.0x2.33956cdae7c2ep-960, 0x3.8e211518bfea2p-108,
       ~-.0x2.02c2b59766d9p-1024, [~-.0x2.02c2b59767564p-1024]);
      ("271", ~-.0x3.a5d5dadd1d3a6p-980, ~-.0x2.9c0cd8c5593bap-64,
       ~-.0x2.49179ac00d15p-1024, [~-.0x2.491702717ed74p-1024]);
      ("272", 0x2.2a7aca1773e0cp-908, 0x9.6809186a42038p-128,
       ~-.0x2.c9e356b3f0fp-1024,
       [~-.0x2.c89d5c48eefa4p-1024; ~-.0x0.b22757123bbe8p-1022]);
      ("273", ~-.0x3.ffffffffffffep-712, 0x3.ffffffffffffep-276,
       0x3.fffffc0000ffep-984, [0x2.fffffc0000ffep-984; 0x1.7ffffe00008p-983]);
      ("274", 0x5p-1024, 0x8.000000000001p-4, 0x1p-1024,
       [0x3.8000000000004p-1024]);
      ("275", ~-.0x5p-1024, 0x8.000000000001p-4, ~-.0x1p-1024,
       [~-.0x3.8000000000004p-1024]);
      ("276", 0x7.ffffffffffffp-1024, 0x8.0000000000008p-4, 0x4p-1076,
       [0x4p-1024]);
      ("277", ~-.0x7.ffffffffffffp-1024, 0x8.0000000000008p-4, ~-.0x4p-1076,
       [~-.0x4p-1024]);
      ("278", 0x4p-1076, 0x8p-4, 0x3.ffffffffffffcp-1024, [0x4p-1024]);
      ("279", ~-.0x4p-1076, 0x8p-4, ~-.0x3.ffffffffffffcp-1024, [~-.0x4p-1024]);
      ("280", 0x4p-1076, 0x8.8p-4, 0x3.ffffffffffffcp-1024, [0x4p-1024]);
      ("281", ~-.0x4p-1076, 0x8.8p-4, ~-.0x3.ffffffffffffcp-1024,
       [~-.0x4p-1024]);
      ("282", 0x4p-1076, 0x4p-1076, 0x8p+1020, [0x8p+1020]);
      ("283", 0x4p-1076, ~-.0x4p-1076, 0x8p+1020, [0x8p+1020]);
      ("284", 0x4p-1076, 0x4p-1076, ~-.0x8p+1020, [~-.0x8p+1020]);
      ("285", 0x4p-1076, ~-.0x4p-1076, ~-.0x8p+1020, [~-.0x8p+1020]);
      ("286", 0x4p-1076, 0x4p-1076, 0x4p-1024, [0x4p-1024]);
      ("287", 0x4p-1076, ~-.0x4p-1076, 0x4p-1024, [0x4p-1024]);
      ("288", 0x4p-1076, 0x4p-1076, ~-.0x4p-1024, [~-.0x4p-1024]);
      ("289", 0x4p-1076, ~-.0x4p-1076, ~-.0x4p-1024, [~-.0x4p-1024]);
      ("290", 0x4p-1076, 0x4p-1076, 0x3.ffffffffffffcp-1024,
       [0x3.ffffffffffffcp-1024]);
      ("291", 0x4p-1076, ~-.0x4p-1076, 0x3.ffffffffffffcp-1024,
       [0x3.ffffffffffffcp-1024]);
      ("292", 0x4p-1076, 0x4p-1076, ~-.0x3.ffffffffffffcp-1024,
       [~-.0x3.ffffffffffffcp-1024]);
      ("293", 0x4p-1076, ~-.0x4p-1076, ~-.0x3.ffffffffffffcp-1024,
       [~-.0x3.ffffffffffffcp-1024]);
      ("294", 0x4p-1076, 0x4p-1076, 0x4p-1076, [0x4p-1076]);
      ("295", 0x4p-1076, ~-.0x4p-1076, 0x4p-1076, [0x4p-1076]);
      ("296", 0x4p-1076, 0x4p-1076, ~-.0x4p-1076, [~-.0x4p-1076]);
      ("297", 0x4p-1076, ~-.0x4p-1076, ~-.0x4p-1076, [~-.0x4p-1076]);
      ("298", 0xf.ffffffffffff8p-4, 0xf.ffffffffffff8p-4,
       ~-.0xf.ffffffffffffp-4, [0x4p-108; 0x0p+0]);
      ("299", 0xf.ffffffffffff8p-4, ~-.0xf.ffffffffffff8p-4,
       0xf.ffffffffffffp-4, [~-.0x4p-108; 0x0p+0]);
      ("300", ~-.0xf.ffffffffffff8p-4, 0xf.ffffffffffff8p-4,
       0xf.ffffffffffffp-4, [~-.0x4p-108; 0x0p+0]);
      ("301", ~-.0xf.ffffffffffff8p-4, ~-.0xf.ffffffffffff8p-4,
       ~-.0xf.ffffffffffffp-4, [0x4p-108; 0x0p+0]);
      ("302", 0x4.0000000000004p-1024, 0x2.0000000000002p-56, 0x8p+1020,
       [0x8p+1020]);
      ("303", 0x4.0000000000004p-1024, ~-.0x2.0000000000002p-56, 0x8p+1020,
       [0x8p+1020]);
      ("304", 0x4.0000000000004p-1024, 0x2.0000000000002p-56, ~-.0x8p+1020,
       [~-.0x8p+1020]);
      ("305", 0x4.0000000000004p-1024, ~-.0x2.0000000000002p-56, ~-.0x8p+1020,
       [~-.0x8p+1020]);
      ("306", 0x4.0000000000004p-1024, 0x2.0000000000002p-56, 0x4p+968,
       [0x4p+968]);
      ("307", 0x4.0000000000004p-1024, ~-.0x2.0000000000002p-56, 0x4p+968,
       [0x4p+968]);
      ("308", 0x4.0000000000004p-1024, 0x2.0000000000002p-56, ~-.0x4p+968,
       [~-.0x4p+968]);
      ("309", 0x4.0000000000004p-1024, ~-.0x2.0000000000002p-56, ~-.0x4p+968,
       [~-.0x4p+968]);
      ("310", 0x7.fffff8p-128, 0x3.fffffcp+24, 0xf.fffffp+124,
       [0xf.fffffp+124]);
      ("311", 0x7.fffff8p-128, ~-.0x3.fffffcp+24, 0xf.fffffp+124,
       [0xf.fffffp+124]);
      ("312", 0x7.fffff8p-128, 0x3.fffffcp+24, ~-.0xf.fffffp+124,
       [~-.0xf.fffffp+124]);
      ("313", 0x7.fffff8p-128, ~-.0x3.fffffcp+24, ~-.0xf.fffffp+124,
       [~-.0xf.fffffp+124]);
      ("314", 0x7.ffffffffffffcp-1024, 0x7.ffffffffffffcp+52,
       0xf.ffffffffffff8p+1020, [0xf.ffffffffffff8p+1020]);
      ("315", 0x7.ffffffffffffcp-1024, ~-.0x7.ffffffffffffcp+52,
       0xf.ffffffffffff8p+1020, [0xf.ffffffffffff8p+1020]);
      ("316", 0x7.ffffffffffffcp-1024, 0x7.ffffffffffffcp+52,
       ~-.0xf.ffffffffffff8p+1020, [~-.0xf.ffffffffffff8p+1020]);
      ("317", 0x7.ffffffffffffcp-1024, ~-.0x7.ffffffffffffcp+52,
       ~-.0xf.ffffffffffff8p+1020, [~-.0xf.ffffffffffff8p+1020])
    ] in
  let rec do_cases c =
    match c with
      (l, x, y, z, r)::t -> fma_test l x y z r;
                            do_cases t
    | [] -> ()
  in
  do_cases cases
