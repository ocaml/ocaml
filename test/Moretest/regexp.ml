open Printf

let build_result ngroups input =
  let res = Array.make (ngroups + 1) "~" in
  for i = 0 to ngroups do
    try
      res.(i) <- Str.matched_group i input
    with Not_found -> ()
  done;
  res

let search_forward re ng input start =
  try
    ignore(Str.search_forward re input start);
    build_result ng input
  with Not_found ->
    [||]

let search_backward re ng input start =
  try
    ignore(Str.search_backward re input start);
    build_result ng input
  with Not_found ->
    [||]

let partial_match re ng input start =
  if Str.string_partial_match re input start
  then build_result ng input
  else [||]

let start_test msg =
  print_newline(); printf "%s\n  " msg

let num_failures = ref 0

let test res1 res2 =
  if res1 = res2 
  then print_char '.'
  else begin print_string " FAIL "; incr num_failures end

let test_search_forward r ng s exp =
  test (search_forward r ng s 0) exp

let test_search_backward r ng s exp =
  test (search_backward r ng s (String.length s)) exp

let test_partial_match r ng s exp =
  test (partial_match r ng s 0) exp

let end_test () =
  print_newline();
  if !num_failures = 0 then
    printf "All tests passed\n"
  else begin
    printf "TEST FAILED: %d failure(s)\n" !num_failures;
    exit 2
  end

let _ =

  (** Forward searches *)
  start_test "Search for /the quick brown fox/";
  let r = Str.regexp "the quick brown fox" in
  let n = 0 in
  test_search_forward r n "the quick brown fox"
    [|"the quick brown fox"|];
  test_search_forward r n "What do you know about the quick brown fox?"
    [|"the quick brown fox"|];
  test_search_forward r n "The quick brown FOX"
    [||];
  test_search_forward r n "What do you know about THE QUICK BROWN FOX?"
    [||];

  start_test "Search for /the quick brown fox/";
  let r = Str.regexp_case_fold "the quick brown fox" in
  let n = 0 in
  test_search_forward r n "the quick brown fox"
    [|"the quick brown fox"|];
  test_search_forward r n "What do you know about the quick brown fox?"
    [|"the quick brown fox"|];
  test_search_forward r n "The quick brown FOX"
    [|"The quick brown FOX"|];
  test_search_forward r n "What do you know about THE QUICK BROWN FOX?"
    [|"THE QUICK BROWN FOX"|];
  test_search_forward r n "The slow white snail"
    [||];

  start_test "Search for /a*abc?xyz+pqrrrabbb*xyyyyy?pq?q?q?q?q?q?AB*zz/";
  let r = Str.regexp "a*abc?xyz+pqrrrabbb*xyyyyy?pq?q?q?q?q?q?AB*zz" in
  let n = 0 in
  test_search_forward r n "abxyzpqrrrabbxyyyypqAzz"
    [|"abxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzpqrrrabbxyyyypqAzz"
    [|"abxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabxyzpqrrrabbxyyyypqAzz"
    [|"aabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabxyzpqrrrabbxyyyypqAzz"
    [|"aaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabxyzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abcxyzpqrrrabbxyyyypqAzz"
    [|"abcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabcxyzpqrrrabbxyyyypqAzz"
    [|"aabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypAzz"
    [|"aaabcxyzpqrrrabbxyyyypAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqqAzz"
    [|"aaabcxyzpqrrrabbxyyyypqqqqqqAzz"|];
  test_search_forward r n "aaaabcxyzpqrrrabbxyyyypqAzz"
    [|"aaaabcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzzpqrrrabbxyyyypqAzz"
    [|"abxyzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabxyzzzpqrrrabbxyyyypqAzz"
    [|"aabxyzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaabxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abcxyzzpqrrrabbxyyyypqAzz"
    [|"abcxyzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aabcxyzzzpqrrrabbxyyyypqAzz"
    [|"aabcxyzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaabcxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaabcxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbxyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbbxyyyypqAzz"|];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"
    [|"aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypABzz"
    [|"aaabcxyzpqrrrabbxyyyypABzz"|];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypABBzz"
    [|"aaabcxyzpqrrrabbxyyyypABBzz"|];
  test_search_forward r n ">>>aaabxyzpqrrrabbxyyyypqAzz"
    [|"aaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n ">aaaabxyzpqrrrabbxyyyypqAzz"
    [|"aaaabxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n ">>>>abcxyzpqrrrabbxyyyypqAzz"
    [|"abcxyzpqrrrabbxyyyypqAzz"|];
  test_search_forward r n "abxyzpqrrabbxyyyypqAzz"
    [||];
  test_search_forward r n "abxyzpqrrrrabbxyyyypqAzz"
    [||];
  test_search_forward r n "abxyzpqrrrabxyyyypqAzz"
    [||];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz"
    [||];
  test_search_forward r n "aaaabcxyzzzzpqrrrabbbxyyypqAzz"
    [||];
  test_search_forward r n "aaabcxyzpqrrrabbxyyyypqqqqqqqAzz"
    [||];

  start_test "Search for /^abc\\(abc\\)?zz/";
  let r = Str.regexp "^abc\\(abc\\)?zz" in
  let n = 1 in
  test_search_forward r n "abczz"
    [|"abczz"; "~"|];
  test_search_forward r n "abcabczz"
    [|"abcabczz"; "abc"|];
  test_search_forward r n "zz"
    [||];
  test_search_forward r n "abcabcabczz"
    [||];
  test_search_forward r n ">>abczz"
    [||];

  start_test "Search for /^\\(b+\\|a\\)\\(b+\\|a\\)?c/";
  let r = Str.regexp "^\\(b+\\|a\\)\\(b+\\|a\\)?c" in
  let n = 2 in
  test_search_forward r n "bc"
    [|"bc"; "b"; "~"|];
  test_search_forward r n "bbc"
    [|"bbc"; "bb"; "~"|];
  test_search_forward r n "bbbc"
    [|"bbbc"; "bbb"; "~"|];
  test_search_forward r n "bac"
    [|"bac"; "b"; "a"|];
  test_search_forward r n "bbac"
    [|"bbac"; "bb"; "a"|];
  test_search_forward r n "aac"
    [|"aac"; "a"; "a"|];
  test_search_forward r n "abbbbbbbbbbbc"
    [|"abbbbbbbbbbbc"; "a"; "bbbbbbbbbbb"|];
  test_search_forward r n "bbbbbbbbbbbac"
    [|"bbbbbbbbbbbac"; "bbbbbbbbbbb"; "a"|];
  test_search_forward r n "aaac"
    [||];
  test_search_forward r n "abbbbbbbbbbbac"
    [||];

  start_test "Search for /^\\(\\(b+\\|a\\)\\(b+\\|a\\)?\\)?bc/";
  let r = Str.regexp "^\\(\\(b+\\|a\\)\\(b+\\|a\\)?\\)?bc" in
  let n = 3 in
  test_search_forward r n "bbc"
    [|"bbc"; "b"; "b"; "~"|];

  start_test "Search for /^\\(\\(b*\\|ba\\)\\(b*\\|ba\\)?\\)?bc/";
  let r = Str.regexp "^\\(\\(b*\\|ba\\)\\(b*\\|ba\\)?\\)?bc" in
  let n = 3 in
  test_search_forward r n "babc"
    [|"babc"; "ba"; ""; "ba"|];
  test_search_forward r n "bbabc"
    [|"bbabc"; "bba"; "b"; "ba"|];
  test_search_forward r n "bababc"
    [|"bababc"; "baba"; "ba"; "ba"|];
  test_search_forward r n "bababbc"
    [||];
  test_search_forward r n "babababc"
    [||];

  start_test "Search for /^[]abcde]/";
  let r = Str.regexp "^[]abcde]" in
  let n = 0 in
  test_search_forward r n "athing"
    [|"a"|];
  test_search_forward r n "bthing"
    [|"b"|];
  test_search_forward r n "]thing"
    [|"]"|];
  test_search_forward r n "cthing"
    [|"c"|];
  test_search_forward r n "dthing"
    [|"d"|];
  test_search_forward r n "ething"
    [|"e"|];
  test_search_forward r n "fthing"
    [||];
  test_search_forward r n "[thing"
    [||];
  test_search_forward r n "\\\\thing"
    [||];

  start_test "Search for /^[]cde]/";
  let r = Str.regexp "^[]cde]" in
  let n = 0 in
  test_search_forward r n "]thing"
    [|"]"|];
  test_search_forward r n "cthing"
    [|"c"|];
  test_search_forward r n "dthing"
    [|"d"|];
  test_search_forward r n "ething"
    [|"e"|];
  test_search_forward r n "athing"
    [||];
  test_search_forward r n "fthing"
    [||];

  start_test "Search for /^[^]abcde]/";
  let r = Str.regexp "^[^]abcde]" in
  let n = 0 in
  test_search_forward r n "fthing"
    [|"f"|];
  test_search_forward r n "[thing"
    [|"["|];
  test_search_forward r n "\\\\thing"
    [|"\\"|];
  test_search_forward r n "athing"
    [||];
  test_search_forward r n "bthing"
    [||];
  test_search_forward r n "]thing"
    [||];
  test_search_forward r n "cthing"
    [||];
  test_search_forward r n "dthing"
    [||];
  test_search_forward r n "ething"
    [||];

  start_test "Search for /^[^]cde]/";
  let r = Str.regexp "^[^]cde]" in
  let n = 0 in
  test_search_forward r n "athing"
    [|"a"|];
  test_search_forward r n "fthing"
    [|"f"|];
  test_search_forward r n "]thing"
    [||];
  test_search_forward r n "cthing"
    [||];
  test_search_forward r n "dthing"
    [||];
  test_search_forward r n "ething"
    [||];

  start_test "Search for /^ÿ/";
  let r = Str.regexp "^ÿ" in
  let n = 0 in
  test_search_forward r n "ÿ"
    [|"ÿ"|];

  start_test "Search for /^[0-9]+$/";
  let r = Str.regexp "^[0-9]+$" in
  let n = 0 in
  test_search_forward r n "0"
    [|"0"|];
  test_search_forward r n "1"
    [|"1"|];
  test_search_forward r n "2"
    [|"2"|];
  test_search_forward r n "3"
    [|"3"|];
  test_search_forward r n "4"
    [|"4"|];
  test_search_forward r n "5"
    [|"5"|];
  test_search_forward r n "6"
    [|"6"|];
  test_search_forward r n "7"
    [|"7"|];
  test_search_forward r n "8"
    [|"8"|];
  test_search_forward r n "9"
    [|"9"|];
  test_search_forward r n "10"
    [|"10"|];
  test_search_forward r n "100"
    [|"100"|];
  test_search_forward r n "abc"
    [||];

  start_test "Search for /^.*nter/";
  let r = Str.regexp "^.*nter" in
  let n = 0 in
  test_search_forward r n "enter"
    [|"enter"|];
  test_search_forward r n "inter"
    [|"inter"|];
  test_search_forward r n "uponter"
    [|"uponter"|];

  start_test "Search for /^xxx[0-9]+$/";
  let r = Str.regexp "^xxx[0-9]+$" in
  let n = 0 in
  test_search_forward r n "xxx0"
    [|"xxx0"|];
  test_search_forward r n "xxx1234"
    [|"xxx1234"|];
  test_search_forward r n "xxx"
    [||];

  start_test "Search for /^.+[0-9][0-9][0-9]$/";
  let r = Str.regexp "^.+[0-9][0-9][0-9]$" in
  let n = 0 in
  test_search_forward r n "x123"
    [|"x123"|];
  test_search_forward r n "xx123"
    [|"xx123"|];
  test_search_forward r n "123456"
    [|"123456"|];
  test_search_forward r n "123"
    [||];
  test_search_forward r n "x123x"
    [||];

  start_test "Search for /^\\([^!]+\\)!\\(.+\\)=apquxz\\.ixr\\.zzz\\.ac\\.uk$/";
  let r = Str.regexp "^\\([^!]+\\)!\\(.+\\)=apquxz\\.ixr\\.zzz\\.ac\\.uk$" in
  let n = 2 in
  test_search_forward r n "abc!pqr=apquxz.ixr.zzz.ac.uk"
    [|"abc!pqr=apquxz.ixr.zzz.ac.uk"; "abc"; "pqr"|];
  test_search_forward r n "!pqr=apquxz.ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!=apquxz.ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!pqr=apquxz:ixr.zzz.ac.uk"
    [||];
  test_search_forward r n "abc!pqr=apquxz.ixr.zzz.ac.ukk"
    [||];

  start_test "Search for /\\([0-9a-f:]+\\)$/";
  let r = Str.regexp_case_fold "\\([0-9a-f:]+\\)$" in
  let n = 1 in
  test_search_forward r n "0abc"
    [|"0abc"; "0abc"|];
  test_search_forward r n "abc"
    [|"abc"; "abc"|];
  test_search_forward r n "fed"
    [|"fed"; "fed"|];
  test_search_forward r n "E"
    [|"E"; "E"|];
  test_search_forward r n "::"
    [|"::"; "::"|];
  test_search_forward r n "5f03:12C0::932e"
    [|"5f03:12C0::932e"; "5f03:12C0::932e"|];
  test_search_forward r n "fed def"
    [|"def"; "def"|];
  test_search_forward r n "Any old stuff"
    [|"ff"; "ff"|];
  test_search_forward r n "0zzz"
    [||];
  test_search_forward r n "gzzz"
    [||];
  test_search_forward r n "fed "
    [||];
  test_search_forward r n "Any old rubbish"
    [||];

  start_test "Search for /^[a-z0-9][a-z0-9-]*\\(\\.[a-z0-9][A-Z0-9-]*\\)*\\.$/";
  let r = Str.regexp_case_fold "^[a-z0-9][a-z0-9-]*\\(\\.[a-z0-9][A-Z0-9-]*\\)*\\.$" in
  let n = 1 in
  test_search_forward r n "a."
    [|"a."; "~"|];
  test_search_forward r n "Z."
    [|"Z."; "~"|];
  test_search_forward r n "2."
    [|"2."; "~"|];
  test_search_forward r n "ab-c."
    [|"ab-c."; "~"|];
  test_search_forward r n "ab-c.pq-r."
    [|"ab-c.pq-r."; ".pq-r"|];
  test_search_forward r n "sxk.zzz.ac.uk."
    [|"sxk.zzz.ac.uk."; ".uk"|];
  test_search_forward r n "sxk.ZZZ.ac.UK."
    [|"sxk.ZZZ.ac.UK."; ".UK"|];
  test_search_forward r n "x-.y-."
    [|"x-.y-."; ".y-"|];
  test_search_forward r n "-abc.peq."
    [||];

  start_test "Search for /^\\*\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\(\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\)*$/";
  let r = Str.regexp "^\\*\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\(\\.[a-z]\\([a-z0-9-]*[a-z0-9]+\\)?\\)*$" in
  let n = 3 in
  test_search_forward r n "*.a"
    [|"*.a"; "~"; "~"; "~"|];
  test_search_forward r n "*.b0-a"
    [|"*.b0-a"; "0-a"; "~"; "~"|];
  test_search_forward r n "*.c3-b.c"
    [|"*.c3-b.c"; "3-b"; ".c"; "~"|];
  test_search_forward r n "*.c-a.b-c"
    [|"*.c-a.b-c"; "-a"; ".b-c"; "-c"|];
  test_search_forward r n "*.0"
    [||];
  test_search_forward r n "*.a-"
    [||];
  test_search_forward r n "*.a-b.c-"
    [||];
  test_search_forward r n "*.c-a.0-c"
    [||];

  start_test "Search for /^[0-9a-fA-F]\\(\\.[0-9a-fA-F]\\)*$/";
  let r = Str.regexp "^[0-9a-fA-F]\\(\\.[0-9a-fA-F]\\)*$" in
  let n = 1 in
  test_search_forward r n "a.b.c.d"
    [|"a.b.c.d"; ".d"|];
  test_search_forward r n "A.B.C.D"
    [|"A.B.C.D"; ".D"|];
  test_search_forward r n "a.b.c.1.2.3.C"
    [|"a.b.c.1.2.3.C"; ".C"|];
  test_search_forward r n "a.b.c.dz"
    [||];
  test_search_forward r n "za"
    [||];

  start_test "Search for /^\\\".*\\\" *\\(;.*\\)?$/";
  let r = Str.regexp "^\\\".*\\\" *\\(;.*\\)?$" in
  let n = 1 in
  test_search_forward r n "\"1234\""
    [|"\"1234\""; "~"|];
  test_search_forward r n "\"abcd\" ;"
    [|"\"abcd\" ;"; ";"|];
  test_search_forward r n "\"\" ; rhubarb"
    [|"\"\" ; rhubarb"; "; rhubarb"|];
  test_search_forward r n "\"1234\" : things"
    [||];

  start_test "Search for /^\\(a\\(b\\(c\\)\\)\\)\\(d\\(e\\(f\\)\\)\\)\\(h\\(i\\(j\\)\\)\\)$/";
  let r = Str.regexp "^\\(a\\(b\\(c\\)\\)\\)\\(d\\(e\\(f\\)\\)\\)\\(h\\(i\\(j\\)\\)\\)$" in
  let n = 9 in
  test_search_forward r n "abcdefhij"
    [|"abcdefhij"; "abc"; "bc"; "c"; "def"; "ef"; "f"; "hij"; "ij"; "j"|];

  start_test "Search for /^[.^$|()*+?{,}]+/";
  let r = Str.regexp "^[.^$|()*+?{,}]+" in
  let n = 0 in
  test_search_forward r n ".^$*(+)|{?,?}"
    [|".^$*(+)|{?,?}"|];

  start_test "Search for /\\(cat\\(a\\(ract\\|tonic\\)\\|erpillar\\)\\) \\1\\(\\)2\\(3\\)/";
  let r = Str.regexp "\\(cat\\(a\\(ract\\|tonic\\)\\|erpillar\\)\\) \\1\\(\\)2\\(3\\)" in
  let n = 5 in
  test_search_forward r n "cataract cataract23"
    [|"cataract cataract23"; "cataract"; "aract"; "ract"; ""; "3"|];
  test_search_forward r n "catatonic catatonic23"
    [|"catatonic catatonic23"; "catatonic"; "atonic"; "tonic"; ""; "3"|];
  test_search_forward r n "caterpillar caterpillar23"
    [|"caterpillar caterpillar23"; "caterpillar"; "erpillar"; "~"; ""; "3"|];

  start_test "Search for /^From +\\([^ ]+\\) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]/";
  let r = Str.regexp "^From +\\([^ ]+\\) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]" in
  let n = 1 in
  test_search_forward r n "From abcd  Mon Sep 01 12:33:02 1997"
    [|"From abcd  Mon Sep 01 12:33"; "abcd"|];

  start_test "Search for /\\ba/";
  let r = Str.regexp "\\ba" in
  let n = 0 in
  test_search_forward r n "abcd"
    [|"a"|];
  test_search_forward r n "the a"
    [|"a"|];
  test_search_forward r n ".ab"
    [|"a"|];
  test_search_forward r n "bad"
    [||];
  test_search_forward r n "the ba"
    [||];
  test_search_forward r n "ba."
    [||];

  start_test "Search for /a\\b/";
  let r = Str.regexp "a\\b" in
  let n = 0 in
  test_search_forward r n "a"
    [|"a"|];
  test_search_forward r n "bcda"
    [|"a"|];
  test_search_forward r n "a foo"
    [|"a"|];
  test_search_forward r n "a."
    [|"a"|];
  test_search_forward r n "bad"
    [||];
  test_search_forward r n "ab"
    [||];

  start_test "Search for /\\([a-z]*\\)b/";
  let r = Str.regexp "\\([a-z]*\\)b" in
  let n = 1 in
  test_search_forward r n "abbb"
    [|"abbb"; "abb"|];

  start_test "Search for /\\([a-z]+\\)b/";
  let r = Str.regexp "\\([a-z]+\\)b" in
  let n = 1 in
  test_search_forward r n "abbb"
    [|"abbb"; "abb"|];

  start_test "Search for /\\([a-z]?\\)b/";
  let r = Str.regexp "\\([a-z]?\\)b" in
  let n = 1 in
  test_search_forward r n "bbbb"
    [|"bb"; "b"|];

  start_test "Search for /^a/";
  let r = Str.regexp "^a" in
  let n = 0 in
  test_search_forward r n "abcdef"
    [|"a"|];
  test_search_forward r n "zzzz\nabcdef"
    [|"a"|];

  start_test "Search for /a$/";
  let r = Str.regexp "a$" in
  let n = 0 in
  test_search_forward r n "xyza"
    [|"a"|];
  test_search_forward r n "xyza\nbcdef"
    [|"a"|];

  start_test "Null characters in regexps";
  let r = Str.regexp "ab\000cd" in
  let n = 0 in
  test_search_forward r n "qerpoiuab\000cdwerltkh"
    [| "ab\000cd" |];
  let r = Str.regexp "\000cd" in
  let n = 0 in
  test_search_forward r n "qerpoiuab\000cdwerltkh"
    [| "\000cd" |];

  (** Backward searches *)
  start_test "Backward search for /the quick/";
  let r = Str.regexp "the quick" in
  let n = 0 in
  test_search_backward r n "the quick brown fox"
    [|"the quick"|];
  test_search_backward r n "What do you know about the quick brown fox?"
    [|"the quick"|];
  test_search_backward r n "The quick brown FOX"
    [||];
  test_search_backward r n "What do you know about THE QUICK BROWN FOX?"
    [||];

  start_test "Backward search for /a\\([0-9]+\\)/";
  let r = Str.regexp "a\\([0-9]+\\)" in
  let n = 1 in
  test_search_backward r n "a123 a456zzzz"
    [|"a456"; "456"|];
  test_search_backward r n "ab123"
    [||];

  (** Partial match searches *)

  start_test "Partial match for /partial match/";
  let r = Str.regexp "partial match" in
  let n = 0 in
  test_partial_match r n ""
    [|""|];
  test_partial_match r n "partial matching"
    [|"partial match"|];
  test_partial_match r n "partial m"
    [|"partial m"|];

  start_test "Partial match for /\\(partial\\)\\|\\(match\\)/";
  let r = Str.regexp "\\(partial\\)\\|\\(match\\)" in
  let n = 2 in
  test_partial_match r n ""
    [|""; "~"; "~"|];
  test_partial_match r n "part"
    [|"part"; "~"; "~"|];
  test_partial_match r n "partial"
    [|"partial"; "partial"; "~"|];
  test_partial_match r n "matching"
    [|"match"; "~"; "match"|];
  test_partial_match r n "mat"
    [|"mat"; "~"; "~"|];
  test_partial_match r n "zorglub"
    [||];

  (** Replacement *)
  start_test "Global replacement";
  test (Str.global_replace (Str.regexp "[aeiou]") ".."
          "abcdefghijklmnopqrstuvwxyz")
       "..bcd..fgh..jklmn..pqrst..vwxyz";
  test (Str.global_replace (Str.regexp "[0-9]\\([0-9]*\\)") "-\\0-\\1-"
          "abc012def3ghi45")
       "abc-012-12-def-3--ghi-45-5-";
  test (Str.global_replace (Str.regexp "[0-9]?") "."
          "abc012def3ghi45")
       ".a.b.c....d.e.f..g.h.i...";

  start_test "First replacement";
  test (Str.replace_first (Str.regexp "[eiou]") ".."
          "abcdefghijklmnopqrstuvwxyz")
       "abcd..fghijklmnopqrstuvwxyz";
  test (Str.replace_first (Str.regexp "[0-9]\\([0-9]*\\)") "-\\0-\\1-"
          "abc012def3ghi45")
       "abc-012-12-def3ghi45";

  end_test()
