(* TEST *)
open Printf

let rec build_string f n accu =
  if n <= 0
    then String.concat "" accu
    else build_string f (n-1) (f (n-1) :: accu)
;;

let char n = String.make 1 (Char.chr n);;

let reference n =
  if n = 8 then "\\b"
  else if n = 9 then "\\t"
  else if n = 10 then "\\n"
  else if n = 13 then "\\r"
  else if n = Char.code '\"' then "\\\""
  else if n = Char.code '\\' then "\\\\"
  else if n < 32 || n > 126 then Printf.sprintf "\\%03d" n
  else char n
;;

let raw_string = build_string char 256 [];;
let ref_string = build_string reference 256 [];;

if String.escaped raw_string <> ref_string then failwith "test:String.escaped";;


let check_split sep s =
  let l = String.split_on_char sep s in
  assert(List.length l > 0);
  assert(String.concat (String.make 1 sep) l = s);
  List.iter (String.iter (fun c -> assert (c <> sep))) l
;;

let () =
  let s = " abc def " in
  for i = 0 to String.length s do
    check_split ' ' (String.sub s 0 i)
  done
;;


let () =
  printf "-- Hashtbl.hash raw_string: %x\n%!" (Hashtbl.hash raw_string);
  printf "-- String.hash raw_string: %x\n%!" (String.hash raw_string);
  printf "-- Hashtbl.seeded_hash 16 raw_string: %x\n%!" (Hashtbl.seeded_hash 16 raw_string);
  printf "-- String.seeded_hash 16 raw_string: %x\n%!" (String.seeded_hash 16 raw_string);
;;

(* GPR#805/815/833 *)

let ()  =
  if Sys.word_size = 32 then begin
    let big = String.make Sys.max_string_length 'x' in
    let push x l = l := x :: !l in
    let (+=) a b = a := !a + b in
    let sz, l = ref 0, ref [] in
    while !sz >= 0 do push big l; sz += Sys.max_string_length done;
    while !sz <= 0 do push big l; sz += Sys.max_string_length done;
    try ignore (String.concat "" !l); assert false
    with Invalid_argument _ -> ();
  end

let () =
  assert(String.starts_with ~prefix:"foob" "foobarbaz");
  assert(String.starts_with ~prefix:"" "foobarbaz");
  assert(String.starts_with ~prefix:"" "");
  assert(not (String.starts_with ~prefix:"foobar" "bar"));
  assert(not (String.starts_with ~prefix:"foo" ""));
  assert(not (String.starts_with ~prefix:"fool" "foobar"));
  assert(String.ends_with ~suffix:"baz" "foobarbaz");
  assert(String.ends_with ~suffix:"" "foobarbaz");
  assert(String.ends_with ~suffix:"" "");
  assert(not (String.ends_with ~suffix:"foobar" "bar"));
  assert(not (String.ends_with ~suffix:"foo" ""));
  assert(not (String.ends_with ~suffix:"obaz" "foobar"));
;;


let () =
  let test ?limit x y d =
    assert (String.edit_distance ?limit x y = d);
    assert (String.edit_distance ?limit y x = d);
    assert (String.edit_distance ?limit x x = 0);
    assert (String.edit_distance ?limit y y = 0);
  in
  test "" "" 0;
  test "" "ab" 2;
  test "function" "function" 0;
  test "function" "fanction" 1;  (* substitute *)
  test "function" "fnction" 1;   (* delete *)
  test "function" "funiction" 1; (* insert *)
  test "function" "funtcion" 1;  (* transpose *)
  test "function" "fantcion" 2;  (* substitute + transpose *)
  test "function" "fantcio" 3;   (* substitute + transpose + delete *)
  test "function" "efantcio" 4;  (* all *)
  test "fun" "function" 5;
  test "fun" "function" ~limit:0 0;
  test "fun" "function" ~limit:1 1;
  test "fun" "function" ~limit:2 2;
  test "fun" "function" ~limit:3 3;
  test "fun" "function" ~limit:4 4;
  test "fun" "function" ~limit:5 5;
  test "fun" "function" ~limit:6 5;
  test "ca" "abc" 3 (* Damerau-Levenshtein would be 2 *);
  test "élément" "élment" 1;
  test "OCaml🐫" "O'Caml🐪" 2;
;;

let () =
  let test ?max_dist dict s res =
    let dict = fun yield -> List.iter yield dict in
    assert (String.spellcheck ?max_dist dict s = res)
  in
  (* max_dist = 0 *)
  test [""] "" [""];
  test ["a"; "b"] "" [];
  test ["a"; "b"] "a" ["a"];
  test ["a"; "b"] "d" [];
  test ["a"; "b"] "é" [];
  test ["aa"; "aé"] "aé" ["aé"];
  test ["aa"; "aé"] "ad" [];
  (* max_dist = 1 *)
  test ["abc"; "abcé"] "abc" ["abc"];
  test ["abc"; "abcé"; "abcéd"] "abé" ["abc"; "abcé"];
  test ["abcdé"; "abcdéf"] "abcd" ["abcdé"];
  (* max_dist = 2 *)
  test ["abcdéf"] "abcde" ["abcdéf"];
  test ["abcdéf"] "ubcde" [];
  let max_dist s = if String.length s <= 1 then 1 else 2 in
  test ~max_dist ["abc"] "a" [];
  test ~max_dist ["abc"; "ab"; "b"] "a" ["ab"; "b"];
  ()

let () =
  (* Some functions of the string module assume this holds *)
  assert (Sys.max_string_length - 1 < max_int)

let () =
  (* Test splitting with magnitudes *)
  (* String.take_first *)
  assert (String.take_first (-1) "" = "");
  assert (String.take_first (-1) "a" = "");
  assert (String.take_first (-1) "ab" = "");
  assert (String.take_first 0 "" = "");
  assert (String.take_first 0 "a" = "");
  assert (String.take_first 0 "ab" = "");
  assert (String.take_first 1 "" = "");
  assert (String.take_first 1 "a" = "a");
  assert (String.take_first 1 "ab" = "a");
  assert (String.take_first 2 "" = "");
  assert (String.take_first 2 "a" = "a");
  assert (String.take_first 2 "ab" = "ab");
  (* String.take_last *)
  assert (String.take_last (-1) "" = "");
  assert (String.take_last (-1) "a" = "");
  assert (String.take_last (-1) "ab" = "");
  assert (String.take_last 0 "" = "");
  assert (String.take_last 0 "a" = "");
  assert (String.take_last 0 "ab" = "");
  assert (String.take_last 1 "" = "");
  assert (String.take_last 1 "a" = "a");
  assert (String.take_last 1 "ab" = "b");
  assert (String.take_last 2 "" = "");
  assert (String.take_last 2 "a" = "a");
  assert (String.take_last 2 "ab" = "ab");
  (* String.drop_first *)
  assert (String.drop_first (-1) "" = "");
  assert (String.drop_first (-1) "a" = "a");
  assert (String.drop_first (-1) "ab" = "ab");
  assert (String.drop_first 0 "" = "");
  assert (String.drop_first 0 "a" = "a");
  assert (String.drop_first 0 "ab" = "ab");
  assert (String.drop_first 1 "" = "");
  assert (String.drop_first 1 "a" = "");
  assert (String.drop_first 1 "ab" = "b");
  assert (String.drop_first 2 "" = "");
  assert (String.drop_first 2 "a" = "");
  assert (String.drop_first 2 "ab" = "");
  (* String.drop_last *)
  assert (String.drop_last (-1) "" = "");
  assert (String.drop_last (-1) "a" = "a");
  assert (String.drop_last (-1) "ab" = "ab");
  assert (String.drop_last 0 "" = "");
  assert (String.drop_last 0 "a" = "a");
  assert (String.drop_last 0 "ab" = "ab");
  assert (String.drop_last 1 "" = "");
  assert (String.drop_last 1 "a" = "");;
  assert (String.drop_last 1 "ab" = "a");
  assert (String.drop_last 2 "" = "");
  assert (String.drop_last 2 "a" = "");
  assert (String.drop_last 2 "ab" = "");
  (* String.cut_first *)
  assert (String.cut_first (-1) "" = ("", ""));
  assert (String.cut_first (-1) "a" = ("", "a"));
  assert (String.cut_first (-1) "ab" = ("", "ab"));
  assert (String.cut_first 0 "" = ("", ""));
  assert (String.cut_first 0 "a" = ("", "a"));
  assert (String.cut_first 0 "ab" = ("", "ab"));
  assert (String.cut_first 1 "" = ("", ""));
  assert (String.cut_first 1 "a" = ("a", ""));
  assert (String.cut_first 1 "ab" = ("a", "b"));
  assert (String.cut_first 2 "" = ("", ""));
  assert (String.cut_first 2 "a" = ("a", ""));
  assert (String.cut_first 2 "ab" = ("ab", ""));
  (* String.cut_last *)
  assert (String.cut_last (-1) "" = ("", ""));
  assert (String.cut_last (-1) "a" = ("a", ""));
  assert (String.cut_last (-1) "ab" = ("ab", ""));
  assert (String.cut_last 0 "" = ("", ""));
  assert (String.cut_last 0 "a" = ("a", ""));
  assert (String.cut_last 0 "ab" = ("ab", ""));
  assert (String.cut_last 1 "" = ("", ""));
  assert (String.cut_last 1 "a" = ("", "a"));
  assert (String.cut_last 1 "ab" = ("a", "b"));
  assert (String.cut_last 2 "" = ("", ""));
  assert (String.cut_last 2 "a" = ("", "a"));
  assert (String.cut_last 2 "ab" = ("", "ab"));
  ()

let () =
  (* Test splitting with predicates *)
  (* String.take_first_while *)
  assert (String.take_first_while Char.Ascii.is_white "" = "");
  assert (String.take_first_while Char.Ascii.is_white "  " = "  ");
  assert (String.take_first_while Char.Ascii.is_white "abc" = "");
  assert (String.take_first_while Char.Ascii.is_white "  abc" = "  ");
  (* String.drop_first_while *)
  assert (String.drop_first_while Char.Ascii.is_white "" = "");
  assert (String.drop_first_while Char.Ascii.is_white "  " = "");
  assert (String.drop_first_while Char.Ascii.is_white "abc" = "abc");
  assert (String.drop_first_while Char.Ascii.is_white "  abc" = "abc");
  (* String.cut_first_while *)
  assert (String.cut_first_while Char.Ascii.is_white "" = ("", ""));
  assert (String.cut_first_while Char.Ascii.is_white "  " = ("  ", ""));
  assert (String.cut_first_while Char.Ascii.is_white "abc" = ("", "abc"));
  assert (String.cut_first_while Char.Ascii.is_white "  abc" = ("  ", "abc"));
  (* String.take_last_while *)
  assert (String.take_last_while Char.Ascii.is_white "" = "");
  assert (String.take_last_while Char.Ascii.is_white "  " = "  ");
  assert (String.take_last_while Char.Ascii.is_white "abc" = "");
  assert (String.take_last_while Char.Ascii.is_white "abc  " = "  ");
  (* String.drop_last_while *)
  assert (String.drop_last_while Char.Ascii.is_white "" = "");
  assert (String.drop_last_while Char.Ascii.is_white "  " = "");
  assert (String.drop_last_while Char.Ascii.is_white "abc" = "abc");
  assert (String.drop_last_while Char.Ascii.is_white "abc  " = "abc");
  (* String.cut_last_while *)
  assert (String.cut_last_while Char.Ascii.is_white "" = ("", ""));
  assert (String.cut_last_while Char.Ascii.is_white "  " = ("", "  "));
  assert (String.cut_last_while Char.Ascii.is_white "abc" = ("abc", ""));
  assert (String.cut_last_while Char.Ascii.is_white "abc  " = ("abc", "  "));
  ()

let () =
  (* Test String.of_char *)
  assert (String.of_char 'a' = "a");
  assert (String.of_char '\x00' = "\x00");
  ()

let () =
  (* Test String.is_empty *)
  assert (String.is_empty "life" = false);
  assert (String.is_empty "" = true);
  ()

let () =
  (* Test String.find_{first,last}_index *)
  let is_letter = Char.Ascii.is_letter in
  let is_invalid_arg f = match f () with
    | exception Invalid_argument _ -> () | _ -> assert false
  in
  is_invalid_arg (fun () -> String.find_first_index ~start:(-1) is_letter "");
  is_invalid_arg (fun () -> String.find_last_index ~start:(-1) is_letter "");
  assert (String.find_first_index ~start:0 is_letter "" = None);
  assert (String.find_last_index ~start:0 is_letter "" = None);
  is_invalid_arg (fun () -> String.find_first_index ~start:1 is_letter "");
  is_invalid_arg (fun () -> String.find_last_index ~start:1 is_letter "");
  assert (String.find_first_index ~start:0 is_letter "-" = None);
  assert (String.find_first_index ~start:1 is_letter "-" = None);
  assert (String.find_last_index ~start:0 is_letter "-" = None);
  assert (String.find_last_index ~start:1 is_letter "-" = None);
  assert (String.find_first_index ~start:0 is_letter "a-" = Some 0);
  assert (String.find_first_index ~start:1 is_letter "a-" = None);
  assert (String.find_first_index ~start:2 is_letter "a-" = None);
  assert (String.find_last_index ~start:0 is_letter "a-" = Some 0);
  assert (String.find_last_index ~start:1 is_letter "a-" = Some 0);
  assert (String.find_last_index ~start:2 is_letter "a-" = Some 0);
  assert (String.find_first_index ~start:0 is_letter "a-a" = Some 0);
  assert (String.find_first_index ~start:1 is_letter "a-a" = Some 2);
  assert (String.find_first_index ~start:2 is_letter "a-a" = Some 2);
  assert (String.find_first_index ~start:3 is_letter "a-a" = None);
  assert (String.find_last_index ~start:0 is_letter "a-a" = Some 0);
  assert (String.find_last_index ~start:1 is_letter "a-a" = Some 0);
  assert (String.find_last_index ~start:2 is_letter "a-a" = Some 2);
  assert (String.find_last_index ~start:3 is_letter "a-a" = Some 2);
  assert (String.find_first_index ~start:0 is_letter "-a-" = Some 1);
  assert (String.find_first_index ~start:1 is_letter "-a-" = Some 1);
  assert (String.find_first_index ~start:2 is_letter "-a-" = None);
  assert (String.find_first_index ~start:3 is_letter "-a-" = None);
  assert (String.find_last_index ~start:0 is_letter "-a-" = None);
  assert (String.find_last_index ~start:1 is_letter "-a-" = Some 1);
  assert (String.find_last_index ~start:2 is_letter "-a-" = Some 1);
  assert (String.find_last_index ~start:3 is_letter "-a-" = Some 1);
  ()

let () =
  (* Test String.find_first *)
  let is_invalid_arg f = match f () with
    | exception Invalid_argument _ -> () | _ -> assert false
  in
  is_invalid_arg (fun () -> String.find_first ~start:(-1) ~sub:"" "");
  assert (String.find_first ~start:0 ~sub:"" "" = Some 0);
  is_invalid_arg (fun () -> String.find_first ~start:1 ~sub:"" "");
  assert (String.find_first ~start:0 ~sub:"" "ab" = Some 0);
  assert (String.find_first ~start:1 ~sub:"" "ab" = Some 1);
  assert (String.find_first ~start:2 ~sub:"" "ab" = Some 2);
  is_invalid_arg (fun () -> String.find_first ~start:3 ~sub:"" "ab");
  assert (String.find_first ~start:0 ~sub:"a" "" = None);
  assert (String.find_first ~start:0 ~sub:"a" "a" = Some 0);
  assert (String.find_first ~start:1 ~sub:"a" "a" = None);
  assert (String.find_first ~start:0 ~sub:"a" "ba" = Some 1);
  assert (String.find_first ~start:1 ~sub:"a" "ba" = Some 1);
  assert (String.find_first ~start:2 ~sub:"a" "ba" = None);
  assert (String.find_first ~start:0 ~sub:"ab" "" = None);
  assert (String.find_first ~start:0 ~sub:"ab" "ab" = Some 0);
  assert (String.find_first ~start:0 ~sub:"ab" "aab" = Some 1);
  assert (String.find_first ~start:1 ~sub:"ab" "aab" = Some 1);
  assert (String.find_first ~start:2 ~sub:"ab" "aab" = None);
  assert (String.find_first ~start:3 ~sub:"ab" "aab" = None);
  is_invalid_arg (fun () -> String.find_first ~start:(-1) ~sub:"abaa" "aba");
  assert (String.find_first ~start:0 ~sub:"abaa" "aba" = None);
  assert (String.find_first ~start:2 ~sub:"abaa" "aba" = None);
  assert (String.find_first ~start:3 ~sub:"abaa" "aba" = None);
  is_invalid_arg (fun () -> String.find_first ~start:4 ~sub:"abaa" "aba");
  assert (String.find_first ~start:0 ~sub:"aba" "ababa" = Some 0);
  assert (String.find_first ~start:1 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_first ~start:2 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_first ~start:3 ~sub:"aba" "ababa" = None);
  assert (String.find_first ~start:4 ~sub:"aba" "ababa" = None);
  assert (String.find_first ~start:5 ~sub:"aba" "ababa" = None);
  assert (String.find_first ~start:0 ~sub:"abab" "ababab" = Some 0);
  assert (String.find_first ~start:1 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_first ~start:2 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_first ~start:3 ~sub:"abab" "ababab" = None);
  assert (String.find_first ~start:4 ~sub:"abab" "ababab" = None);
  assert (String.find_first ~start:5 ~sub:"abab" "ababab" = None);
  assert (String.find_first ~start:6 ~sub:"abab" "ababab" = None);
  assert (String.find_first ~start:0 ~sub:"aba" "xbabxbaba" = Some 6);
  assert (String.find_first ~start:0 ~sub:"xxxxaz" "yyyyazxxxxxaz" = Some 7);
  assert (String.find_first ~start:0 ~sub:"aaa" "abaacaaad" = Some 5);
  ()

let () =
  (* Test String.find_last *)
  let is_invalid_arg f = match f () with
    | exception Invalid_argument _ -> () | _ -> assert false
  in
  is_invalid_arg (fun () -> String.find_last ~start:(-1) ~sub:"" "");
  assert (String.find_last ~start:0 ~sub:"" "" = Some 0);
  is_invalid_arg (fun () -> String.find_last ~start:1 ~sub:"" "");
  assert (String.find_last ~start:0 ~sub:"" "ab" = Some 0);
  assert (String.find_last ~start:1 ~sub:"" "ab" = Some 1);
  assert (String.find_last ~start:2 ~sub:"" "ab" = Some 2);
  is_invalid_arg (fun () -> String.find_last ~start:3 ~sub:"" "ab");
  assert (String.find_last ~start:0 ~sub:"a" "" = None);
  assert (String.find_last ~start:0 ~sub:"a" "a" = Some 0);
  assert (String.find_last ~start:1 ~sub:"a" "a" = Some 0);
  assert (String.find_last ~start:0 ~sub:"a" "ba" = None);
  assert (String.find_last ~start:1 ~sub:"a" "ba" = Some 1);
  assert (String.find_last ~start:2 ~sub:"a" "ba" = Some 1);
  assert (String.find_last ~start:0 ~sub:"ab" "" = None);
  assert (String.find_last ~start:0 ~sub:"ab" "ab" = Some 0);
  assert (String.find_last ~start:0 ~sub:"ab" "aab" = None);
  assert (String.find_last ~start:1 ~sub:"ab" "aab" = Some 1);
  assert (String.find_last ~start:2 ~sub:"ab" "aab" = Some 1);
  assert (String.find_last ~start:3 ~sub:"ab" "aab" = Some 1);
  is_invalid_arg (fun () -> String.find_last ~start:(-1) ~sub:"abaa" "aba");
  assert (String.find_last ~start:0 ~sub:"abaa" "aba" = None);
  assert (String.find_last ~start:2 ~sub:"abaa" "aba" = None);
  assert (String.find_last ~start:3 ~sub:"abaa" "aba" = None);
  is_invalid_arg (fun () -> String.find_last ~start:4 ~sub:"abaa" "aba");
  assert (String.find_last ~start:0 ~sub:"aba" "ababa" = Some 0);
  assert (String.find_last ~start:1 ~sub:"aba" "ababa" = Some 0);
  assert (String.find_last ~start:2 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_last ~start:3 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_last ~start:4 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_last ~start:5 ~sub:"aba" "ababa" = Some 2);
  assert (String.find_last ~start:0 ~sub:"abab" "ababab" = Some 0);
  assert (String.find_last ~start:1 ~sub:"abab" "ababab" = Some 0);
  assert (String.find_last ~start:2 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_last ~start:3 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_last ~start:4 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_last ~start:5 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_last ~start:6 ~sub:"abab" "ababab" = Some 2);
  assert (String.find_last ~sub:"ab" "aabb" = Some 1);
  ()

let () =
  (* Test String.includes *)
  assert (String.includes ~affix:"" "" = true);
  assert (String.includes ~affix:"" "a" = true);
  assert (String.includes ~affix:"" "ab" = true);
  assert (String.includes ~affix:"a" "" = false);
  assert (String.includes ~affix:"a" "a" = true);
  assert (String.includes ~affix:"a" "ab" = true);
  assert (String.includes ~affix:"a" "ba" = true);
  assert (String.includes ~affix:"a" "bab" = true);
  assert (String.includes ~affix:"ab" "" =  false);
  assert (String.includes ~affix:"ab" "a" = false);
  assert (String.includes ~affix:"ab" "ab" = true);
  assert (String.includes ~affix:"ab" "aab" = true);
  assert (String.includes ~affix:"aab" "ab" = false);
  assert (String.includes ~affix:"ab" "aba" = true);
  assert (String.includes ~affix:"ab" "aaba" = true);
  assert (String.includes ~affix:"abab" "aababa" = true);
  ()

let () =
  (* Test String.replace_first *)
  assert (String.replace_first ~sub:"" ~by:"" "" = "");
  assert (String.replace_first ~sub:"" ~by:"a" "" = "a");
  assert (String.replace_first ~sub:"" ~by:"a" "123" = "a123");
  assert (String.replace_first ~start:1 ~sub:"" ~by:"a" "123" = "1a23");
  assert (String.replace_first ~start:2 ~sub:"" ~by:"a" "123" = "12a3");
  assert (String.replace_first ~start:3 ~sub:"" ~by:"a" "123" = "123a");
  assert (String.replace_first ~sub:"1" ~by:"" "123" = "23");
  assert (String.replace_first ~sub:"3" ~by:"" "123" = "12");
  assert (String.replace_first ~sub:"1" ~by:"" "1" = "");
  assert (String.replace_first ~sub:"12" ~by:"z" "123" = "z3");
  assert (String.replace_first ~start:2 ~sub:"" ~by:"z" "123" = "12z3");
  assert (String.replace_first ~start:3 ~sub:"" ~by:"z" "123" = "123z");
  assert (String.replace_first ~start:3 ~sub:"a" ~by:"z" "123" = "123");
  assert (String.replace_first ~sub:"aba" ~by:"1" "ababa" = "1ba");
  assert (String.replace_first ~sub:"aba" ~by:"12" "ababa" = "12ba");
  ()

let () =
  (* Test String.replace_last *)
  assert (String.replace_last ~sub:"" ~by:"" "" = "");
  assert (String.replace_last ~sub:"" ~by:"a" "" = "a");
  assert (String.replace_last ~sub:"" ~by:"a" "123" = "123a");
  assert (String.replace_last ~start:1 ~sub:"" ~by:"a" "123" = "1a23");
  assert (String.replace_last ~start:2 ~sub:"" ~by:"a" "123" = "12a3");
  assert (String.replace_last ~start:3 ~sub:"" ~by:"a" "123" = "123a");
  assert (String.replace_last ~sub:"1" ~by:"" "123" = "23");
  assert (String.replace_last ~sub:"3" ~by:"" "123" = "12");
  assert (String.replace_last ~sub:"1" ~by:"" "1" = "");
  assert (String.replace_last ~sub:"12" ~by:"z" "123" = "z3");
  assert (String.replace_last ~start:2 ~sub:"" ~by:"z" "123" = "12z3");
  assert (String.replace_last ~start:3 ~sub:"" ~by:"z" "123" = "123z");
  assert (String.replace_last ~start:3 ~sub:"a" ~by:"z" "123" = "123");
  assert (String.replace_last ~sub:"aba" ~by:"1" "ababa" = "ab1");
  assert (String.replace_last ~sub:"aba" ~by:"12" "ababa" = "ab12");
  ()

let () =
  (* Test String.replace_all *)
  assert (String.replace_all ~sub:"" ~by:"" "" = "");
  assert (String.replace_all ~sub:"" ~by:"" "1" = "1");
  assert (String.replace_all ~sub:"" ~by:"" "12" = "12");
  assert (String.replace_all ~sub:"" ~by:"a" "" = "a");
  assert (String.replace_all ~sub:"" ~by:"a" "1" = "a1a");
  assert (String.replace_all ~sub:"" ~by:"a" "12" = "a1a2a");
  assert (String.replace_all ~sub:"" ~by:"a" "123" = "a1a2a3a");
  assert (String.replace_all ~start:0 ~sub:"" ~by:"a" "123" = "a1a2a3a");
  assert (String.replace_all ~start:1 ~sub:"" ~by:"a" "123" = "1a2a3a");
  assert (String.replace_all ~start:2 ~sub:"" ~by:"a" "123" = "12a3a");
  assert (String.replace_all ~start:3 ~sub:"" ~by:"a" "123" = "123a");
  assert (String.replace_all ~sub:"1" ~by:"" "121" = "2");
  assert (String.replace_all ~sub:"1" ~by:"3" "121" = "323");
  assert (String.replace_all ~sub:"1" ~by:"" "1" = "");
  assert (String.replace_all ~sub:"12" ~by:"a" "123" = "a3");
  assert (String.replace_all ~sub:"12" ~by:"a" "123112" = "a31a");
  assert (String.replace_all ~start:1 ~sub:"12" ~by:"a" "123112" = "1231a");
  assert (String.replace_all ~sub:"12" ~by:"ab" "123112" = "ab31ab");
  ()
