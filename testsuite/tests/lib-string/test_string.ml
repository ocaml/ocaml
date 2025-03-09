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
