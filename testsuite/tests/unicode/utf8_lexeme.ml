(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

module Utf8 = Misc.Utf8_lexeme
type 'a mismatch = { output: 'a; expected: 'a }
type test_result = (unit, (Utf8.t, Utf8.t) Result.t mismatch) result
let test f input expected : test_result =
  let output = f input in
  if output = expected then Ok ()
  else Error { output; expected }
[%%expect {|
module Utf8 = Misc.Utf8_lexeme
type 'a mismatch = { output : 'a; expected : 'a; }
type test_result = (unit, (Utf8.t, Utf8.t) Result.t mismatch) result
val test :
  ('a -> (Utf8.t, Utf8.t) Result.t) ->
  'a -> (Utf8.t, Utf8.t) Result.t -> test_result = <fun>
|}];;


(* empty string *)

test Utf8.normalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;



(* ascii-only fast path *)

test Utf8.normalize "hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "hello" (Ok "Hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "Hello" (Ok "Hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "Hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;


(* non-ascii-only, no normalization *)

test Utf8.normalize "helloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "helloÀ" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "HelloÀ" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "helloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "HelloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* non-ascii-only, normalization on first char *)
test Utf8.normalize "A\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "A\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "a\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "A\xcc\x80" (Ok "à");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "a\xcc\x80" (Ok "à");;
[%%expect {|
- : test_result = Ok ()
|}];;


(* non-ascii-only, normalization on non-first char *)

test Utf8.normalize "helloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "helloA\xcc\x80" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "HelloA\xcc\x80" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "helloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "HelloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;



(* outside the ascii-only fast path: error *)

test Utf8.normalize "hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "hello\255" (Error "Hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "Hello\255" (Error "Hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "Hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;
