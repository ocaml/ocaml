(* TEST
   * expect
*)

(* this is a set of tests to test the #show functionality
 * of toplevel *)

#show Foo;;
[%%expect {|
Unknown element.
|}];;

module type S = sig type t val x : t end;;
module M : S = struct type t = int let x = 3 end;;

[%%expect {|
module type S = sig type t val x : t end
module M : S
|}];;

#show M;;
[%%expect {|
module M : S
|}];;

#show S;;
[%%expect {|
module type S = sig type t val x : t end
|}];;

#show Invalid_argument;;
[%%expect {|
exception Invalid_argument of string
|}];;

#show Some;;
[%%expect {|
type 'a option = None | Some of 'a
|}];;

#show option;;
[%%expect {|
type nonrec 'a option = None | Some of 'a
|}];;

#show Open_binary;;
[%%expect {|
type Stdlib.open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
|}];;

#show open_flag;;
[%%expect {|
type nonrec open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
|}];;

type extensible = ..;;
type extensible += A | B of int;;
[%%expect {|
type extensible = ..
type extensible += A | B of int
|}];;

#show A;;
[%%expect {|
type extensible += A
|}];;

#show B;;
[%%expect {|
type extensible += B of int
|}];;

#show extensible;;
[%%expect {|
type nonrec extensible = ..
|}];;

type 'a t = ..;;
type _ t += A : int t;;
[%%expect{|
type 'a t = ..
type _ t += A : int t
|}];;

#show A;;
[%%expect{|
type 'a t += A : int t
|}];;
