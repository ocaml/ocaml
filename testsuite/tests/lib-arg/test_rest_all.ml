(* TEST
   * expect
*)

type arg = AString of string | ARest of string | ARest_all of string list

let push acc s =
  acc := s :: !acc

let f_str acc s = push acc (AString s)

let f_rest acc s = push acc (ARest s)

let f_rest_all acc ss = push acc (ARest_all ss)

let test args =
  let acc = ref [] in
  Arg.parse_argv ~current:(ref 0) args Arg.[
    "-str", String (f_str acc), "String (1)";
    "-rest", Rest (f_rest acc), "Rest (*)";
    "-rest-all", Rest_all (f_rest_all acc), "Rest_all (*)";
  ] failwith "";
  List.rev !acc

[%%expect{|
type arg = AString of string | ARest of string | ARest_all of string list
val push : 'a list ref -> 'a -> unit = <fun>
val f_str : arg list ref -> string -> unit = <fun>
val f_rest : arg list ref -> string -> unit = <fun>
val f_rest_all : arg list ref -> string list -> unit = <fun>
val test : string array -> arg list = <fun>
|}];;

let _ = test [|
  "prog";
  "-str"; "foo";
  "-str"; "bar";
  "-rest";
  "foobar";
  "-str"; "foobaz"
|];;
[%%expect{|
- : arg list =
[AString "foo"; AString "bar"; ARest "foobar"; ARest "-str"; ARest "foobaz"]
|}];;

let _ = test [|
  "prog";
  "-str"; "foo";
  "-str"; "bar";
  "-rest-all";
  "foobar";
  "-str"; "foobaz"
|];;
[%%expect{|
- : arg list =
[AString "foo"; AString "bar"; ARest_all ["foobar"; "-str"; "foobaz"]]
|}];;

(* Rest does nothing when there are no following arguments *)
let _ = test [|
  "prog";
  "-str"; "foo";
  "-str"; "bar";
  "-rest";
|];;
[%%expect{|
- : arg list = [AString "foo"; AString "bar"]
|}];;

(* Rest_all lets us detect that there were no rest arguments *)
let _ = test [|
  "prog";
  "-str"; "foo";
  "-str"; "bar";
  "-rest-all";
|];;
[%%expect{|
- : arg list = [AString "foo"; AString "bar"; ARest_all []]
|}];;
