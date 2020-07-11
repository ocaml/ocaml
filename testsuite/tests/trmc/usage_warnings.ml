(* TEST
   * expect *)

(* build-up *)
let[@tail_mod_cons] rec append xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> x :: append xs ys
[%%expect {|
val append : 'a list -> 'a list -> 'a list = <fun>
|}]

(* incorrect version: this cannot work *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss -> append xs (flatten xss)
[%%expect {|
Line 3, characters 17-40:
3 |   | xs :: xss -> append xs (flatten xss)
                     ^^^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be in tail position in the transformed version.
Please either mark the called function with the [@tail_mod_cons] attribute,
or mark this call with the [@tailcall false] attribute to make its
non-tailness explicit.
Lines 1-3, characters 34-40:
1 | ..................................function
2 |   | [] -> []
3 |   | xs :: xss -> append xs (flatten xss)
Warning 71 [unused-tmc-attribute]: This function is marked @tail_mod_cons but is never applied in TMC position.
val flatten : 'a list list -> 'a list = <fun>
|}]

(* correct version *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss ->
      let[@tail_mod_cons] rec append_flatten xs xss =
        match xs with
        | [] -> flatten xss
        | x :: xs -> x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
val flatten : 'a list list -> 'a list = <fun>
|}]

(* incorrect version *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss ->
      let rec append_flatten xs xss =
        match xs with
        | [] -> flatten xss
        | x :: xs ->
            (* incorrect: this call to append_flatten is not transformed *)
            x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
Line 10, characters 9-30:
10 |       in append_flatten xs xss
              ^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be in tail position in the transformed version.
Please either mark the called function with the [@tail_mod_cons] attribute,
or mark this call with the [@tailcall false] attribute to make its
non-tailness explicit.
Lines 1-10, characters 34-30:
 1 | ..................................function
 2 |   | [] -> []
 3 |   | xs :: xss ->
 4 |       let rec append_flatten xs xss =
 5 |         match xs with
 6 |         | [] -> flatten xss
 7 |         | x :: xs ->
 8 |             (* incorrect: this call to append_flatten is not transformed *)
 9 |             x :: append_flatten xs xss
10 |       in append_flatten xs xss
Warning 71 [unused-tmc-attribute]: This function is marked @tail_mod_cons but is never applied in TMC position.
val flatten : 'a list list -> 'a list = <fun>
|}]

(* incorrect version: the call to append-flatten is not transformed *)
let rec flatten = function
  | [] -> []
  | xs :: xss ->
      let[@tail_mod_cons] rec append_flatten xs xss =
        match xs with
        | [] ->
            (* incorrect: if flatten does not have a TMC version,
               this call is not tail-recursive in the TMC version of
               append-flatten, so this version is in fact equivalent
               to the "cannot work" version above: the "append" part
               runs in constant stack space, but the "flatten" part is
               not tail-recursive. *)
            flatten xss
        | x :: xs ->
            x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
Line 13, characters 12-23:
13 |             flatten xss
                 ^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be in tail position in the transformed version.
Please either mark the called function with the [@tail_mod_cons] attribute,
or mark this call with the [@tailcall false] attribute to make its
non-tailness explicit.
val flatten : 'a list list -> 'a list = <fun>
|}]
