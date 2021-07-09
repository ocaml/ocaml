(* TEST
   * expect
*)

(** Deprecated sequences of unsigned letters *)

[@@@warning "fragile-math"]
[%%expect {|
Line 3, characters 0-27:
3 | [@@@warning "fragile-math"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert ocaml_deprecated_cli: Setting a warning with a sequence of lowercase or uppercase letters,
like 'ath', is deprecated.
Use the equivalent signed form: -f-r-a-g-i-l-e-m-a-t-h.
Hint: Enabling or disabling a warning by its mnemonic name requires a + or - prefix.
Hint: Did you make a spelling mistake when using a mnemonic name?
|}]

[@@@warning "ab-cdg+efh"]
[%%expect {|
Line 1, characters 0-25:
1 | [@@@warning "ab-cdg+efh"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert ocaml_deprecated_cli: Setting a warning with a sequence of lowercase or uppercase letters,
like 'fh', is deprecated.
Use the equivalent signed form: -a-b-c-d-g+e-f-h.
Hint: Enabling or disabling a warning by its mnemonic name requires a + or - prefix.
|}]


(** -w "a+10..." and -w "A-10..." are still supported *)
[@@@warning "a+1..20+50"]
[%%expect {|
|}]

[@@@warning "A-3..14-56"]
[%%expect {|
|}]
