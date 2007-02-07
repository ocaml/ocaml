(* Testglob *)

open Bool;;
open Glob;;

let yep f x =
  try
    ignore (f x);
    true
  with
  | _ -> false
;;

let tests1 = [
  "\"hello\"",           true;
  "<hello>",             true;
  "<hel*lo>",            true;
  "<a> and <b> or <c>",  true;
  "<a> titi",            false
];;

let tests2 = [
  "<[a]>",              ["a"], ["b"];
  "<[a-z]>",            ["a";"e";"k";"z"], ["0";"A";"~"];
  "<[a-z][0-9]>",       ["a0";"b9"], ["a00";"a0a";"b0a";"isduis";""];
  "<hello>",            ["hello"], ["helli"];
  "\"hello\"",          ["hello"], ["heidi"];
  "<*>",                ["";"a";"ax"], [];
  "<a*b>",              ["ab";"acb";"axxxxxb";"ababbababb"], ["abx";"xxxxxab";"xab"];
  "<*.ml>",             ["hello.ml";".ml"], ["ml"; ""; "toto.mli"];
  "<a>",                ["a"], ["";"aa";"ba";"ab";"abaa"];
  "<ab>",               ["ab"], ["";"abab";"aba";"abx"];
  "<ab?c>",             ["abac";"abxc"], ["abab";"ababab";"ababa"];
  "<*ab?cd*>",          ["123abecd345";"abccd";"abccd345";"ababcababccdab"], ["abcd";"aaaaabcdababcd"];
  "<*this*is*a*test*>", ["this is a test";"You know this is a test really";"thisisatest"], ["thisatest"];
  "<b*>",               ["bxx";"bx"], ["aaab";""];
  "<*>",                ["";"a";"aaa";"aaaaa"], [];
  "<?>",                ["a"],["";"aaa";"aaaaa"];
  "<{a,b}>",              ["a";"b"],["";"aa";"ab";"ba";"bb";"c"];
  "<toto.{ml,mli}>",      ["toto.ml";"toto.mli"],["toto.";"toto.mll"];
  "<{a,b}{c,[de]}{f,g}>", ["acf";"acg";"adf";"adg";"aef";"aeg";"bcf";"bcg";"bdf";"bdg";"bef";"beg"],
                          ["afg";"af";"aee"];
  "(<*.ml> or <*.mli>) and not \"hello.ml\"",
     ["a.ml"; "b.ml"; "a.mli"],
     ["hello.ml"; "a.mli.x"];
  "<*>",   ["alpha";"beta"], ["alpha/beta";"gamma/delta"];
  "<alpha/**/beta>",  ["alpha/beta";"alpha/gamma/beta";"alpha/gamma/delta/beta"],
                      ["alpha";"beta";"gamma/delta"];
  "<**/*.ml>",  ["toto.ml";"toto/tata.ml";"alpha/gamma/delta/beta.ml"],
                ["toto.mli"];
  "<toto/**>",  ["toto/";"toto/tata";"toto/alpha/gamma/delta/beta.ml";"toto"],
                ["toto2/tata"; "tata/titi"]
];;


let _ =
  let times = 3 in
  List.iter
    begin fun (str, ast) ->
      let ast' = yep Glob.parse str in
      if ast <> ast' then
        begin
          Printf.printf "Globexp parsing failed for %S.\n%!" str;
          exit 1
        end
      else
        Printf.printf "Globexp for %S OK\n%!" str
    end
    tests1;
  List.iter
    begin fun (gstr, yes, no) ->
      let globber = Glob.parse gstr in
      let check polarity =
        List.iter
          begin fun y ->
            if Glob.eval globber y = polarity then
              Printf.printf "Glob.eval %S %S = %b OK\n%!" gstr y polarity
            else
              begin
                Printf.printf "Glob.eval %S %S = %b FAIL\n%!" gstr y (not polarity);
                exit 1
              end
          end
      in
      for k = 1 to times do
        check true yes;
        check false no
      done
    end
    tests2
;;
