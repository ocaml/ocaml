(* TEST
 set foo = "bar";
 set expand = "\$foo";
 set var0 = "var";
 set var1 = "$var0";
 set var2 = "$var1";
 arguments = "str \$notvar $unsetvar $expand $foo $var2";
*)

let () =
  let len = Array.length Sys.argv - 1 in
  let argv = Array.sub Sys.argv 1 len in
  Array.iter print_endline argv
