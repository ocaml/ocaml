(* TEST
 set foo = "bar";
 set expand = "\$foo";
 arguments = "str \$notvar $unsetvar $expand $foo";
*)

let () =
  let len = Array.length Sys.argv - 1 in
  let argv = Array.sub Sys.argv 1 len in
  Array.iter print_endline argv
