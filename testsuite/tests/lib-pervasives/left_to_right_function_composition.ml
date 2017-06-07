let l1 = [(1,"foo"); (2,"bar"); (3,"baz")]
let l2 = List.map (fst %> succ) l1
let l3 = List.map (fst %> succ %> pred) l1
let l4 = List.map (snd %> String.uppercase_ascii) l1

;;

List.iter (Printf.printf "%d\n") l2;
print_newline ();
List.iter (Printf.printf "%d\n") l3;
print_newline ();
List.iter (Printf.printf "%s\n") l4;
