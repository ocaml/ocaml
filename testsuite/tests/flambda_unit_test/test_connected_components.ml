open Ext_types

module SCC = Sort_connected_components.Make(Int)

let graph_1 =
  [1, [2;3;4];
   2, [3;5];
   3, [5];
   4, [1];
   5, [5]]

let empty = []

let print_scc scc =
  Printf.printf "begin\n";
  Array.iter (function
      | SCC.No_loop e -> Printf.printf "%i\n" e
      | SCC.Has_loop l ->
          Printf.printf "[%s]\n"
            (String.concat "; " (List.map string_of_int l))) scc;
  Printf.printf "end\n"

let scc graph =
  SCC.connected_components_sorted_from_roots_to_leaf
    (IntMap.map IntSet.of_list (IntMap.of_list graph))

let run () =
  print_scc (scc empty);
  print_scc (scc graph_1);
  Format.printf "done@."
