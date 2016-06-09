type line =
  | Envstatement of string
  | Test of int * string

type test_tree =
  | Node of (string list) * string * (test_tree list)

let too_deep testname max_level real_level =
  Printf.eprintf "Test %s should have depth atmost %d but has depth %d\n%!"
    testname max_level real_level;
  exit 2

let test_tree_of_lines lines =
  let rec env_of_lines = function
    | [] -> ([], [])
    | (Envstatement s)  :: lines ->
      let (env', remaining_lines) = env_of_lines lines in
      (s :: env', remaining_lines)
    | lines -> ([], lines)
  and tree_of_lines depth = function
    | [] -> (None, [])
    | line::remaining_lines as l ->
      begin match line with
        | Envstatement _ -> failwith "Unexpected env statement"
        | Test (test_depth, test_name) ->
          begin
            if test_depth > depth then too_deep test_name depth test_depth
            else if test_depth < depth then (None, l)
            else
              let (env, rem) = env_of_lines remaining_lines in
              let (trees, rem) = trees_of_lines (depth+1) rem in
              (Some (Node (env, test_name, trees)), rem)
          end
      end
  and trees_of_lines depth lines =
    let remaining_lines = ref lines in
    let trees = ref [] in
    let continue = ref true in
    while !continue; do
      let (tree, rem) = tree_of_lines 1 !remaining_lines in
      remaining_lines := rem;
      match tree with
        | None -> continue := false
        | Some t -> trees := t :: !trees
    done;
    (List.rev !trees, !remaining_lines) in
  let (env, rem) = env_of_lines lines in
  let (trees, rem) = trees_of_lines 1 rem in
  match rem with
    | [] -> (env, trees)
    | _ -> failwith "Unexpected env statements"
