let rec foldl op acc = function
    [] -> acc
    | x :: xs ->
        try foldl op (op x acc) xs [@tailcall]
        with Not_found -> assert false
