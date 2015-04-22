
let f = (fun x -> x) [@inline] [@inline never]
let g = (fun x -> x) [@inline] [@something_else] [@ocaml.inline]
