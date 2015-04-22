
let a = (fun x -> x) [@inline]
let b = (fun x -> x) [@inline never]
let c = (fun x -> x) [@inline force]
let d = (fun x -> x) [@inline malformed attribute]
let e = (fun x -> x) [@inline malformed_attribute]
let f = (fun x -> x) [@inline : malformed_attribute]
let g = (fun x -> x) [@inline ? malformed_attribute]
