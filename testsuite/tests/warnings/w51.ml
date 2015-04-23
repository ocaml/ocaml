
let h x = x [@inline] (* rejected *)
let h x = x [@ocaml.inline] (* rejected *)

let i x = x [@inlined] (* rejected *)
let j x = x [@ocaml.inlined] (* rejected *)
let k x = (h [@inlined]) x (* accepted *)
let k' x = (h [@ocaml.inlined]) x (* accepted *)
let l x = h x [@inlined] (* rejected *)

let m x = x [@tailcall] (* rejected *)
let n x = x [@ocaml.tailcall] (* rejected *)
let o x = (h [@tailcall]) x (* accepted *)
let p x = (h [@ocaml.tailcall]) x (* accepted *)
let q x = h x [@tailcall] (* rejected *)
