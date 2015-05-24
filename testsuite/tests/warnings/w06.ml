let foo ~bar = () (* one label *)

let bar ~foo ~baz = () (* two labels *)

let _ = foo 2
let _ = bar 4 2
