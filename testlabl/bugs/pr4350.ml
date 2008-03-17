type 'a s = [ `A of 'a | `B ]
type 'a t = { s : 'a s }
let ko (t: 'a t) =
        (function `A _ -> () | _ -> ()) t.s
