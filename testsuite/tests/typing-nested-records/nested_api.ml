type t = { info : { name : string } }

let make name = { info = { name } }
let info t = t.info
