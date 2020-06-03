type 'a t = 'a Original.t = T

let f: (module Original.T with type t = int) -> unit = fun _ -> ()
let x = (module struct type t end: Original.T )
let g: (module Original.T) -> unit = fun _ -> ()
