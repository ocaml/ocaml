let (.?[]) = Hashtbl.find_opt
let (.@[]) = Hashtbl.find
let ( .@[]<- ) = Hashtbl.add
let (.@{}) = Hashtbl.find
let ( .@{}<- ) = Hashtbl.add
let (.@()) = Hashtbl.find
let ( .@()<- ) = Hashtbl.add

let h = Hashtbl.create 17

;;
  h.@("One") <- 1
; assert (h.@{"One"} = 1)
; assert (h.?["Two"] = None)
