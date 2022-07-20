let store: string list Atomic.t = Atomic.make []

let add x =
  let rec swap prev =
    if Atomic.compare_and_set store prev (x :: prev) then ()
    else swap (Atomic.get store) in
  swap (Atomic.get store)
