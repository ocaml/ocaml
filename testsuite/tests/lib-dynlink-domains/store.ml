let store: string list Atomic.t = Atomic.make []

let add x = Atomic.update (fun prev -> x :: prev) store
