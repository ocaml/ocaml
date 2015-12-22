type 'a t
type a

let f : < .. > t -> unit = fun _ -> ();;

let g : [< `b] t -> unit = fun _ -> ();;

let h : [> `b] t -> unit = fun _ -> ();;

let _ = fun (x : a t) -> f x;;

let _ = fun (x : a t) -> g x;;

let _ = fun (x : a t) -> h x;;
