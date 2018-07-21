type 'a t = 'a

let map f x = f x

let (<@@>) f x = f x

let (<*>) f x = f x

let return x = x

let bind x f = f x

let (>>=) x f = f x

let compose_after g f x = g (f x)

let compose_before f g x = g (f x)
