type ab = [ `A | `B ];;
let f (x : [`A]) = match x with #ab -> 1;;
let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
