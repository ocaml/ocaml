type t = A of t | B ;;
let f = function A A B -> B | B | A B | A (A _) -> B ;;
