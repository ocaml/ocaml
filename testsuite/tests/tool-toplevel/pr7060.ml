type t = A | B;;
let print_t out = function A -> Format.fprintf out "A";;
#install_printer print_t;;
B;;
