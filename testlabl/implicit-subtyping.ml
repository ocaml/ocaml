class c = object method m = 1 method n = true end;;
class d = object method m = 1 end;;

class e (x : d) = object method d = x end;;
class f (x : c) = object inherit e x end;;

let f (x : d) = x#m;;

List.map f [new c; new d];;

let g l = List.map f l;;
g [new c; new d];;
