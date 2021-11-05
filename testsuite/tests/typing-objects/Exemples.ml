(* TEST
   * expect
*)

class point x_init = object
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
end;;
[%%expect{|
class point :
  int ->
  object val mutable x : int method get_x : int method move : int -> unit end
|}];;

let p = new point 7;;
[%%expect{|
val p : point = <obj>
|}];;

p#get_x;;
[%%expect{|
- : int = 7
|}];;
p#move 3;;
[%%expect{|
- : unit = ()
|}];;
p#get_x;;
[%%expect{|
- : int = 10
|}];;

let q = Oo.copy p;;
[%%expect{|
val q : point = <obj>
|}, Principal{|
val q : < get_x : int; move : int -> unit > = <obj>
|}];;

q#move 7; p#get_x, q#get_x;;
[%%expect{|
- : int * int = (10, 17)
|}];;

class color_point x (c : string) = object
  inherit point x
  val c = c
  method color = c
end;;
[%%expect{|
class color_point :
  int ->
  string ->
  object
    val c : string
    val mutable x : int
    method color : string
    method get_x : int
    method move : int -> unit
  end
|}];;

let p' = new color_point 5 "red";;
[%%expect{|
val p' : color_point = <obj>
|}];;

p'#get_x, p'#color;;
[%%expect{|
- : int * string = (5, "red")
|}];;

let l = [p; (p' :> point)];;
[%%expect{|
val l : point list = [<obj>; <obj>]
|}];;

let get_x p = p#get_x;;
[%%expect{|
val get_x : < get_x : 'a; .. > -> 'a = <fun>
|}];;
let set_x p = p#set_x;;
[%%expect{|
val set_x : < set_x : 'a; .. > -> 'a = <fun>
|}];;
List.map get_x l;;
[%%expect{|
- : int list = [10; 5]
|}];;

class ref x_init = object
  val mutable x = x_init
  method get = x
  method set y = x <- y
end;;
[%%expect{|
Lines 1-5, characters 0-3:
1 | class ref x_init = object
2 |   val mutable x = x_init
3 |   method get = x
4 |   method set y = x <- y
5 | end..
Error: Some type variables are unbound in this type:
         class ref :
           'a ->
           object
             val mutable x : 'a
             method get : 'a
             method set : 'a -> unit
           end
       The method get has type 'a where 'a is unbound
|}];;

class ref (x_init:int) = object
  val mutable x = x_init
  method get = x
  method set y = x <- y
end;;
[%%expect{|
class ref :
  int ->
  object val mutable x : int method get : int method set : int -> unit end
|}];;

class ['a] ref x_init = object
  val mutable x = (x_init : 'a)
  method get = x
  method set y = x <- y
end;;
[%%expect{|
class ['a] ref :
  'a -> object val mutable x : 'a method get : 'a method set : 'a -> unit end
|}];;

let r = new ref 1 in r#set 2; (r#get);;
[%%expect{|
- : int = 2
|}];;

class ['a] circle (c : 'a) = object
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = (center#move : int -> unit)
end;;
[%%expect{|
class ['a] circle :
  'a ->
  object
    constraint 'a = < move : int -> unit; .. >
    val mutable center : 'a
    method center : 'a
    method move : int -> unit
    method set_center : 'a -> unit
  end
|}];;

class ['a] circle (c : 'a) = object
  constraint 'a = #point
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = center#move
end;;
[%%expect{|
class ['a] circle :
  'a ->
  object
    constraint 'a = #point
    val mutable center : 'a
    method center : 'a
    method move : int -> unit
    method set_center : 'a -> unit
  end
|}];;

let (c, c') = (new circle p, new circle p');;
[%%expect{|
val c : point circle = <obj>
val c' : color_point circle = <obj>
|}, Principal{|
val c : point circle = <obj>
val c' : < color : string; get_x : int; move : int -> unit > circle = <obj>
|}];;

class ['a] color_circle c = object
  constraint 'a = #color_point
  inherit ['a] circle c
  method color = center#color
end;;
[%%expect{|
class ['a] color_circle :
  'a ->
  object
    constraint 'a = #color_point
    val mutable center : 'a
    method center : 'a
    method color : string
    method move : int -> unit
    method set_center : 'a -> unit
  end
|}];;

let c'' = new color_circle p;;
[%%expect{|
Line 1, characters 27-28:
1 | let c'' = new color_circle p;;
                               ^
Error: This expression has type point but an expression was expected of type
         #color_point
       The first object type has no method color
|}];;
let c'' = new color_circle p';;
[%%expect{|
val c'' : color_point color_circle = <obj>
|}];;

(c'' :> color_point circle);;
[%%expect{|
- : color_point circle = <obj>
|}];;
(c'' :> point circle);;
[%%expect{|
Line 1, characters 0-21:
1 | (c'' :> point circle);;
    ^^^^^^^^^^^^^^^^^^^^^
Error: Type
         color_point color_circle =
           < center : color_point; color : string; move : int -> unit;
             set_center : color_point -> unit >
       is not a subtype of
         point circle =
           < center : point; move : int -> unit; set_center : point -> unit >
       Type point is not a subtype of color_point
       The first object type has no method color
|}];;                 (* Fail *)
fun x -> (x : color_point color_circle :> point circle);;
[%%expect{|
Line 1, characters 9-55:
1 | fun x -> (x : color_point color_circle :> point circle);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type
         color_point color_circle =
           < center : color_point; color : string; move : int -> unit;
             set_center : color_point -> unit >
       is not a subtype of
         point circle =
           < center : point; move : int -> unit; set_center : point -> unit >
       Type point is not a subtype of color_point
       The first object type has no method color
|}];;

class printable_point y = object (s)
  inherit point y
  method print = Format.print_int s#get_x
end;;
[%%expect{|
class printable_point :
  int ->
  object
    val mutable x : int
    method get_x : int
    method move : int -> unit
    method print : unit
  end
|}];;

let p = new printable_point 7;;
[%%expect{|
val p : printable_point = <obj>
|}];;
p#print;;
[%%expect{|
- : unit = ()
|}];;

class printable_color_point y c = object (self)
  inherit color_point y c
  inherit printable_point y as super
  method print =
    Format.print_string "(";
    super#print;
    Format.print_string ", ";
    Format.print_string (self#color);
    Format.print_string ")"
end;;
[%%expect{|
Line 3, characters 2-36:
3 |   inherit printable_point y as super
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 13 [instance-variable-override]: the following instance variables are overridden by the class printable_point :
  x
class printable_color_point :
  int ->
  string ->
  object
    val c : string
    val mutable x : int
    method color : string
    method get_x : int
    method move : int -> unit
    method print : unit
  end
|}];;

let p' = new printable_color_point 7 "red";;
[%%expect{|
val p' : printable_color_point = <obj>
|}];;
p'#print;;
[%%expect{|
- : unit = ()
|}];;

class functional_point y = object
  val x = y
  method get_x = x
  method move d = {< x = x + d >}
end;;
[%%expect{|
class functional_point :
  int ->
  object ('a) val x : int method get_x : int method move : int -> 'a end
|}];;

let p = new functional_point 7;;
[%%expect{|
val p : functional_point = <obj>
|}];;

p#get_x;;
[%%expect{|
- : int = 7
|}];;
(p#move 3)#get_x;;
[%%expect{|
- : int = 10
|}];;
p#get_x;;
[%%expect{|
- : int = 7
|}];;

fun x -> (x :> functional_point);;
[%%expect{|
- : #functional_point -> functional_point = <fun>
|}];;

(*******************************************************************)

class virtual ['a] lst () = object (self)
  method virtual null : bool
  method virtual hd : 'a
  method virtual tl : 'a lst
  method map f =
    (if self#null then
       new nil ()
     else
       new cons (f self#hd) (self#tl#map f)
     : 'a lst)
  method iter (f : 'a -> unit) =
    if self#null then ()
    else begin
      f self#hd;
      self#tl#iter f
    end
  method print (f : 'a -> unit) =
    Format.print_string "(";
    self#iter (fun x -> f x; Format.print_string "::");
    Format.print_string "[]";
    Format.print_string ")"
end and ['a] nil () = object
  inherit ['a] lst ()
  method null = true
  method hd   = failwith "hd"
  method tl   = failwith "tl"
end and ['a] cons h t = object
  inherit ['a] lst ()
  val h = h val t = t
  method null = false
  method hd   = h
  method tl   = t
end;;
[%%expect{|
class virtual ['a] lst :
  unit ->
  object
    method virtual hd : 'a
    method iter : ('a -> unit) -> unit
    method map : ('a -> 'a) -> 'a lst
    method virtual null : bool
    method print : ('a -> unit) -> unit
    method virtual tl : 'a lst
  end
and ['a] nil :
  unit ->
  object
    method hd : 'a
    method iter : ('a -> unit) -> unit
    method map : ('a -> 'a) -> 'a lst
    method null : bool
    method print : ('a -> unit) -> unit
    method tl : 'a lst
  end
and ['a] cons :
  'a ->
  'a lst ->
  object
    val h : 'a
    val t : 'a lst
    method hd : 'a
    method iter : ('a -> unit) -> unit
    method map : ('a -> 'a) -> 'a lst
    method null : bool
    method print : ('a -> unit) -> unit
    method tl : 'a lst
  end
|}];;

let l1 = new cons 3 (new cons 10 (new nil ()));;
[%%expect{|
val l1 : int lst = <obj>
|}];;

l1#print Format.print_int;;
[%%expect{|
- : unit = ()
|}];;

let l2 = l1#map (fun x -> x + 1);;
[%%expect{|
val l2 : int lst = <obj>
|}];;
l2#print Format.print_int;;
[%%expect{|
- : unit = ()
|}];;

let rec map_list f (x:'a lst) =
  if x#null then new nil()
  else new cons (f x#hd) (map_list f x#tl);;
[%%expect{|
val map_list : ('a -> 'b) -> 'a lst -> 'b lst = <fun>
|}];;

let p1 = (map_list (fun x -> new printable_color_point x "red") l1);;
[%%expect{|
val p1 : printable_color_point lst = <obj>
|}];;
p1#print (fun x -> x#print);;
[%%expect{|
- : unit = ()
|}];;

(*******************************************************************)

class virtual comparable () = object (self : 'a)
  method virtual cmp : 'a -> int
  end;;
[%%expect{|
class virtual comparable :
  unit -> object ('a) method virtual cmp : 'a -> int end
|}];;

class int_comparable (x : int) = object
  inherit comparable ()
  val x = x
  method x = x
  method cmp p = compare x p#x
end;;
[%%expect{|
class int_comparable :
  int -> object ('a) val x : int method cmp : 'a -> int method x : int end
|}];;

class int_comparable2 xi = object
  inherit int_comparable xi
  val mutable x' = xi
  method set_x y = x' <- y
end;;
[%%expect{|
class int_comparable2 :
  int ->
  object ('a)
    val x : int
    val mutable x' : int
    method cmp : 'a -> int
    method set_x : int -> unit
    method x : int
  end
|}];;

class ['a] sorted_list () = object
  constraint 'a = #comparable
  val mutable l = ([] : 'a list)
  method add x =
    let rec insert =
      function
            []     ->  [x]
      | a::l as l' -> if a#cmp x <= 0 then a::(insert l) else x::l'
    in
      l <- insert l
  method hd = List.hd l
end;;
[%%expect{|
class ['a] sorted_list :
  unit ->
  object
    constraint 'a = #comparable
    val mutable l : 'a list
    method add : 'a -> unit
    method hd : 'a
  end
|}];;

let l = new sorted_list ();;
[%%expect{|
val l : _#comparable sorted_list = <obj>
|}];;
let c = new int_comparable 10;;
[%%expect{|
val c : int_comparable = <obj>
|}];;
l#add c;;
[%%expect{|
- : unit = ()
|}];;

let c2 = new int_comparable2 15;;
[%%expect{|
val c2 : int_comparable2 = <obj>
|}];;
l#add (c2 :> int_comparable);;
[%%expect{|
Line 1, characters 6-28:
1 | l#add (c2 :> int_comparable);;
          ^^^^^^^^^^^^^^^^^^^^^^
Error: Type
         int_comparable2 =
           < cmp : int_comparable2 -> int; set_x : int -> unit; x : int >
       is not a subtype of
         int_comparable = < cmp : int_comparable -> int; x : int >
       Type int_comparable = < cmp : int_comparable -> int; x : int >
       is not a subtype of
         int_comparable2 =
           < cmp : int_comparable2 -> int; set_x : int -> unit; x : int >
       The first object type has no method set_x
|}];;      (* Fail : 'a comp2 is not a subtype *)
(new sorted_list ())#add c2;;
[%%expect{|
- : unit = ()
|}];;

class int_comparable3 (x : int) = object
  val mutable x = x
  method cmp (y : int_comparable) = compare x y#x
  method x = x
  method setx y = x <- y
end;;
[%%expect{|
class int_comparable3 :
  int ->
  object
    val mutable x : int
    method cmp : int_comparable -> int
    method setx : int -> unit
    method x : int
  end
|}];;

let c3 = new int_comparable3 15;;
[%%expect{|
val c3 : int_comparable3 = <obj>
|}];;
l#add (c3 :> int_comparable);;
[%%expect{|
- : unit = ()
|}];;
(new sorted_list ())#add c3;;
[%%expect{|
Line 1, characters 25-27:
1 | (new sorted_list ())#add c3;;
                             ^^
Error: This expression has type
         int_comparable3 =
           < cmp : int_comparable -> int; setx : int -> unit; x : int >
       but an expression was expected of type
         #comparable as 'a = < cmp : 'a -> int; .. >
       Type int_comparable = < cmp : int_comparable -> int; x : int >
       is not compatible with type
         #comparable as 'a = < cmp : 'a -> int; .. >
       The first object type has no method setx
|}];;   (* Error; strange message with -principal *)

let sort (l : #comparable list) = List.sort (fun x -> x#cmp) l;;
[%%expect{|
val sort : (#comparable as 'a) list -> 'a list = <fun>
|}];;
let pr l =
  List.map (fun c -> Format.print_int c#x; Format.print_string " ") l;
  Format.print_newline ();;
[%%expect{|
Line 2, characters 2-69:
2 |   List.map (fun c -> Format.print_int c#x; Format.print_string " ") l;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.
val pr : < x : int; .. > list -> unit = <fun>
|}];;
let l = [new int_comparable 5; (new int_comparable3 2 :> int_comparable);
         new int_comparable 4];;
[%%expect{|
val l : int_comparable list = [<obj>; <obj>; <obj>]
|}];;
pr l;;
[%%expect{|
7(7, red)(3::10::[])(4::11::[])((3, red)::(10, red)::[])5 2 4
- : unit = ()
|}];;
pr (sort l);;
[%%expect{|
2 4 5
- : unit = ()
|}];;
let l = [new int_comparable2 2; new int_comparable2 0];;
[%%expect{|
val l : int_comparable2 list = [<obj>; <obj>]
|}];;
pr l;;
[%%expect{|
2 0
- : unit = ()
|}];;
pr (sort l);;
[%%expect{|
0 2
- : unit = ()
|}];;

let min (x : #comparable) y =
  if x#cmp y <= 0 then x else y;;
[%%expect{|
val min : (#comparable as 'a) -> 'a -> 'a = <fun>
|}];;

(min (new int_comparable  7) (new int_comparable 11))#x;;
[%%expect{|
- : int = 7
|}];;
(min (new int_comparable2 5) (new int_comparable2 3))#x;;
[%%expect{|
- : int = 3
|}];;

(*******************************************************************)

class ['a] link (x : 'a) = object (self : 'b)
  val mutable x = x
  val mutable next = (None : 'b option)
  method x = x
  method  next = next
  method  set_x y = x <- y
  method  set_next l = next <- l
  method  append l =
    match next with
      None ->
        self#set_next l
    | Some l' ->
        l'#append l
end;;
[%%expect{|
class ['a] link :
  'a ->
  object ('b)
    val mutable next : 'b option
    val mutable x : 'a
    method append : 'b option -> unit
    method next : 'b option
    method set_next : 'b option -> unit
    method set_x : 'a -> unit
    method x : 'a
  end
|}];;

class ['a] double_link x = object (self)
  inherit ['a] link x
  val mutable prev = None
  method prev = prev
  method  set_next l =
         next <- l;
         match l with Some l -> l#set_prev (Some self) | None -> ()
  method  set_prev l = prev <- l
end;;
[%%expect{|
class ['a] double_link :
  'a ->
  object ('b)
    val mutable next : 'b option
    val mutable prev : 'b option
    val mutable x : 'a
    method append : 'b option -> unit
    method next : 'b option
    method prev : 'b option
    method set_next : 'b option -> unit
    method set_prev : 'b option -> unit
    method set_x : 'a -> unit
    method x : 'a
  end
|}];;

let rec fold_right f (l : 'a #link option) accu =
  match l with
    None -> accu
  | Some l ->
      f l#x (fold_right f l#next accu);;
[%%expect{|
val fold_right : ('a -> 'b -> 'b) -> 'a #link option -> 'b -> 'b = <fun>
|}];;

(*******************************************************************)

class calculator () = object (self)
  val mutable arg = 0.
  val mutable acc = 0.
  val mutable equals = function s -> s#arg
  method arg = arg
  method acc = acc
  method enter n = arg <- n; self
  method add =
    acc <- equals self;
    equals <- (function s -> s#acc +. s#arg);
    self
  method sub =
    acc <- equals self;
    equals <- (function s -> s#acc -. s#arg);
    self
  method equals = equals self
end;;
[%%expect{|
class calculator :
  unit ->
  object ('a)
    val mutable acc : float
    val mutable arg : float
    val mutable equals : 'a -> float
    method acc : float
    method add : 'a
    method arg : float
    method enter : float -> 'a
    method equals : float
    method sub : 'a
  end
|}];;

((new calculator ())#enter 5.)#equals;;
[%%expect{|
- : float = 5.
|}];;
(((new calculator ())#enter 5.)#sub#enter 3.5)#equals;;
[%%expect{|
- : float = 1.5
|}];;
((new calculator ())#enter 5.)#add#add#equals;;
[%%expect{|
- : float = 15.
|}];;

class calculator () = object (self)
  val mutable arg = 0.
  val mutable acc = 0.
  val mutable equals = function s -> s#arg
  method arg = arg
  method acc = acc
  method enter n = arg <- n; self
  method add = {< acc = equals self; equals = function s -> s#acc +. s#arg >}
  method sub = {< acc = equals self; equals = function s -> s#acc -. s#arg >}
  method equals = equals self
end;;
[%%expect{|
class calculator :
  unit ->
  object ('a)
    val mutable acc : float
    val mutable arg : float
    val mutable equals : 'a -> float
    method acc : float
    method add : 'a
    method arg : float
    method enter : float -> 'a
    method equals : float
    method sub : 'a
  end
|}];;

((new calculator ())#enter 5.)#equals;;
[%%expect{|
- : float = 5.
|}];;
(((new calculator ())#enter 5.)#sub#enter 3.5)#equals;;
[%%expect{|
- : float = 1.5
|}];;
((new calculator ())#enter 5.)#add#add#equals;;
[%%expect{|
- : float = 15.
|}];;

class calculator arg acc = object (self)
  val arg = arg
  val acc = acc
  method enter n = new calculator n acc
  method add = new calculator_add arg self#equals
  method sub = new calculator_sub arg self#equals
  method equals = arg
end and calculator_add arg acc = object
  inherit calculator arg acc
  method enter n = new calculator_add n acc
  method equals = acc +. arg
end and calculator_sub arg acc = object
  inherit calculator arg acc
  method enter n = new calculator_sub n acc
  method equals = acc -. arg
end;;
[%%expect{|
class calculator :
  float ->
  float ->
  object
    val acc : float
    val arg : float
    method add : calculator
    method enter : float -> calculator
    method equals : float
    method sub : calculator
  end
and calculator_add :
  float ->
  float ->
  object
    val acc : float
    val arg : float
    method add : calculator
    method enter : float -> calculator
    method equals : float
    method sub : calculator
  end
and calculator_sub :
  float ->
  float ->
  object
    val acc : float
    val arg : float
    method add : calculator
    method enter : float -> calculator
    method equals : float
    method sub : calculator
  end
|}];;

let calculator = new calculator 0. 0.;;
[%%expect{|
val calculator : calculator = <obj>
|}];;

(calculator#enter 5.)#equals;;
[%%expect{|
- : float = 5.
|}];;
((calculator#enter 5.)#sub#enter 3.5)#equals;;
[%%expect{|
- : float = 1.5
|}];;
(calculator#enter 5.)#add#add#equals;;
[%%expect{|
- : float = 15.
|}];;
