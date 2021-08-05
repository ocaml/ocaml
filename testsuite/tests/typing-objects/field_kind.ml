(* TEST
   * expect
*)

type _ t = Int : int t;;
[%%expect{|
type _ t = Int : int t
|}]

let o =
  object (self)
    method private x = 3
    method m : type a. a t -> a = fun Int -> (self#x : int)
  end;;
[%%expect{|
val o : < m : 'a. 'a t -> 'a > = <obj>
|}]

let o' =
  object (self : 's)
    method private x = 3
    method m : type a. a t -> 's -> a = fun Int other -> (other#x : int)
  end;;

let aargh = assert (o'#m Int o' = 3);;
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : type a. a t -> 's -> a = fun Int other -> (other#x : int)
5 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made public implicitly:
 x.
val o' : < m : 'a. 'a t -> 'b -> 'a; x : int > as 'b = <obj>
val aargh : unit = ()
|}]

let o2 =
  object (self : 's)
    method private x = 3
    method m : 's -> int = fun other -> (other#x : int)
  end;;
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : 's -> int = fun other -> (other#x : int)
5 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made public implicitly:
 x.
val o2 : < m : 'a -> int; x : int > as 'a = <obj>
|}]

let o3 =
  object (self : 's)
    method private x = 3
    method m : 's -> int = fun other ->
      let module M = struct let other = other end in (M.other#x : int)
  end;;

let aargh = assert (o3#m o3 = 3);;
[%%expect{|
Lines 2-6, characters 2-5:
2 | ..object (self : 's)
3 |     method private x = 3
4 |     method m : 's -> int = fun other ->
5 |       let module M = struct let other = other end in (M.other#x : int)
6 |   end..
Warning 15 [implicit-public-methods]: the following private methods were made public implicitly:
 x.
val o3 : < m : 'a -> int; x : int > as 'a = <obj>
val aargh : unit = ()
|}]
