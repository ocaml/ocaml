(* Exemple d'Alain *)
type ('a,'b) type_path = 'a -> 'b

class type ['a] control = object
  method sub: 'b. ('a, 'b) type_path -> 'b sub_control
end
with type 'a sub_control = SubControl of 'a control

(* Cannot create a meaningful class, lacking polymorphic recursion *)
class ['a] acontrol = object (_ : 'a #control)
  method sub p = SubControl (fun  ???)
end
with type 'a sub_control = SubControl of 'a control

(* Using immediate objects works *)
let rec new_control : 'a. 'a -> 'a control = fun (type t) x ->
  object (_ : t control) (* beware, 'a is local to the annotation! *)
    method sub f = SubControl (new_control (f x))
  end

(* With recursive module we can build a real class *)
module rec Typed_controls : sig
  type 'a sub_control = SubControl of 'a Typed_controls.control
  class ['a] control : 'a -> object
    method sub: 'b. ('a, 'b) type_path -> 'b sub_control
  end
end = struct
  type 'a sub_control = SubControl of 'a Typed_controls.control
  class ['a] control x = object
    method sub : 'b. ('a -> 'b) -> 'b sub_control =
      fun f -> SubControl (new Typed_controls.control (f x))
  end
end

(* Building only a class type is a bit verbose *)
module rec Typed_controls : sig
  class type ['a] control = object
    method sub: 'b. ('a, 'b) type_path -> ('b,int) Typed_controls.sub_control
  end
  type ('a,'b) sub_control = SubControl of 'a control constraint 'b = int
end = struct
  class type ['a] control = ['a] Typed_controls.control
  type ('a,'b) sub_control = SubControl of 'a control constraint 'b = int
end

(* Oops, recursive module didn't propagate variance correctly.
   Work now with explicit variances. *)
module rec A : sig
  type +'a t = 'a A.u
  type +'a u = 'a A.v
  type +'a v = 'a
end = struct
  type 'a t = 'a A.u
  type 'a u = 'a A.v
  type 'a v = 'a
end

(* Another example with lists *)
class type ['a] olist = object ('s)
  method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
  method map : 'b. ('a -> 'b) -> 'b wlist
  method map_self : ('a -> 'a) -> 's
end
with type 'a wlist = Wlist of 'a olist

let rec make_olist : 'a. 'a list -> 'a olist = fun (type t) l ->
  object (_ : t #olist)
    val l = l
    method fold f = List.fold_right f l
    method map f = Wlist (make_olist (List.map f l))
    method map_self f = {< l = List.map f l >}
  end

(* The following definition would require polymorphic recusion *)
class ['a] olist l = object (_ : 's)
  val l : 'a list = l
  method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b =
    fun f -> List.fold_right f l
  method map : 'b. ('a -> 'b) -> 'b wlist =
    fun f -> Wlist (List.map f l)
  method map_self f =
    {< l = List.map f l >}
end
with type 'a wlist = Wlist of 'a olist

(* Recursive modules are fine too *)
module rec Olist : sig
  type 'a wrap = W of 'a Olist.c
  class ['a] c : 'a list -> object ('s)
    method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
    method map : 'b. ('a -> 'b) -> 'b wrap
    method map_self : ('a -> 'a) -> 's
  end
end = struct
  type 'a wrap = W of 'a Olist.c
  class ['a] c l = object (_ : 'a #Olist.c)
    val l = l
    method fold f = List.fold_right f l
    method map f = W (new Olist.c (List.map f l))
    method map_self f = {< l = List.map f l >}
  end
end

class ['a] olist l = object
  inherit ['a] Olist.c l
  method contents = l
end
  
let l = new olist [1;2;3]
let Olist.W l' = l#map float
let l'' = l#map_self succ
