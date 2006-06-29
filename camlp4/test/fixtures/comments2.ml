(** The first special comment of the file is the comment associated
    to the whole module.*)

(** The comment for function f *)
let f x y = x + y

(** This comment is not attached to any element since there is another
    special comment just before the next element. *)

(** Comment for exception My_exception, even with a simple comment
    between the special comment and the exception.*)
(* A simple comment. *)
exception My_exception of (int -> int) * int

(** Comment for type weather  *)
type weather =
  (** The comment for constructor Rain *)
| Rain of int
  (** The comment for constructor Sun *)
| Sun

(** The comment for type my_record *)
type my_record = {
    (** Comment for field foo *)
    foo : int ;
    (** Comment for field bar *)
    bar : string ;
  }

(** The comment for class my_class *)
class my_class =
    object
      (** A comment to describe inheritance from cl *)
      inherit cl

      (** The comment for the instance variable tutu *)
      val mutable tutu = "tutu"
      (** The comment for toto *)
      val toto = 1
      val titi = "titi"
      (** Comment for method toto *)
      method toto = tutu ^ "!"
      (** Comment for method m *)
      method m (f : float) = 1
    end

(** The comment for class type my_class_type *)
class type my_class_type =
  object
    (** The comment for the instance variable x. *)
    val mutable x : int
    (** The commend for method m. *)
    method m : int -> int
  end

(** The comment for module Foo *)
module Foo =
  struct
    (** The comment for x *)
    let x = 42
    (** A special comment in the class, but not associated to any element. *)
  end

(** The comment for module type my_module_type. *)
module type MY_MODULE_TYPE =
  sig
    (* Comment for value x. *)
    val x : int
    (* ... *)
  end
