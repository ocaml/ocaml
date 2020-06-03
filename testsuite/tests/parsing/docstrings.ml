(* TEST
   * expect
   flags += " -dsource "
*)

(***********************************************************************)
(* Test based on the example in the ocamldoc manual
   Obviously some parts are different due to the simplified
   rules used by the compiler *)

module Manual : sig

  (** Special comments can be placed between elements and are kept
      by the OCamldoc tool, but are not associated to any element.
      @-tags in these comments are ignored.*)

  (*******************************************************************)
  (** Comments like the one above, with more than two asterisks,
      are ignored. *)

  (** The comment for function f. *)
  val f : int -> int -> int
  (** The continuation of the comment for function f. *)

  (** Comment for exception My_exception, even with a simple comment
      between the special comment and the exception.*)
  (* Hello, I'm a simple comment :-) *)
  exception My_exception of (int -> int) * int

  (** Comment for type weather  *)
  type weather =
    | Rain of int (** The comment for constructor Rain *)
    | Sun (** The comment for constructor Sun *)

  (** Comment for type weather2  *)
  type weather2 =
    | Rain of int (** The comment for constructor Rain *)
    | Sun (** The comment for constructor Sun *)
  (** I can continue the comment for type weather2 here
      because there is already a comment associated to the last constructor.*)

  (** The comment for type my_record *)
  type my_record = {
    foo : int ;    (** Comment for field foo *)
    bar : string ; (** Comment for field bar *)
  }
  (** Continuation of comment for type my_record *)

  (** Comment for foo *)
  val foo : string
  (** This comment is ambiguous and associated to both foo and bar. *)
  val bar : string
  (** This comment is associated to bar. *)

  (** The comment for class my_class *)
  class my_class : object
    (** A comment to describe inheritance from cl *)
    inherit cl

    (** The comment for attribute tutu *)
    val mutable tutu : string

    (** The comment for attribute toto. *)
    val toto : int

    (** This comment is not attached to titi since
        there is a blank line before titi, but is kept
        as a comment in the class. *)

    val titi : string

    (** Comment for method toto *)
    method toto : string

    (** Comment for method m *)
    method m : float -> int
  end

  (** The comment for the class type my_class_type *)
  class type my_class_type = object
    (** This is a docstring that OCaml <= 4.07.1 drops.
        For some reason, when a class type begins with two docstrings,
        it keeps only the second one.
        This is fixed by GPR#2151. *)

    (** The comment for variable x. *)
    val mutable x : int

    (** The comment for method m. *)
    method m : int -> int

    (** This is a docstring that OCaml <= 4.07.1 misplaces.
        For some reason, when a class type ends with two docstrings,
        it keeps both of them, but exchanges their order.
        This is again fixed by GPR#2151. *)

    (** Another docstring that OCaml <= 4.07.1 misplaces. *)

  end

  (** The comment for module Foo *)
  module Foo : sig
    (** The comment for x *)
    val x : int

    (** A special comment that is kept but not associated to any element *)
  end

  (** The comment for module type my_module_type. *)
  module type my_module_type = sig
    (** The comment for value x. *)
    val x : int

    (** The comment for module M. *)
    module M : sig
      (** The comment for value y. *)
      val y : int

      (* ... *)
    end

  end

end = struct

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
    | Rain of int (** The comment for constructor Rain *)
    | Sun (** The comment for constructor Sun *)

  (** The comment for type my_record *)
  type my_record = {
    foo : int ;    (** Comment for field foo *)
    bar : string ; (** Comment for field bar *)
  }

  (** The comment for class my_class *)
  class my_class = object
    (** A comment to describe inheritance from cl *)
    inherit cl

    (** The comment for the instance variable tutu *)
    val mutable tutu = "tutu"

    (** The comment for toto *)
    val toto = 1
    val titi = "titi"
    (** Ambiguous comment on both titi and toto *)
    method toto = tutu ^ "!"

    (** floating 1 *)

    (** floating 2 *)

    (** The comment for method m *)
    method m (f : float) = 1
  end

  (** The comment for class type my_class_type *)
  class type my_class_type = object
    (** The comment for the instance variable x. *)
    val mutable x : int

    (** floating 1 *)

    (** floating 2 *)

    (** The comment for method m. *)
    method m : int -> int
  end

  (** The comment for module Foo *)
  module Foo = struct
    (** The comment for x *)
    val x : int
    (** Another comment for x *)
  end

  (** The comment for module type my_module_type. *)
  module type my_module_type = sig
    (* Comment for value x. *)
    val x : int
    (* ... *)
  end

end;;
[%%expect {|

module Manual :
  sig
    [@@@ocaml.text
      " Special comments can be placed between elements and are kept\n      by the OCamldoc tool, but are not associated to any element.\n      @-tags in these comments are ignored."]
    [@@@ocaml.text
      " Comments like the one above, with more than two asterisks,\n      are ignored. "]
    val f : int -> int -> int[@@ocaml.doc " The comment for function f. "]
    [@@ocaml.doc " The continuation of the comment for function f. "]
    exception My_exception of (int -> int) * int
      [@ocaml.doc
        " Comment for exception My_exception, even with a simple comment\n      between the special comment and the exception."]
    type weather =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather  "]
    type weather2 =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather2  "]
    [@@ocaml.doc
      " I can continue the comment for type weather2 here\n      because there is already a comment associated to the last constructor."]
    type my_record =
      {
      foo: int [@ocaml.doc " Comment for field foo "];
      bar: string [@ocaml.doc " Comment for field bar "]}[@@ocaml.doc
                                                           " The comment for type my_record "]
    [@@ocaml.doc " Continuation of comment for type my_record "]
    val foo : string[@@ocaml.doc " Comment for foo "][@@ocaml.doc
                                                       " This comment is ambiguous and associated to both foo and bar. "]
    val bar : string[@@ocaml.doc
                      " This comment is ambiguous and associated to both foo and bar. "]
    [@@ocaml.doc " This comment is associated to bar. "]
    class my_class :
      object
        inherit cl[@@ocaml.doc " A comment to describe inheritance from cl "]
        val  mutable tutu : string[@@ocaml.doc
                                    " The comment for attribute tutu "]
        val  toto : int[@@ocaml.doc " The comment for attribute toto. "]
        [@@@ocaml.text
          " This comment is not attached to titi since\n        there is a blank line before titi, but is kept\n        as a comment in the class. "]
        val  titi : string
        method  toto : string[@@ocaml.doc " Comment for method toto "]
        method  m : float -> int[@@ocaml.doc " Comment for method m "]
      end[@@ocaml.doc " The comment for class my_class "]
    class type my_class_type =
      object
        [@@@ocaml.text
          " This is a docstring that OCaml <= 4.07.1 drops.\n        For some reason, when a class type begins with two docstrings,\n        it keeps only the second one.\n        This is fixed by GPR#2151. "]
        val  mutable x : int[@@ocaml.doc " The comment for variable x. "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
        [@@@ocaml.text
          " This is a docstring that OCaml <= 4.07.1 misplaces.\n        For some reason, when a class type ends with two docstrings,\n        it keeps both of them, but exchanges their order.\n        This is again fixed by GPR#2151. "]
        [@@@ocaml.text " Another docstring that OCaml <= 4.07.1 misplaces. "]
      end[@@ocaml.doc " The comment for the class type my_class_type "]
    module Foo :
    sig
      val x : int[@@ocaml.doc " The comment for x "]
      [@@@ocaml.text
        " A special comment that is kept but not associated to any element "]
    end[@@ocaml.doc " The comment for module Foo "]
    module type my_module_type  =
      sig
        val x : int[@@ocaml.doc " The comment for value x. "]
        module M :
        sig val y : int[@@ocaml.doc " The comment for value y. "] end
        [@@ocaml.doc " The comment for module M. "]
      end[@@ocaml.doc " The comment for module type my_module_type. "]
  end =
  struct
    let f x y = x + y[@@ocaml.doc " The comment for function f "]
    [@@@ocaml.text
      " This comment is not attached to any element since there is another\n      special comment just before the next element. "]
    exception My_exception of (int -> int) * int
      [@ocaml.doc
        " Comment for exception My_exception, even with a simple comment\n      between the special comment and the exception."]
    type weather =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather  "]
    type my_record =
      {
      foo: int [@ocaml.doc " Comment for field foo "];
      bar: string [@ocaml.doc " Comment for field bar "]}[@@ocaml.doc
                                                           " The comment for type my_record "]
    class my_class =
      object
        inherit  cl[@@ocaml.doc
                     " A comment to describe inheritance from cl "]
        val mutable tutu = "tutu"[@@ocaml.doc
                                   " The comment for the instance variable tutu "]
        val toto = 1[@@ocaml.doc " The comment for toto "]
        val titi = "titi"[@@ocaml.doc
                           " Ambiguous comment on both titi and toto "]
        method toto = tutu ^ "!"[@@ocaml.doc
                                  " Ambiguous comment on both titi and toto "]
        [@@@ocaml.text " floating 1 "]
        [@@@ocaml.text " floating 2 "]
        method m (f : float) = 1[@@ocaml.doc " The comment for method m "]
      end[@@ocaml.doc " The comment for class my_class "]
    class type my_class_type =
      object
        val  mutable x : int[@@ocaml.doc
                              " The comment for the instance variable x. "]
        [@@@ocaml.text " floating 1 "]
        [@@@ocaml.text " floating 2 "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
      end[@@ocaml.doc " The comment for class type my_class_type "]
    module Foo =
      struct
        external x : int[@@ocaml.doc " The comment for x "][@@ocaml.doc
                                                             " Another comment for x "]
      end[@@ocaml.doc " The comment for module Foo "]
    module type my_module_type  = sig val x : int end[@@ocaml.doc
                                                       " The comment for module type my_module_type. "]
  end ;;
Line 141, characters 12-14:
141 |     inherit cl
                  ^^
Error: Unbound class cl
|}]

(***********************************************************************)
(* Empty doc comments (GPR#548) *)

module M = struct
  type t = Label (**)
  (** attached to t *)

  (**)

  (** Empty docstring comments should not generate attributes *)

  type w (**)
end;;
[%%expect {|

module M =
  struct
    type t =
      | Label [@@ocaml.doc " attached to t "]
    [@@@ocaml.text
      " Empty docstring comments should not generate attributes "]
    type w
  end;;
module M : sig type t = Label type w end
|}]

(***********************************************************************)
(* Comments at the beginning and end of structures (MPR#7701) *)

module M = struct
  (** foo *)
  type t

  type s
  (** bar *)
end;;
[%%expect {|

module M = struct type t[@@ocaml.doc " foo "]
                  type s[@@ocaml.doc " bar "] end;;
module M : sig type t type s end
|}]

module M = struct

  (** foo *)
  type t

  type s
  (** bar *)

end;;
[%%expect {|

module M = struct type t[@@ocaml.doc " foo "]
                  type s[@@ocaml.doc " bar "] end;;
module M : sig type t type s end
|}]

module M = struct
  (** foo *)

  type t

  type s

  (** bar *)
end;;
[%%expect {|

module M =
  struct [@@@ocaml.text " foo "]
         type t
         type s
         [@@@ocaml.text " bar "] end;;
module M : sig type t type s end
|}]

module M = struct

  (** foo *)

  type t

  type s

  (** bar *)

end;;
[%%expect {|

module M =
  struct [@@@ocaml.text " foo "]
         type t
         type s
         [@@@ocaml.text " bar "] end;;
module M : sig type t type s end
|}]

module M = struct

  (** foo1: this comment is unattached *)
  (** foo2 *)
  type t

  type s
  (** bar1 *)
  (** bar2: this comment is unattached *)

end;;
[%%expect {|

module M =
  struct type t[@@ocaml.doc " foo2 "]
         type s[@@ocaml.doc " bar1 "] end;;
module M : sig type t type s end
|}]

module M = struct
  (** foo1 *)

  (** foo2 *)

  type t

  type s

  (** bar1 *)

  (** bar2 *)
end;;
[%%expect {|

module M =
  struct
    [@@@ocaml.text " foo1 "]
    [@@@ocaml.text " foo2 "]
    type t
    type s
    [@@@ocaml.text " bar1 "]
    [@@@ocaml.text " bar2 "]
  end;;
module M : sig type t type s end
|}]

module M = struct

  (** foo1 *)

  (** foo2 *)

  type t

  type s

  (** bar1 *)

  (** bar2 *)

end;;
[%%expect {|

module M =
  struct
    [@@@ocaml.text " foo1 "]
    [@@@ocaml.text " foo2 "]
    type t
    type s
    [@@@ocaml.text " bar1 "]
    [@@@ocaml.text " bar2 "]
  end;;
module M : sig type t type s end
|}]

module M = struct (** foo *) type t (** bar *) end;;
[%%expect {|

module M = struct type t[@@ocaml.doc " foo "][@@ocaml.doc " bar "] end;;
module M : sig type t end
|}]

module M = struct (** foo *)

type t

(** bar *) end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "]
                  type t
                  [@@@ocaml.text " bar "] end;;
module M : sig type t end
|}]

module M = struct (** foo *) end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct (** foo *)

end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct

(** foo *) end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct
(** foo *)
end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct

(** foo *)
end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct
(** foo *)

end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct

(** foo *)

end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "] end;;
module M : sig end
|}]

module M = struct

(** foo *)

(** bar *)

end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "]
                  [@@@ocaml.text " bar "] end;;
module M : sig end
|}]

module M = struct
(** foo *)

(** bar *)
end;;
[%%expect {|

module M = struct [@@@ocaml.text " foo "]
                  [@@@ocaml.text " bar "] end;;
module M : sig end
|}]


(*****************************************************************************)
(* Comments on parameters, variant constructors and object methods (GPR#477) *)

type 'a with_default
  =  ?size:int       (** default [42] *)
  -> ?resizable:bool (** default [true] *)
  -> 'a;;
[%%expect {|

type 'a with_default =
  ?size:((int)[@ocaml.doc " default [42] "]) ->
    ?resizable:((bool)[@ocaml.doc " default [true] "]) -> 'a;;
type 'a with_default = ?size:int -> ?resizable:bool -> 'a
|}]

type obj = <
  meth1 : int -> int;
  (** method 1 *)

  meth2: unit -> float (** method 2 *);
>;;
[%%expect {|

type obj =
  <
    meth1: int -> int [@ocaml.doc " method 1 "] ;meth2: unit -> float
                                                   [@ocaml.doc " method 2 "]
    > ;;
type obj = < meth1 : int -> int; meth2 : unit -> float >
|}]

type var = [
  | `Foo (** foo *)
  | `Bar of int * string (** bar *)
];;
[%%expect {|

type var =
  [ `Foo [@ocaml.doc " foo "] | `Bar of (int * string) [@ocaml.doc " bar "]];;
type var = [ `Bar of int * string | `Foo ]
|}]

module type S = sig

  val before : unit -> unit
  (** docstring before *)
  [@@@foo]

  [@@@foo]
  (** docstring after *)
  val after : unit -> unit

end;;
[%%expect {|

module type S  =
  sig
    val before : unit -> unit[@@ocaml.doc " docstring before "]
    [@@@foo ]
    [@@@foo ]
    val after : unit -> unit[@@ocaml.doc " docstring after "]
  end;;
module type S = sig val before : unit -> unit val after : unit -> unit end
|}]
