(* TEST
   * expect
*)

class type foo_t =
  object
    method foo: string
  end

module M: sig
  class type ct = object val m: string end
end = struct
  class type ct = object end
end

[%%expect{|
class type foo_t = object method foo : string end
Lines 8-10, characters 6-3:
 8 | ......struct
 9 |   class type ct = object end
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig class type ct = object  end end
       is not included in
         sig class type ct = object val m : string end end
       Class type declarations do not match:
         class type ct = object  end
       does not match
         class type ct = object val m : string end
       The first class type has no instance variable m
|}]

module M: sig
  class c : object
    method a: string
  end
end = struct
  class virtual c = object
    method virtual a: string
  end
end
;;
[%%expect{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   class virtual c = object
7 |     method virtual a: string
8 |   end
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig class virtual c : object method virtual a : string end end
       is not included in
         sig class c : object method a : string end end
       Class declarations do not match:
         class virtual c : object method virtual a : string end
       does not match
         class c : object method a : string end
       A class cannot be changed from virtual to concrete
|}]

class type ['a] ct = object val x: 'a end

module M: sig
  class type ['a] c = object end
end = struct
  class type c = object end
end
;;

[%%expect{|
class type ['a] ct = object val x : 'a end
Lines 5-7, characters 6-3:
5 | ......struct
6 |   class type c = object end
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig class type c = object  end end
       is not included in
         sig class type ['a] c = object  end end
       Class type declarations do not match:
         class type c = object  end
       does not match
         class type ['a] c = object  end
       The classes do not have the same number of type parameters
|}]

module M: sig
  class ['a] c: object constraint 'a = int end
end = struct
  class ['a] c = object end
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   class ['a] c = object end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig class ['a] c : object  end end
       is not included in
         sig class ['a] c : object constraint 'a = int end end
       Class declarations do not match:
         class ['a] c : object  end
       does not match
         class ['a] c : object constraint 'a = int end
       A type parameter has type 'a but is expected to have type int
|}]

module M: sig
  class c : int -> object end
end = struct
  class c (x : float) = object end
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   class c (x : float) = object end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig class c : float -> object  end end
       is not included in
         sig class c : int -> object  end end
       Class declarations do not match:
         class c : float -> object  end
       does not match
         class c : int -> object  end
       A parameter has type float but is expected to have type int
|}]

class virtual foo: foo_t =
    object
        method foo = "foo"
        method private virtual cast: int
    end
;;

[%%expect{|
Lines 2-5, characters 4-7:
2 | ....object
3 |         method foo = "foo"
4 |         method private virtual cast: int
5 |     end
Error: The class type
         object method private virtual cast : int method foo : string end
       is not matched by the class type foo_t
       The virtual method cast cannot be hidden
|}]

class type foo_t2 =
    object
        method private foo: string
    end

class foo: foo_t2 =
    object
        method foo = "foo"
    end
;;
[%%expect{|
class type foo_t2 = object method private foo : string end
Lines 7-9, characters 4-7:
7 | ....object
8 |         method foo = "foo"
9 |     end
Error: The class type object method foo : string end
       is not matched by the class type foo_t2
       The public method foo cannot become private
|}]

class virtual foo: foo_t =
    object
        method virtual foo: string
    end
;;
[%%expect{|
Lines 2-4, characters 4-7:
2 | ....object
3 |         method virtual foo: string
4 |     end
Error: The class type object method virtual foo : string end
       is not matched by the class type foo_t
       The virtual method foo cannot become concrete
|}]

class type foo_t3 =
    object
        val mutable x : int
    end

class foo: foo_t3 =
    object
        val x = 1
    end
;;
[%%expect{|
class type foo_t3 = object val mutable x : int end
Lines 7-9, characters 4-7:
7 | ....object
8 |         val x = 1
9 |     end
Error: The class type object val x : int end is not matched by the class type
         foo_t3
       The non-mutable instance variable x cannot become mutable
|}]

class type foo_t4 =
    object
        val x : int
    end

class virtual foo: foo_t4 =
    object
        val virtual x : int
    end
;;
[%%expect{|
class type foo_t4 = object val x : int end
Lines 7-9, characters 4-7:
7 | ....object
8 |         val virtual x : int
9 |     end
Error: The class type object val virtual x : int end
       is not matched by the class type foo_t4
       The virtual instance variable x cannot become concrete
|}]

module M: sig
  class type c = object method m: string end
end = struct
  class type c = object method private m: string end
end
;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   class type c = object method private m: string end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig class type c = object method private m : string end end
       is not included in
         sig class type c = object method m : string end end
       Class type declarations do not match:
         class type c = object method private m : string end
       does not match
         class type c = object method m : string end
       The private method m cannot become public
|}]
