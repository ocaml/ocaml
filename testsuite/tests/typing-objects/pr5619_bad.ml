(* TEST
   * expect
*)

class type foo_t =
  object
    method foo: string
  end

type 'a name =
    Foo: foo_t name
  | Int: int name
;;

[%%expect{|
class type foo_t = object method foo : string end
type 'a name = Foo : foo_t name | Int : int name
|}]

class foo =
  object(self)
    method foo = "foo"
    method cast =
      function
          Foo -> (self :> <foo : string>)
  end
;;
[%%expect{|
class foo :
  object method cast : foo_t name -> < foo : string > method foo : string end
|}]

class foo: foo_t =
  object(self)
    method foo = "foo"
    method cast: type a. a name -> a =
      function
          Foo -> (self :> foo_t)
        | _ -> raise Exit
  end
;;
[%%expect{|
Line _, characters 2-156:
  ..object(self)
      method foo = "foo"
      method cast: type a. a name -> a =
        function
            Foo -> (self :> foo_t)
          | _ -> raise Exit
    end
Error: The class type
         object method cast : 'a name -> 'a method foo : string end
       is not matched by the class type foo_t
       The public method cast cannot be hidden
|}]
