(* TEST
   expect;
*)

let id x = x

[%%expect {|
val id : 'a -> 'a = <fun>
|}]

external identity = id

[%%expect {|
Line 11, characters 20-22:
11 | external identity = id
                         ^^
Error: This identifier should be a primitive, but it is bound to
       a regular value.
|}]

class c =
  object
    val id = id

    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

[%%expect {|
Line 27, characters 28-30:
27 |         external identity = id
                                 ^^
Error: This identifier should be a primitive, but it is bound to
       an instance variable.
|}]

class c =
  object (id)
    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

[%%expect {|
Line 44, characters 28-30:
44 |         external identity = id
                                 ^^
Error: This identifier should be a primitive, but it is bound to
       the self object.
|}]

class s = object end

class c =
  object
    inherit s as id

    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

[%%expect{|
class s : object  end
Line 65, characters 28-30:
65 |         external identity = id
                                 ^^
Error: This identifier should be a primitive, but it is bound to
       an ancestor object.
|}]
