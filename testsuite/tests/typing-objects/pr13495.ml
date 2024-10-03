(* TEST
 expect;
*)

class type gui =
  object
    method sub: 'b. 'b -> 'b
  end

class virtual local_sub =
  object
    method virtual sub: 'b. 'b -> 'b option
  end
[%%expect{|
class type gui = object method sub : 'b -> 'b end
class virtual local_sub : object method virtual sub : 'b -> 'b option end
|}]

class virtual ['a] compound_gui =
  object (_: #gui)
    constraint 'a = #local_sub
  end
[%%expect{|
class virtual ['a] compound_gui :
  object constraint 'a = #local_sub method virtual sub : 'b -> 'b end
|}]
