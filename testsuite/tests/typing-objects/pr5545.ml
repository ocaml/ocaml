(* TEST
   * expect
*)

type foo = int;;
[%%expect{|
type foo = int
|}]

class o =
  object(this)
    method x : foo = 10
    method y : int = this # x
  end;;
[%%expect{|
class o : object method x : foo method y : int end
|}]


class o =
  object(this)
    method x : foo = 10
    method y = (this # x : int)
  end;;
[%%expect{|
class o : object method x : foo method y : int end
|}]



class o =
  object(this)
    method x : int = (10 : int)
    method y = (this # x : foo)
  end;;
[%%expect{|
class o : object method x : int method y : foo end
|}]
