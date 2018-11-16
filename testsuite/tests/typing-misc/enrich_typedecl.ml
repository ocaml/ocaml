(* TEST
   * expect
*)

module rec A : sig
  type t = int * string
end = struct
  type t = A | B

  let f (x : t) =
    match x with
    | A -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-97:
  ......struct
    type t = A | B

    let f (x : t) =
      match x with
      | A -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A.t = A | B val f : t -> unit end
       is not included in
         sig type t = int * string end
       Type declarations do not match:
         type t = A.t = A | B
       is not included in
         type t = int * string
|}]

module rec B : sig
  type 'a t = 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a B.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = 'a end
       Type declarations do not match:
         type 'a t = 'a B.t = A of 'a | B
       is not included in
         type 'a t = 'a
|}];;

module rec C : sig
  type 'a t = { x : 'a }
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a C.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = { x : 'a; } end
       Type declarations do not match:
         type 'a t = 'a C.t = A of 'a | B
       is not included in
         type 'a t = { x : 'a; }
       Their kinds differ.
|}];;


module rec D : sig
  type 'a t = int
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a D.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = int end
       Type declarations do not match:
         type 'a t = 'a D.t = A of 'a | B
       is not included in
         type 'a t = int
|}];;

module rec E : sig
  type 'a t = [> `Foo ] as 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E.t = A of 'a | B
       is not included in
         type 'a t = 'a constraint 'a = [> `Foo ]
|}];;

module rec E2 : sig
  type 'a t = [ `Foo ]
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E2.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = [ `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E2.t = A of 'a | B
       is not included in
         type 'a t = [ `Foo ]
|}];;

module rec E3 : sig
  type 'a t = [< `Foo ] as 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Line _, characters 6-110:
  ......struct
    type 'a t = A of 'a | B

    let f (x : _ t) =
      match x with
      | A _ -> ()
      | B -> ()
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E3.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = 'a constraint 'a = [< `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E3.t = A of 'a | B
       is not included in
         type 'a t = 'a constraint 'a = [< `Foo ]
|}];;


module rec F : sig
  type ('a, 'b) t = Foo of 'a
end = struct
  type ('a, 'b) t = Foo of 'b

  (* this function typechecks properly, which means that we've added the
     manisfest. *)
  let coerce : 'a 'b. ('a, 'b) t -> ('a, 'b) F.t = fun x -> x
end;;
[%%expect{|
Line _, characters 6-201:
  ......struct
    type ('a, 'b) t = Foo of 'b

    (* this function typechecks properly, which means that we've added the
       manisfest. *)
    let coerce : 'a 'b. ('a, 'b) t -> ('a, 'b) F.t = fun x -> x
  end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a, 'b) t = ('a, 'b) F.t = Foo of 'b
           val coerce : ('a, 'b) t -> ('a, 'b) F.t
         end
       is not included in
         sig type ('a, 'b) t = Foo of 'a end
       Type declarations do not match:
         type ('a, 'b) t = ('a, 'b) F.t = Foo of 'b
       is not included in
         type ('a, 'b) t = Foo of 'a
       The types for field Foo are not equal.
|}];;
