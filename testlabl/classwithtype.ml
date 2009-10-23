type (-'a,+'b) type_path

module rec Typed_controls : sig
  class type ['a] control = object
    method sub: 'b. ('a, 'b) type_path -> 'b Typed_controls.sub_control
  end
  type 'a sub_control = SubControl of 'a control
end = struct
  class type ['a] control = ['a] Typed_controls.control

  type 'a sub_control = SubControl of 'a control
end

module rec Typed_controls : sig
  class type ['a] control = object
    method sub: 'b. ('a, 'b) type_path -> ('b,int) Typed_controls.sub_control
  end
  type ('a,'b) sub_control = SubControl of 'a control constraint 'b = int
end = struct
  class type ['a] control = ['a] Typed_controls.control

  type ('a,'b) sub_control = SubControl of 'a control constraint 'b = int
end


class type ['a] control = object
  method sub: 'b. ('a, 'b) type_path -> 'b sub_control
end
with type 'a sub_control = SubControl of 'a control

module rec A : sig
  type +'a t = 'a A.u
  type +'a u = 'a A.v
  type +'a v = 'a
end = struct
  type 'a t = 'a A.u
  type 'a u = 'a A.v
  type 'a v = 'a
end
