module type S = Test_functor.S with type elt := unit

module M : S = Test_functor.Apply (String)
