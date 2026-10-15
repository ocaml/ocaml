let get
    (t : (Complex.t, Bigarray.complex32_elt, Bigarray.fortran_layout)
         Bigarray.Array1.t) =
  Bigarray.Array1.unsafe_get t 1
