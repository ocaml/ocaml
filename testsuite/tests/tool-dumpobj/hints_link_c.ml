let set
    (t : (int, Bigarray.int16_unsigned_elt, Bigarray.fortran_layout)
         Bigarray.Array1.t) v =
  Bigarray.Array1.unsafe_set t 1 v
