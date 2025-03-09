let _ = B.g Baz

(* Type-directed disambiguation: Baz is defined in A, and even when a.cmi is
   provided with -H this still typechecks.  It's not obvious that this is
   necessary (rejecting this program also seems fine, in that case), but this
   test is here to record the current behavior so any change will be
   intentional. *)
