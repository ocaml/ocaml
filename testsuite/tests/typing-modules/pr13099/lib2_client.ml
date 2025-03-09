let f (_ : Lib.t) = ()

(* The naming of this module is important: When the error reporting
   is running in a mode where it can load new cmis from disk, this
   module leads the compiler to try to load a cmi file [lib1_client.cmi].
   That's because the compiler tries to be smart about double-underscore
   paths, rewriting [Foo__Bar] to [Foo.Bar] when these names are aliases.
 *)
module Lib1_client__X = struct
  type t = A
end

module F (T : sig type t end) = struct
  type t = Lib1_client__X.t

  let f (_ : T.t) = ()
end

module _ = F (struct type t = T end)
