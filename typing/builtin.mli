val path_match_failure: Path.t
val path_assert_failure : Path.t
val path_undefined_recursive_module : Path.t

(* To build the initial environment. Since there is a nasty mutual
   recursion between predef and env, we break it by parameterizing
   over Env.t, Env.add_type and Env.add_exception. *)

val build_initial_env:
  (Ident.t -> Types.type_declaration -> 'a -> 'a) ->
  (Ident.t -> Types.exception_declaration -> 'a -> 'a) ->
  'a -> 'a

(* To initialize linker tables *)

val builtin_values: (string * Ident.t) list
