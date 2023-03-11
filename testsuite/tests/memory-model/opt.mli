(* Configuration:
 * the test can also be used as a program, with command line options.
 * In particular, option `-v` also runs allowed tests,
 * whose occurrence is legal according to the memory model
 * By contrast, option `-q` (default) does not runs
 * forbidden tests only, whose occurrence are not legal
 * according to the memory model.
*)

module type Config = sig
  val verbose : bool
  val size : int
  val nruns : int
  val navail : int
end

module Make :
functor (N:sig val pgm:string end) -> sig module Config : Config end
