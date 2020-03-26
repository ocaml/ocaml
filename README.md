# Multicore OCaml

A concurrent and shared-memory parallel extension of the OCaml compiler.

Multicore OCaml cleanly separates abstractions for concurrency (overlapped
execution) from parallelism (simultaneous execution). Concurrency is expressed
through [effect handlers](http://kcsrk.info/papers/system_effects_feb_18.pdf)
and parallelism through
[domains](https://github.com/ocaml-multicore/ocaml-multicore/blob/master/stdlib/domain.mli).
Much of the work in supporting shared memory parallelism is the development of a
mostly-concurrent, generational, mark-and-sweep collector that strikes a balance
between single-threaded performance and feature backwards compatibility, and
multicore scalability.

See the
[wiki](https://github.com/ocaml-multicore/ocaml-multicore/wiki) for more
resources.

## Variants

* Parallel Minor GC (default) -- A variant that performs stop-the-world parallel
  minor collection. The branch is `parallel_minor_gc`.
* Concurrent Minor GC -- A variant that garbage collects each domain's minor
  heap concurrently. The branch is `master`.
* Parallel Minor GC + No effect syntax -- This branch does not have syntax
  extension for effects (but includes the compiler and runtime system changes).
  The branch is `no-effect-syntax`. This branch is useful to install ppx
  libraries.

The above variants are kept in sync.

## Notes

The original README.adoc file has been moved to
[README.stock.adoc](README.stock.adoc).
