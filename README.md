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

### Active variants

* **4.12+domains+effects (default)** -- A variant based on 4.12 that performs stop-the-world parallel minor collection. It makes available domains and the syntax extensions for using effects. The branch is
 [`4.12+domains+effects`](https://github.com/ocaml-multicore/ocaml-multicore/tree/4.12+domains%2Beffects).
* **4.12+domains** -- This branch has domains but does not have the syntax
  extension for effects (it retains the compiler and runtime system changes needed for effects).
  The branch is
  [`4.12+domains`](https://github.com/ocaml-multicore/ocaml-multicore/tree/4.12+domains).
  This branch is useful to install ppx libraries.

### Historical variants

 * **Parallel Minor GC (4.10)** -- A 4.10 branch using a stop-the-world parallel minor collection. The branch is [`parallel_minor_gc`](https://github.com/ocaml-multicore/ocaml-multicore/tree/parallel_minor_gc).
 * **Parallel Minor GC + No effect syntax (4.10)** -- A 4.10 branch using a stop-the-world parallel minor collection but without the syntax extension for effects. The branch is [`no-effect-syntax`](https://github.com/ocaml-multicore/ocaml-multicore/tree/no-effect-syntax).
 * **Parallel Minor GC (4.06)** -- A 4.06 branch using a stop-the-world parallel minor collection. The branch is [`parallel_minor_gc_4_06`](https://github.com/ocaml-multicore/ocaml-multicore/tree/parallel_minor_gc_4_06).
 * **Concurrent Minor GC (4.06)** -- A 4.06 branch that collects each domain's minor heap concurrently. The branch is [`master`](https://github.com/ocaml-multicore/ocaml-multicore/tree/master).

## Installation

If you want to try out Multicore OCaml, the easiest way is to install the compiler using [OPAM](https://github.com/ocaml-multicore/multicore-opam#install-multicore-ocaml).

Parallel programming examples are available in the [domainslib](https://github.com/ocaml-multicore/domainslib/tree/master/test) library.

Effect handler (concurrent programming) examples are available in [effects-examples](https://github.com/kayceesrk/effects-examples) repo.

## Notes

The original README.adoc file has been moved to
[README.stock.adoc](README.stock.adoc).
