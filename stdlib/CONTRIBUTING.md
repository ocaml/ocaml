# How to contribute changes

Contributions to the standard library are very welcome.  There is some
widespread belief in the community that the stdlib is somehow "frozen"
and that its evolutions are mostly driven by the need of the OCaml
compiler itself.  Let's be clear: this is just plain wrong. The
compiler is happy with its own local utility functions, and many
recent additions to the stdlib are not used by the compiler.

Another common and wrong idea is that core OCaml maintainers don't
really care about the standard library.  This is not true, and won't
be unless one of the "alternative standard" libraries really gains
enough "market share" in the community.

So: please contribute!

Obviously, proposals made to evolve the standard library will be
evaluated with very high standards, similar to those applied to the
evolution of the surface langage, and much higher than those for
internal compiler changes (optimizations, etc).

A key property of the standard library is its stability.  Backward
compatibility is not an absolute technical requirement (any addition
to/of a module can break existing code, formally), but breakage should
be limited as much as possible (and assessed, when relevant).  A
corollary is that any addition creates a long-term support commitment.
For instance, once a concrete type or function is made public,
changing the exposed definition cannot be done easily.

There is no plan to extend dramatically the functional domain covered
by the standard library.  For instance, proposals to include support
for XML, JSON, or network protocols are very likely to be rejected.  Such
domains are better treated by external libraries.  Small additions to
existing modules are much simpler to get in, even more so (but not
necessarily) when:

  - they cannot easily be implemented externally, or when
  - they facilitate communication between independent external
    libraries, or when
  - they fill obvious gaps.

Of course, standard guidelines apply as well: proper documentation,
proper tests, portability (yes, also Windows!), good justification for
why the change is desirable and why it should go into stdlib.

So: be prepared for some serious review process!  But yes, yes,
contributions are welcome and appreciated.  Promised.

## Naming functions and API

Naming functions or finding a suitable argument order is a
notoriously hard task. In general the name of a function should
be descriptive and already make the usage clear. In case a similar
function already exists in another module of the standard library
one should use the same name and argument order.

If the function takes several arguments with the same type it is okay to
use labels for some of the arguments in order to avoid confusion about
argument order.

A good starting point for function names and API
is checking if these functions are already contained in some of the
popular alternative standard library function such as
[Base](https://github.com/janestreet/base),
[OCaml Batteries](https://github.com/ocaml-batteries-team/batteries-included),
[ExtLib](https://github.com/ygrek/ocaml-extlib) or
[OCaml-containers](https://github.com/c-cube/ocaml-containers).
One can also take a look at the standard libraries from other
programming languages for inspiration regarding names and API.
