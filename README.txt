Namespaces
==========

Author: Fabrice LE FESSANT (INRIA/OCamlPro)
Date: 2011/03/07

Status: beta, 1st attempt
  "ocamldep" and "ocaml" not yet updated to work with namespaces

The goal of this branch is to provide namespaces in Objective-Caml.
Namespaces allow:
- to link several modules with the same name, as soon as they are in
different namespaces
- to map the directory structure into module paths

They should be an improvement other the use of the -pack option, that
would probably be obsoleted afterwards.

In this version, modules and namespaces share the same syntax.  "open
X" and "X.x" are OK when X is either a module or a namespace.
However, "module Y = X" and "Make(X)" are not allowed if X is a
namespace (as its signature depends on the content of the file
system).

================================================================

This version:

Compiler options:
-----------------
- adds an option "-namespace X.Y", to activate namespaces and to tell
the compiler that the current module should be compiled as part of
namespace X.Y. The special case "-namespace -" can be used to tell the
compiler to use the relative path of the file (i.e.
"ocamlc-namespace - x/y/z.ml" is equivalent to
 "ocamlc -namespace X.Y x/y/z.ml"). The namespace "X.Y" is then opened, so
that modules from that namespace can be refered to without the X.Y prefix.

- adds an option "-open-namespace X.Z" to tell the compiler to pre-open the
namespace X.Z (i.e. modules in the directory x/z/). Is it really useful ?

- adds an option "-debug ns" to get verbose debug message for namespaces.

Language extensions:
--------------------

A .ml/.mli file can start by "in X.Y" to declare that it is part of namespace "X.Y".
It is equivalent to using the compiler option "-namespace X.Y". If the option is
also provided, the compiler will check that both notations are consistent.

Module aliasing: the new construct "open X.Y as N" allows to alias
both modules and namespaces. It is particularly useful as, contrarily
to "module N = X.Y", (1) it is allowed for namespaces (2) it does not
define a new module N, but instead alias the module with N in its
environment (i.e. N will not appear in the .cmi).

Features
--------

Namespaces and the -I option cannot be used one instead of the other
one. Indeed, if you compile a module Z in namespace X.Y, trying to
access it as Z using "-I x/y" will generate an error as x/y/z.cmi
defines X.Y.Z and not Z. The opposite is also true: trying to use
X.Y.Z to access a module compiled as Z in directory x/y will generate
an error. It also means that, until everybody start using namespaces,
libraries should be compiled twice, once with namespaces (the new way)
and once without namespaces (the old way), and installed in different
trees. Trying to mix both approaches could work, but would be
dangerous (the same module could be included in file, once without
a namespace and once with a namespace).

==================================================================

