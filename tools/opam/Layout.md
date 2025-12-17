# OCaml opam packaging

Users in opam primarily see the OCaml compiler through the `ocaml` package, for
example installing the [ocaml.5.4.0](https://ocaml.org/p/ocaml/5.4.0) package in
a switch succeeds if and only if OCaml 5.4.0 (or some variant of it) is
available in the opam switch.

There are various additional packages which assist in the process:
- `ocaml-compiler` contains the actual build instructions and is responsible for
  configuring and building a compiler from sources.
- `ocaml-system` is available if the user has installed OCaml outside of opam
  and made it available in PATH. For example, if `/usr/bin/ocaml` is an
  installation of OCaml 5.4.0, then `ocaml-system.5.4.0` can be installed.
- `arch-*` and `system-*` are meta-packages used to control exactly which
  architecture and C compiler ecosystem are targetted by the compiler (at
  present these are only used for Windows)
- `ocaml-option-*` are meta-packages used to control features of the compiler,
  for example whether frame pointers are enabled ([ocaml-option-fp](https://ocaml.org/p/ocaml-option-fp/latest))
  or the flambda optimizer ([ocaml-option-flambda](https://ocaml.org/p/ocaml-option-flambda/latest)).
- `ocaml-options-vanilla` and `ocaml-options-only-*` are additional
  meta-packages used to aid creation of switches with specific options.
  `ocaml-options-vanilla` is the "default" build of OCaml with no additional
  options set. `ocaml-options-only-flambda`, for example, creates a switch with
  the flambda optimizer enabled, but which prohibits the reconfiguration of the
  compiler with different options.
- `ocaml-base-compiler` is a meta-package which builds OCaml sources and depends
  on `ocaml-options-vanilla` - i.e. it installs an altered OCaml compiler.
- `ocaml-variants` was used extensively prior to OCaml 4.12 when the
  ocaml-option- packages were introduced. It is also used for experimental
  branches of OCaml (e.g. MetaOCaml, Multicore OCaml, OxCaml, etc.)
- `host-arch-*` and `host-system-*` are work-in-progress meta-packages which
  _indicate_ which architecture and system the compiler targets. Their purpose
  is to allow other packages in opam-repository to indicate a requirement or a
  conflict with a given architecture or system.
- `base-*` are similarly applied for some compiler features. For example,
  [base-nnp](https://ocaml.org/p/base-nnp/latest) is installed with any compiler
  which does not permit naked pointers in the heap (i.e. OCaml 5.x or 4.x when
  [ocaml-option-nnp](https://ocaml.org/p/ocaml-option-nnp/latest) has been
  installed).
- `ocaml-beta` is a single package used to prevent accidental installation of
  pre-release compilers when using opam 2.0.
- `ocaml-config` packaged the tools/opam/gen\_ocaml\_config.ml script, but is no
  longer required.
- `flexdll` and `winpthreads` are source-installing packages which provide the
  source code, when required, for the flexdll and winpthreads submodules when
  compiling OCaml on Windows.

## Scripts

### `gen_ocaml_config.ml`

### `gen_ocaml-system_config.ml.in`

### `generate.ml` and `process.sh`
