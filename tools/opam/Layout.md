# OCaml opam packaging

Users in opam primarily see the OCaml compiler through the `ocaml` package. For
example, installing the [ocaml.5.4.0](https://ocaml.org/p/ocaml/5.4.0) package
in a switch succeeds if and only if OCaml 5.4.0 (or some variant of it) is
available in the opam switch.

There are various additional packages which assist in the process:
- `ocaml-compiler` contains the actual build instructions and is responsible for
  configuring and building a compiler from sources.
- `ocaml-system` is available if the user has installed OCaml outside of opam
  and made it available in PATH. For example, if `/usr/bin/ocaml` is an
  installation of OCaml 5.4.0, then `ocaml-system.5.4.0` can be installed.
- `arch-*` and `system-*` are meta-packages used to control exactly which
  architecture and C compiler ecosystem are targetted by the compiler (at
  present these are only used for Windows).
- `ocaml-option-*` are meta-packages used to control features of the compiler,
  for example whether frame pointers are used ([ocaml-option-fp](https://ocaml.org/p/ocaml-option-fp/latest))
  or the flambda optimizer enabled ([ocaml-option-flambda](https://ocaml.org/p/ocaml-option-flambda/latest)).
- `ocaml-options-vanilla` and `ocaml-options-only-*` are additional
  meta-packages used to aid creation of switches with fixed specific options.
  `ocaml-options-vanilla` is the "default" build of OCaml with no additional
  options set. `ocaml-options-only-flambda`, for example, creates a switch with
  the flambda optimizer enabled, but which prohibits the reconfiguration of the
  compiler with different options. The idea behind these packages is that if
  they are used when creating a switch, for example by running
  `opam switch create release-builds ocaml-options-only-flambda ocaml.5.4.0`,
  then packages which _conflict_ `ocaml-option-flambda` for some reason will be
  rejected, rather than causing the compiler to be rebuilt with flambda support
  disabled.
- `ocaml-base-compiler` is a meta-package which builds OCaml sources and depends
  on `ocaml-options-vanilla` - i.e. it installs an altered OCaml compiler.
- `ocaml-variants` was used extensively prior to OCaml 4.12 when the
  ocaml-option- packages were introduced for alternate configurations (for
  example, `ocaml-variants.4.11.0+flambda` provided OCaml 4.11.0 with the
  flambda optimizer enabled). It was also, and still is, used for experimental
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

Packages in opam are able to set variables as part of their build process; the
`ocaml` package defines six:
- `native` (boolean): indicates that `ocamlopt` is available
- `native-tools` (boolean): indicates that the ocamlopt-compiled versions of
  tools are available (`ocamlc.opt`, `ocamlopt.opt`, etc.)
- `native-dynlink` (boolean): indicates that `dynlink.cmxa` is available
- `stubsdir` (string): contains the lines of `ld.conf` presented in a format
  suitable for `CAML_LD_LIBRARY_PATH`
- `preinstalled` (boolean): is true if the `ocaml-system` package provided the
  compiler (i.e. if OCaml is being used from an installation outside of opam)
- `compiler` (string): OPAM 1.x derived value reflecting the name of the switch
  based on the compiler's version. This is largely a legacy value, and is
  essentially either "system" (if the ocaml-system package provided the
  compiler) or the compiler version (e.g. "5.4.0"), otherwise.

This script is used by the `ocaml` package to generate the file `ocaml.config`
when the `ocaml` package is installed.

Historically, this file lived in opam-repository itself, and was installed as a
script by the `ocaml-config` package and then used by each of the `ocaml`
packages. opam-repository no longer stores files directly in its repo, but the
script is still downloaded from this repository by the `ocaml` package.

### `gen_ocaml-system_config.ml`

This script is used by the `ocaml-system` package to generate the file
`ocaml-system.config` when using a compiler which has been installed outside of
opam. It works in a similar way to `gen_ocaml_config.ml`. `ocaml-system` defines
one variable `path` which contains the directory where the compiler's binaries
are located. The script also sets up another mechanism in opam to
ensure that the package is recompiled by opam if the compiler is altered (for
example, by upgrading the compiler through the OS's package manager via
`apt upgrade` or `brew upgrade`). This is done by recording the digest of
the `ocamlc` binary and the `graphics.cmi` file. The latter is a legacy check
from when the Graphics library was part of OCaml, but not necessary installed,
and could be added via a separate package.

Historically, this file lived in opam-repository itself, but opam-repository no
longer stores files directly in its repo. This script is referenced in this
repository by each of the `ocaml-system` packages.

### `generate.ml`

The `install` target of the build system is meta-programmed to allow extraction
of the directories, files and symlinks which are required for a given
installation. This script is called automatically by the build system and takes
the files emitted during:

```
make OPAM_PACKAGE_NAME=ocaml-compiler INSTALL_MODE=opam install
```

and produces `ocaml-compiler.install` and `ocaml-compiler-fixup.sh`. The
.install file can be used by opam instead of running `make install`. At present,
there are two limitations in opam's installation mechanism. It is not possible
to specify symlinks and it is not possible to install documentation files to
arbitrary package locations. These two limitations are dealt with by the
`ocaml-compiler-fixup.sh` script - i.e. `ocaml-compiler.install` provides opam
with almost the entire installation manifest, and then the
`ocaml-compiler-fixup.sh` script completes the missing parts (as it happens, the
`-fixup` script runs first). Future versions of opam will hopefully deal with
these two limitations, at which point the `-fixup` script will become
unnecessary.

`generate.ml` is also used with `INSTALL_MODE=clone` and then produces
`ocaml-compiler-clone.sh`. This script is used by `process.sh` as part of
Relocatable OCaml to allow compilers to be duplicated by opam, instead of built
from sources each time. The script is intended to be run from the prefix
directory and receives as a single argument the directory to which the new
compiler should be written. The script's preference is to use copy-on-write (via
`cp --reflink=always`) but falls back to hard-linking or normal copying
otherwise.

### `process.sh`

This script is used by `ocaml-compiler` packages which support Relocatable OCaml
and provides the ability to clone a compiler from an existing opam switch,
rather than freshly building it from sources. It uses the `-clone.sh` scripts
produced by `generate.ml` in order to do this.

`process.sh` is called in both the `build` and `install` sections of the
`ocaml-compiler` package. In the `build` section, it is passed enough
information to be able to build the compiler from sources if necessary, but also
the opam `build-id` of the compiler. The script searches for an existing
compiler. If one is found, the script records these details to be used when it
is called again from the `install` section. Otherwise, it builds the compiler
from the sources available. When invoked from the `install` section,
`process.sh` then either clones the compiler which was found in the `build`
stage, or installs the compiler which was built in the `build` stage, along with
its cloning script, which allows subsuquent switches to clone it.
