# Changes to `configure` in OCaml 4.08.0+

The `configure` script was switched from a handwritten script to GNU autoconf
starting with OCaml 4.08.0. Many options passed to `configure` changed name as a
result and these are described here.

`configure` previously did not accept anything passed via environment variables.
Note that GNU autoconf always prefers a variable to be passed explicitly *after*
`configure`, i.e. `./configure CC=gcc` is preferred over `CC=gcc ./configure`.

`--prefix`, `--bindir`, `--libdir`, `--mandir`, `--host`, `--target` and
`--verbose` are unchanged.

## Parameters

- `-target-bindir`
  --with-target-bindir=param **dra27** _did this get changed to `TARGET_BINDIR`?_
- `-disable-libunwind` ⇒ `--without-libunwind`
- `-libunwinddir`
  **dra** _what happened here?_
- `-libunwindlib`
  **dra** _what happened here?_
- `-libunwindinclude`
  **dra** _what happened here?_
- `-cc*` should be passed using `CC=`
- `-as` should be passed using `AS=`
- `-aspp` should be passed using `ASPP=`
- `-lib*` should be passed using `LIBS=`
- `-no-curses` is no longer supported (redundant since OCaml 4.07.0)
- `-no-shared-libs` ⇒ `--disable-shared`
- `-x11include*` ⇒ `--x-includes=`_`*`_
- `-x11lib*` ⇒ `--x-libraries=`_`*`_
- `-no-graph` ⇒ `--disable-graph-lib`
- `-with-pthread*` ⇒ `--enable-systhreads`
- `-no-pthread*` ⇒ `--disable-systhreads` **dra27** _is that correct - it was `--disable-pthread` early on?_
- `-partialld` should be passed using `PARTIALLD=`
- `-dldefs*` should be passed using `CFLAGS=`_`*`_
- `-dllibs*` should be passed using `DLLIBS=`_`*`_ **dra27** _is that where this ended up?_
- `-with-debug-runtime`  ⇒ `--enable-debug-runtime`
- `-no-debug-runtime` ⇒ `--disable-debug-runtime`
- `-with-instrumented-runtime` ⇒ `--enable-instrumented-runtime`
- `-no-instrumented-runtime` ⇒ `--disable-instrumented-runtime`
- `-no-debugger` ⇒ `--disable-debugger`
- `-no-ocamldoc` ⇒ `--disable-ocamldoc`
- `-no-ocamlbuild` is no longer supported (redundant since OCaml 4.03.0)
- `-with-frame-pointers` ⇒ `--enable-frame-pointers`
- `-no-naked-pointers` ⇒ `--disable-naked-pointers`
- `-spacetime` ⇒ `--enable-spacetime`
- `-disable-call-counts` ⇒ `--disable-call-counts` **dra27** _did this get changed?_
- `-reserved-header-bits` ⇒ `--with-reserved-header-bits` **dra27** _did this get changed?_
- `-no-cfi` ⇒ `--disable-cfi`
- `-install-source-artifacts` ⇒ `--enable-installing-source-artifacts` **dra27** _final name?_
- `-no-install-source-artifacts` ⇒ `--disable-installing-source-artifacts` **dra27** _final name?_
- `-no-native-compiler` ⇒ `--disable-native-compiler`
- `-install-bytecode-programs` ⇒ `--enable-installing-bytcode-programs` **dra27** _final name?_
- `-no-install-bytecode-programs` ⇒ `--enable-installing-bytecode-programs` **dra27** _final name?_
- `-flambda` ⇒ `--enable-flambda`
- `-with-flambda-invariants` ⇒ `--enable-flambda-invariants` **dra27** _final name?_
- `-with-cplugins` is no longer supported (redundant since OCaml 4.05.0)
- `-no-cplugins` is no longer supported (redundant since OCaml 4.05.0)
- `-fPIC` ⇒ `--with-pic`
- `-force-safe-string` ⇒ `--enable-force-safe-string` **dra27** _final name?_
- `-no-force-safe-string` ⇒ `--disable-force-safe-string` **dra27** _final name?_
- `-default-safe-string` ⇒ `--with-default-string=safe` **dra27** _final name?_
- `-default-unsafe-string` ⇒ `--with-default-string=unsafe` **dra27** _final name?_
- `-flat-float-array` ⇒ `--enable-float-float-array`
- `-no-flat-float-array` ⇒ `--disable-flat-float-array`
- `-afl-instrument` ⇒ `--with-afl`
