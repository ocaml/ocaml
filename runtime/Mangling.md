# Filename Mangling

## Background

OCaml compiler installations exist in isolation. When running the compiler, it
is assumed that the caller will have configured the environment of the compiler
such that files and settings related to other compiler installations will not
interfere.

This is not true of the runtime. Shared libraries are loaded from a global
namespace (dynamically loaded bytecode stub libraries and the shared versions of
both the native and bytecode runtimes) and programs may be searched in a global
PATH. To allow programs compiled against different coinstalled versions of the
runtime to be executed, a name mangling scheme is used for the runtime's
executables and shared libraries.

## Filename Mangling

Filenames are mangled using one or both of two pieces of configuration
information. The first is the standard "autoconf" triplet on which the runtime
executes (e.g. `x86_64-pc-linux-gnu`). The other is a summary of the runtime
version and configuration called the Runtime ID. This information is a series of
bits encoded in base32 using the alphabet `[0-9a-v]` and with the quintets laid
out little-endian.

Mangling is applied to the name of any file which will be searched for at
runtime:

- `ocamlrun` (and variants) are triplet-prefixed and Bytecode-suffixed. For
  example, `x86_64-pc-linux-gnu-ocamlrun-a140` is OCaml 5.5 configured with
  `--disable-flat-float-array` on 64-bit Intel/AMD Linux. A symbolic link is
  still created for `ocamlrun` pointing to this mangled name. Additionally, a
  symbolic link is also created for `ocamlrun-a140`, using the Zinc-suffix.
- C stub libraries loaded by both the bytecode runtime and bytecode `Dynlink`
  library are triplet- and Bytecode-suffixed. For example,
  `dllunixbyt-x86_64-pc-linux-gnu-a140.so` contains the C stubs for the Unix
  library for OCaml 5.5 configured with `--disable-flat-float-array` on 64-bit
  Intel/AMD Linux.
- Shared versions of the bytecode and native runtimes (`libcamlrun_shared.so`
  and `libasmrun_shared.so`) are triplet- and Bytecode/Native-suffixed
  respectively. For example, `libasmrun-x86_64-pc-linux-gnu-a1k0.so` and
  `libcamlrun-x86_64-pc-linux-gnu-a140.so` are OCaml 5.5 configured with
  `--disable-flat-float-array` and `--enable-tsan` on 64-bit Intel/AMD Linux
  (note the **tsan** bit not being set for the name of libcamlrun).
  Additionally, symbolic links are also created for `libasmrun_shared.so` and
  `libcamlrun_shared.so`.

## Runtime ID

A Runtime ID is a bit string describing a given OCaml runtime. At present,
20 bits are used, but the format is intended to be trivially extensible.
Ultimately, the only requirement is that each version and configuration
generates some kind of unique identifier which can then be used in filenames.

- Bit 0 (**dev**): Development bit. This should be set for development versions
  of OCaml or for customised compilers. If it is not set, the compiler should be
  an unaltered official release.
- Bits 1-6 (**release**): OCaml release number. This is incremented for each
  minor release of the compiler, with OCaml 3.12.0[^1] being release 0. At
  present, the ordering of release numbers matches the semantic ordering of the
  version numbers, but this is not guaranteed and should not be assumed[^2].
- Bits 7-11 (**reserved**): Number of reserved bits in the OCaml value header.
  This is the number passed to `--enable-reserved-header-bits` when the compiler
  distribution was configured.
- Bit 12 (**no-flat-float-array**): Set if the compiler distribution was
  configured with `--disable-flat-float-array`.
- Bit 13 (**fp**): Set if the compiler distribution was configured with
  `--enable-frame-pointers`. Affects the **native** runtime only.
- Bit 14 (**tsan**): For OCaml 5.2 onwards, set if the compiler distribution was
  configured with `--enable-tsan`. Prior to OCaml 5.2, set if the compiler
  distribution was configured with `--enable-spacetime` (this option was removed
  in OCaml 4.12, meaning this bit is always unset for OCaml 4.12-5.1). Affects
  the **native** runtime only.
- Bit 15 (**int31**): Set if the runtime uses 31-bit `int` values (i.e. runtimes
  running on 32-bit systems).
- Bit 16 (**static**): Set if the runtime does not support shared libraries,
  meaning dynamic loading of C code is not supported in bytecode, and native
  dynlink is not supported at all.
- Bit 17 (**no-compression**): For OCaml 5.1 onwards, set if the runtime does
  not support compressed marshalling. Prior to OCaml 5.1, set if the compiler
  distribution was configured with `--enable-naked-pointers` (this bit was
  always unset for OCaml 5.0, since it supports neither naked pointers nor
  compressed marshalling).
- Bit 18 (**ansi**): Set if the compiler distribution was configured with the
  legacy support `WINDOWS_UNICODE=ansi`.
- Bit 19 (**mutable-string**): Set if the compiler distribution was configured
  with `--disable-force-safe-string`. This option was removed in OCaml 5.0, and
  the bit is available for re-use. When this bit is unset, strings are
  guaranteed to be immutable.

The bit descriptions are designed such that the default configuration of the
latest version of the compiler has unset bits. The ordering of the bits is
designed to mean ID values in the same version of OCaml will usually have the
same opening sequence of characters (since `--enable-reserved-header-bits` is
now rarely used) and laying out the characters little-endian in the mangling
scheme means that the opening two characters of the Runtime ID define its
version (and consequently its length, should that change in future).

[^1]: OCaml 3.12.0 was the first version where `ocamlrun` supported the `-vnum`
argument; the original author had a fantasy of backporting the scheme to the
entire 4.x series, but following some therapy stopped at 4.08. The release
numbering persists to allow for future madness.
[^2]: In particular, should there be any additional releases in the OCaml 4.x
series, these will have higher release numbers than releases already made in the
OCaml 5.x series.

## Masks

A particular configuration of the compiler has one Runtime ID, but this is used
in three different contexts where certain bits are masked out:

1. _Bytecode Mask_: masks out bits which are only ever set by the native runtime
   (at present, **fp** and **tsan**).
2. _Native Mask_: masks out bits which are only ever set by the bytecode runtime
   (at present there aren't any).
3. _Zinc Mask_: masks out bits which are not related to bytecode portability.
   Where the _Bytecode_ and _Native_ masks relate to _runtimes_, the _Zinc_ mask
   relates to _bytecode images_. The Zinc ID therefore includes:
   - **release** and **dev** (a given bytecode image targets a specific version
     of OCaml)
   - **no-flat-float-array** (code compiled assuming that float arrays are boxed
     will segfault on runtimes which unbox them)
   - **int31**, **static**, and **no-compression** (a bytecode image using
     63-bit integers, dynamically loaded C stubs and compressed marshalling will
     be rejected by an interpreter which doesn't support any of these features)

Note that the inclusion of a bit in a mask is determined by whether that
property affects the ability to load and execute the code, rather than whether
it is semantically affected by it. For example, the **reserved** bits affects
the value representation, and therefore both runtimes. It does not directly
affect bytecode (although a bytecode program may use unsafe features to observe
it). **reserved** is therefore part of both the _Bytecode_ and _Native Masks_,
but not part of the _Zinc Mask_. Similarly, although **no-flat-float-array**
affects code generation for bytecode, **mutable-string** never did, and so would
not be included in the _Zinc Mask_.
