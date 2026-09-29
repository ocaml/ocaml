# FlexDLL: an implementation of a dlopen-like API for Windows

[![Build status](https://ci.appveyor.com/api/projects/status/github/ocaml/flexdll?svg=true)](https://ci.appveyor.com/project/dra27/flexdll-7sptc)

## Introduction

Under Windows, DLL ([Dynamically-Linked Libraries][DLL]) are generally
used to improve code modularity and sharing. A DLL can be loaded
automatically when the program is loaded (if it requires the DLL).
The program can also explicitly request Windows to load a DLL at any
moment during runtime, using the [`LoadLibrary`][LoadLibrary] function
from the Win32 API.

This naturally suggests to use DLLs as a plugin mechanism. For instance,
a web server could load extensions modules stored in DLLs at runtime.
But Windows does not really make it easy to implement plugins that way.
The reason is that when you try to create a DLL from a set of object
files, the linker needs to resolve all the symbols, which leads to the
very problem solved by FlexDLL:

**Windows DLL cannot refer to symbols defined in the main application or
in previously loaded DLLs.**

Some usual solutions exist, but they are not very flexible. A notable
exception is the [edll][] library (its homepage also describes the usual
solutions), which follows a rather drastic approach; indeed, edll
implements a new dynamic linker which can directly load object files
(without creating a Windows DLL).

FlexDLL is another solution to the same problem. Contrary to edll, it
relies on the native static and dynamic linkers. Also, it works both
with the Microsoft environment (MS linker, Visual Studio compilers) and
with Cygwin (GNU linker and compilers, in Cygwin or MinGW mode).
Actually, FlexDLL implements mostly the usual [`dlopen`][dlopen] POSIX
API, without trying to be fully conformant though (e.g. it does not
respect the official priority ordering for symbol resolution).  This
should make it easy to port applications developed for Unix.

[DLL]: https://en.wikipedia.org/wiki/Dynamic-link_library
[LoadLibrary]: https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibraryw
[edll]: https://edll.sourceforge.net/
[dlopen]: https://pubs.opengroup.org/onlinepubs/9799919799/functions/dlopen.html

## About

FlexDLL is distributed under the terms of a zlib/libpng open source
[license](LICENSE). The copyright holder is the Institut National de
Recherche en Informatique et en Automatique (INRIA). The project was
started when I (= Alain Frisch) was working at INRIA. I'm now working
for [LexiFi][], which is kind enough to let me continue my work on
FlexDLL. My office mate at INRIA, Jean-Baptiste Tristan, coined the name
FlexDLL.

The runtime support library is written in C. The `flexlink` wrapper is
implemented in the wonderful [OCaml][] language.

[LexiFi]: https://www.lexifi.com
[OCaml]: https://ocaml.org/

## Supported toolchains

MSVC: the 32-bit C compiler from Microsoft.

MSVC64: the 64-bit C compiler from Microsoft.

CYGWIN64: the 64-bit gcc compiler shipped with Cygwin.

MINGW: the 32-bit gcc compiler from the MinGW-w64 project, packaged in
Cygwin (as i686-w64-mingw32-gcc).

MINGW64: the 64-bit gcc compiler from the MinGW-w64 project, packaged in
Cygwin (as x86\_64-w64-mingw32-gcc).

LD: an internal linker to produce .dll (only).

## Download

- [Source and binary releases](https://github.com/ocaml/flexdll/releases).
- [Development version](https://github.com/ocaml/flexdll).
- [Changelog](CHANGES).

**Installation instructions:** Simply run the installer and add the
resulting directory (e.g. `C:\Program Files\flexdll` or
`C:\Program Files (x86)\flexdll`) to the PATH. You can also create this
directory by hand and unzip the .zip file in it.

**Compiling from sources:** To compile the code from sources, you'll
need a working installation of OCaml, GNU Make, and a C toolchain
(compiler + linker) either the one from Microsoft (any version of Visual
Studio should work), Cygwin, or MinGW. It is probably a good idea to use
a native version of ocamlopt (not the Cygwin port) to compile flexlink.
By default, the `Makefile` will compile support objects for the
supported toolchains; you can choose a subset with the `CHAINS`
variable, e.g.: `make CHAINS="mingw msvc"`.


## Overview

FlexDLL has two components: a wrapper around the static linker, and a
tiny runtime library to be linked with the main application. The wrapper
must be called in place of the normal linker when you want to produce a
DLL or to link the main application. The runtime library relies
internally on the native `LoadLibrary` API to implement a dlopen-like
interface.

Let's see a simple example of a plugin. Here is the code for the main
program (file `dump.c`):

```c
#include <stdlib.h>
#include "flexdll.h"

typedef void torun(void);

void api(char *msg){ printf("API: %s\n", msg); }

int main(int argc, char **argv)
{
  void *sym;
  void *handle;
  int i;
  torun *torun;

  for (i = 1; i < argc; i++) {
    handle = flexdll_dlopen(argv[i], FLEXDLL_RTLD_GLOBAL);

    if (NULL == handle) { printf("error: %s\n", flexdll_dlerror()); exit(2); }

    torun = flexdll_dlsym(handle, "torun");
    if (torun) torun();
  }
  exit(0);
}
```

This application opens in turn all the DLLs given on its command line,
using the FlexDLL function `flexdll_dlopen`. For each DLL, the program
looks for a symbol named `torun` (which is supposed to refer to a
function) and if the symbol is available in the DLL, the function is
called. The program also provides a very simple API to its plugin: the
`api` function. The `FLEX_RTLD_GLOBAL` flag makes all the symbols
exported by each DLL available for the DLL to be loaded later.

This main program can be compiled and linked like the commands below
(the `[...]` refers to the directory where FlexDLL is installed).

```sh
# MSVC
cl /nologo /MD -I[...] -c dump.c
flexlink -chain msvc -exe -o dump.exe dump.obj

# MINGW
i686-w64-mingw32-gcc -I[...] -c dump.c
flexlink -chain mingw -exe -o dump.exe dump.o

# CYGWIN
gcc -I[...] -c dump.c
flexlink -chain cygwin64 -exe -o dump.exe dump.o
```

The compilation step is completely standard, but in order to link the
main application, you must call the `flexlink` tool, which is the
wrapper around the linker. The `-chain` command line switch selects
which linker to use, and the `-exe` option tells the wrapper that it
must produce a stand-alone application (not a DLL).

Now we can provide a first plugin (file `plug1.c`):

```c
int x = 3;
void dump_x(void) { printf("x=%i\n", x); }
void torun(void) { api("plug1.torun();"); }
```

Note that the plugin uses the `api` symbol from the main application (it
would be cleaner to introduce it with an `extern` declaration). You can
compile and link this plugin (into a DLL) with the following commands:

```sh
# MSVC
cl /nologo /MD -c plug1.c
flexlink -chain msvc -o plug1.dll plug1.obj

# MINGW
i686-w64-mingw32-gcc -c plug1.c
flexlink -chain mingw -o plug1.dll plug1.o

# CYGWIN
gcc -D_CYGWIN_  -c plug1.c
flexlink -chain cygwin64 -o plug1.dll plug1.o
```

And now you can ask the main program to load the plugin:

```console
$ ./dump plug1.dll
API: plug1.torun();
```

Here is the code for a second plugin (file `plug2.c`) that refers to
symbols (a function and a global variable) defined in the first plugin:

```c
extern int x;

void torun(void) {
  api("plug2.torun();");

  dump_x();
  x = 100;
  dump_x();
}
```

Since the second plugin depends on the first one, you need to load both:

```console
$ ./dump plug2.dll
error: Cannot resolve dump_x
$ ./dump plug1.dll plug2.dll;
API: plug1.torun();
API: plug2.torun();
x=3
x=100
```

Simple, isn't it? No `declspec` declaration, no import library to deal
with, …


## How it works

Object files (.obj/.o) contain relocation information that explain to
the linker how some addresses in their code or data sections have to be
patched, using the value of some global symbols. When the static linker
is invoked to produce a DLL from a set of object files, it assumes that
all the relocations can be performed: all the symbols which are used in
relocations must be defined in some the objects linked together.
FlexDLL drops this constraint following a very simple idea: when a
relocation refers to a symbol which is not available, the relocation is
turned into a piece of data that will be passed to the runtime support
library.

In the example above, the `plug1.obj` object refers to a symbol
`api`. When this object is turned into a DLL, FlexDLL produce a new
temporary object file derived from `plug1.obj` without the relocation
that mentions `api`. Instead, it adds an “import table”, which is just a
piece of data that tells the FlexDLL support library which address has
to be patched with the value of a symbol called `api` to be found
somewhere else. You can see the list of such imported symbols by adding
the `-show-imports` option to the `flexlink` command line:

```console
$ ../flexlink -chain msvc -o plug1.dll plug1.obj -show-imports
** Imported symbols for plug1.obj:
_api
```

When the `flexdll_dlopen` function opens this DLL, it will look for an
internal symbol that points to the import table, resolve the symbols and
patch the code and data segments accordingly. The FlexDLL runtime
library must thus maintain a set of symbols together with their concrete
values (addresses). In particular, it knows about the global symbols
defined in the main program. Indeed, when you link the main program with
`flexlink -exe`, the wrapper produces a small fresh object file that
contains a symbol table, mapping symbol names to their addresses.

```console
$ ../flexlink -chain msvc -exe -o dump.exe dump.obj -show-exports
** Exported symbols:
_api
_flexdll_dlclose
_flexdll_dlerror
_flexdll_dlopen
_flexdll_dlsym
_flexdll_dump_exports
_flexdll_dump_relocations
_main
```

As you can see, all the global symbols (including those that comes from
FlexDLL itself) appear in the global symbol table. FlexDLL knows not
only about symbols that comes from the main program, but also about
symbols exported by the DLL it loads. This is needed to implement the
`flexdll_dlsym` function, but also to deal with import tables that
mention symbols defined in previously loaded DLLs (for which the
`FLEXDLL_RTLD_GLOBAL` was used). So the wrapper produces not only an
import table for DLLs, but also an export table:

```console
$ ../flexlink -chain msvc -o plug1.dll plug1.obj -show-imports -show-exports
** Imported symbols for plug1.obj:
_api
** Exported symbols:
_dump_x
_torun
_x

$ ../flexlink -chain msvc -o plug2.dll plug2.obj -show-imports -show-exports
** Imported symbols for plug2.obj:
_api
_dump_x
_x
** Exported symbols:
_torun
```

How does FlexDLL determine which symbols are imported or exported? It
uses an algorithm similar to the linker itself. The command line
mentions a number of object and library files. In a first pass, the
wrapper computed which objects embedded in those libraries will be used.
To do that, it looks at which symbols are used, and where they are
defined. Then the wrapper considers that all the global (external)
symbols are exported. Note that the `/export` or `__declspec(dllexport)`
directives are not used: all the symbols are exported. (In a future
version, FlexDLL will allow to control more precisely which symbols are
exported). All the object files (given explicitly, or embedded in a
library) that need to import symbols must be rewritten. The `flexlink`
wrapper will produce new temporary object files for them. If you want to
understand better how FlexDLL works, you can use the `-v` and
`-save-temps` command options to tell the wrapper to show you the linker
command line and to preserve those temporary files alive (by default, they
are removed automatically).

Some object files can mention default libraries (they correspond to the
`/defaultlib` linker flag, which is often embedded in the object
`.drectve` section). FlexDLL will parse those libraries, but only to see
which symbols they define. Those symbols are not considered as being
imported by the DLL, but they won't be exported either. A typical case
of default libraries are import libraries that behave as interfaces to
(normal, non-FlexDLL) DLLs.


## Advanced topic: `__declspec(dllimport)`

C compilers under Windows support a special declaration of external
symbols. You can write:

```c
__declspec(dllimport) extern int mysymbol;
```

Internally, this declaration has the same effect as declaring:

```c
extern int *_imp__mysymbol;
```

and using `&x` instead of `x` everywhere in the current unit. In other
words, even if your code seems to access `x` directly, each access
actually goes through an extra indirection.

FlexDLL knows about this convention. When a object refers to a symbol of
the form `_imp__XXX` which is not available in the objects that will
form the DLL to be created, it resists the temptation of putting an
entry for `_imp__XXX` in the import table. Instead, it adds the
equivalent of the following declaration:

```c
void *_imp__XXX = &XXX;
```

If the symbol `XXX` itself is not available, this will in turn produce
an entry for `XXX` in the import table. All these new declarations are
put in the same object file that contain the export table, which is
global for the DLL to be produced. So, if all the external symbols in a
given object files are accessed through this convention, the object file
need not be patched at all.

Note that you can define and use the `_imp__XXX` symbols by hand, you
don't have to use the `__declspec(dllimport)` notation (this is useful
if you use a compiler that doesn't support this notation).

There *are* compelling reasons to adopt this style.

A very small advantage might be that there will be fewer relocations at
runtime and that more code pages can be shared amongst several instances
of the same DLL used by different processes.

A big advantage on x86_64 systems is to avoid relocation errors like:

```text
Fatal error: cannot load shared library plug1
Reason: flexdll error: cannot relocate XXX
  RELOC_REL32, target is too far: FFFFFFFF2EDEC956  000000002EDEC956
```

Without `__declspec(dllimport)` your symbol (ex. `extern XXX`) generates
the following pseudo-assembly:

```asm
CALL XXX ; this is a 32-bit relative address
```

These 32-bit relative addresses are restricted to 2GiB jumps, and there is
no guarantee that your main programs and all its DLLs will reside in the same
2GiB block of virtual memory. The more DLLs you have, and the bigger they
are, the more likely you will encounter a `RELOC_REL32` error.

flexdll will verify every 32-bit relative address `CALL` during
`flexdll_dlopen` to ensure they are within 2GiB.
However, the indirection introduced by `__declspec(dllimport) extern` makes
the pseudo-assembly instead:

```asm
void *_imp__XXX = &XXX;
CALL [_imp__XXX] ; this is a 64-bit indirect address
```

which works well with flexdll.

## Advanced topic: static constructors and the entry point

A Windows DLL can define an optional entry point. When the DLL is
loaded, this function is automatically called. (The same function is
called when the DLL is unloaded, or when threads are spawned or
destroyed.)

Usually, the real entry point is provided by the C runtime library:
`_cygwin_dll_entry` for Cygwin, `DllMainCRTStartup` for MinGW,
`_DllMainCRTStartup` for MSVC. These functions perform various
initialization for the C runtime library, invoke the code that has to be
run automatically at load time (e.g for C++: constructors of static
objects, or right-hand sides of non-constant initializers for global
variables), and then call the function [`DllMain`][DllMain], which by
default does nothing but can be overridden by the program to perform
custom initialization of the DLL.

FlexDLL must take control before any custom code (static constructors,
`DllMain`) so as to perform relocations (in case this code refers to
symbols found in the main program or previously loaded DLLs). As a
consequence, FlexDLL defines its own entry point, which first asks the
main program to perform relocations and then calls the regular entry
point of the C runtime library. This behavior is implemented in the
`flexdll_initer.c` file, and the corresponding object file (whose name
depends on the toolchain) is automatically included by `flexlink`.

It is possible to completely disable the DLL entry point with the
`-noentry` option passed to `flexlink`. In this case, FlexDLL will
perform relocations after the DLL has been opened.

[DllMain]: https://learn.microsoft.com/en-us/windows/win32/dlls/dllmain

## The API

Here is the content of the `flexdll.h` file:

```c
#define FLEXDLL_RTLD_GLOBAL 0x0001
#define FLEXDLL_RTLD_LOCAL  0x0000
#define FLEXDLL_RTLD_NOEXEC 0x0002

void *flexdll_dlopen(const char *, int);
#ifndef CYGWIN
void *flexdll_wdlopen(const wchar_t *, int);
#endif
void *flexdll_dlsym(void *, const char *);
void flexdll_dlclose(void *);
char *flexdll_dlerror(void);

void flexdll_dump_exports(void *);
void flexdll_dump_relocations(void *);
```

The `flexdll_dl*` functions are mostly compatible with their POSIX
counterparts. Here is a short explanation of their semantics.

The most important function is `flexdll_dlopen`. The first argument
gives the filename of a DLL to be opened. This DLL must have been
produced by the `flexlink` wrapper. The function resolves the symbols
mentioned in the DLL's import table and performs the relocations.
The second argument is a mode, made of flags that can be or'ed together.
The flag `FLEXDLL_RTLD_GLOBAL` means that the symbols exported by the
opened DLL can be used to resolve relocations for DLLs to be opened
later on. The flag `FLEXDLL_RTLD_NOEXEC` opens the DLL is a special
mode, disabling the automatic loading of dependencies and the FlexDLL
resolution pass. This is useful if you want to open a DLL only to check
whether it defines some symbol.

The `flexdll_dlopen` function returns a pointer to an opaque handle that
can be used as an argument to the other API functions. If the filename
is `NULL`, the function returns a special handle which refers to the
global unit: it includes all the static symbols, plus the symbols from
the DLLs opened with the `FLEXDLL_RTLD_GLOBAL` flag. A given DLL will be
opened only once, even if you call the function several times on the
same file. The `FLEXDLL_RTLD_GLOBAL` flag is sticky: if one of the calls
mentions it, it will stay forever, even if the corresponding handle is
then passed to `dlclose` (this is because the same handle is actually
returned for all the calls). If an error occurs during the call to
`flexdll_dlopen`, the functions returns `NULL` and the error message can
be retrieved using `flexdll_dlerror`.

The function `flexdll_wdlopen` is a wide-character version of
`flexdll_dlopen`. The filename argument to `flexdll_wdlopen` is a
wide-character string. `flexdll_wdlopen` and `flexdll_dlopen` behave
identically otherwise.

The second most important function is `flexdll_dlsym` which looks for a
symbol whose name is the second argument. The first argument can be
either a regular handle returned by `flexdll_dlopen` (the symbol is
searched only in the corresponding DLL), the special handle for the
global unit as return by a call to `flexdll_dlopen(NULL,...)` (the
symbol is searched amongst the static symbols plus the ones in the DLL
opened with the flag `FLEXDLL_RTLD_GLOBAL`), or `NULL` (the symbol is
searched only amongst the static symbols). If the symbol cannot be
found, the function returns `NULL`.

The same symbol name can be defined several times. The policy used to
choose amongst the various definitions is not specified. This applies
both to the automatic resolution that happens when you open a DLL and to
the explicit resolution performed by `dlsym`.

The function `flexdll_dlclose` must be used with caution. It decrements
the reference counter for the given handle and releases the DLL from memory
when the counter reaches 0. After that time, symbols defined in this DLL
are no longer used for resolution. You most probably don't want to close
a DLL if you still hold pointers to some of its symbols.

The two functions `flexdll_dump_exports` and `flexdll_dump_relocations`
are used to display (to the standard output) the internal tables
associated with a given DLL handle.


## Command line for the flexlink wrapper

```
Usage:
  flexlink -o <result.dll/exe> file1.obj file2.obj ... -- <extra linker arguments>

  -o                  Choose the name of the output file
  -exe                Link the main program as an exe file
  -maindll            Link the main program as a dll file
  -noflexdllobj       Do not add the Flexdll runtime object (for exe)
  -noentry            Do not use the Flexdll entry point (for dll)
  -noexport           Do not export any symbol
  -norelrelocs        Ensure that no relative relocation is generated
  -base               Specify base address (Win64 only)
  -pthread            Pass -pthread to the linker
  -I <dir>            Add a directory where to search for files
  -L <dir>            Add a directory where to search for files
  -l <lib>            Library file
  -chain {msvc|msvc64|cygwin64|mingw|mingw64|gnat|gnat64|ld}
                      Choose which linker to use
  -use-linker <cmd>   Choose an alternative linker to use
  -use-mt <cmd>       Choose an alternative manifest tool to use
  -x64                (Deprecated)
  -defaultlib <obj>   External object (no export, no import)
  -save-temps         Do not delete intermediate files
  -implib             Do not delete the generated import library
  -outdef             Produce a def file with exported symbols
  -v                  Increment verbosity (can be repeated)
  -show-exports       Show exported symbols
  -show-imports       Show imported symbols
  -dry                Show the linker command line, do not actually run it
  -dump               Only dump the content of object files
  -patch              Only patch the target image (to be used with -stack)
  -nocygpath          Do not use cygpath (default for msvc, mingw)
  -cygpath            Use cygpath (default for cygwin)
  -no-merge-manifest  Do not merge the manifest (takes precedence over -merge-manifest)
  -merge-manifest     Merge manifest to the dll or exe (if generated)
  -real-manifest      Use the generated manifest (default behavior)
  -default-manifest   Use the default manifest (default.manifest/default_amd64.manifest)
  -export <sym>       Explicitly export a symbol
  -noreexport         Do not reexport symbols imported from import libraries
  -where              Show the FlexDLL directory
  -nounderscore       Normal symbols are not prefixed with an underscore
  -nodefaultlibs      Do not assume any default library
  -builtin            Use built-in linker to produce a dll
  -explain            Explain why library objects are linked
  -subsystem <id>     Set the subsystem (default: console)
  -custom-crt         Use a custom CRT
  -stack <int>        Set the stack reserve in the resulting image
  -link <option>      Next argument is passed verbatim to the linker
  -g                  (Ignored)
  -D <symbol>         (Ignored)
  -U <symbol>         (Ignored)
  --                  Following arguments are passed verbatim to the linker
  -version            Print linker version and FlexDLL directory and exit
  -vnum               Print linker version number and exit
  -help               Display this list of options
  --help              Display this list of options
```

The files given on the command line can be object files (.obj/.o),
library files (.a/.lib), or C files (.c). C files will be compiled using
the toolchain's C compiler and the resulting object will be used for the
actual linking.

The argument for the `-l`, `-I` and `-L` options does not need to be
separated by whitespace from the option (i.e. `-LXXX` is equivalent to
`-L XXX`).

There is a single set of search directories for all kinds of files.
The `-I` and `-L` options are synonyms.

As usual, object files included in libraries are linked in only if one
of the symbol they export is needed.

The `-export` option explicitly exports a symbol. If this symbols is
found in a library, it will force the corresponding object file to be
included.

The `-defaultlib` option is used to tell `flexlink` that some object
(usually, a library) does not need any relocations (that is, all the
symbols it refers to are defined in one of the objects being linked) and
that we don't want to re-export the symbols exported by this object.
This is usually used for system libraries.

The `-cygpath` and `-nocygpath` options control whether `flexlink` uses
the `cygpath` command or not (default is: no under MSVC, yes under
Cygwin/MinGW if cygpath can be found in the PATH). When it uses
`cygpath`, `flexlink` tries to resolve file names directly and otherwise
calls `cygpath -m` (to produce Windows paths from Cygwin paths) if
`cygpath` is available in the path.

The `-maindll` option is used to build a DLL that behaves as the main
program from the point of view of FlexDLL. It cannot have unresolved
symbols.

By default, `flexlink` looks for FlexDLL's object files in the same
directory as `flexlink.exe` itself. It is possible to specify another
directory with the `FLEXDIR` environment variable.

Extra arguments can be passed to `flexlink.exe` through the environment
variable `FLEXLINKFLAGS`. The arguments coming from this variable are
parsed before those coming from the command line.


## Performance

FlexDLL performs relocations at runtime in the code of the DLL. The good
consequence is that there is no indirection: a function call or a
reference to a global variable where the target symbol is not in the
current DLL be compiled as if it were. This might improve performance,
especially because an indirection would consume a register that might be
better used for something more interesting. The bad consequence is that
the memory pages that contains relocations cannot be shared between
different processes.


## Bugs, limitations

FlexDLL relies on a parser and generator for COFF files. However, some
features are not very well specified, and some well specified features
have not been fully implemented. Normally, you should get some assertion
failure in these cases. Please report them, so that I can improve
FlexDLL.

FlexDLL works for 32 and 64 bits version of Windows. The 32 bits version
has been tested under XP and Vista, with the three supported toolchains.
The 64 bits version has been tested under Windows Vista x64 with the
Microsoft Platform SDK and under Windows 7 64-bit with the Win7 SDK
toolchain (no Cygwin).


## Real-world examples

Please let me know if you use FlexDLL!

### Dynamic loading for OCaml

The initial motivation for FlexDLL was to add dynamic linking of native
code to Windows ports of [OCaml][] (Cygwin, MinGW, MSVC). A side-effect
was to simplify the dynamic loading of C libraries (e.g. for the
toplevel) and to make it work under the Cygwin port, to simplify
Makefiles of libraries (now shared between Unix and Windows ports), and
to create a native toplevel.


## Links

- [Microsoft Portable Executable and Common Object File Format Specification](https://learn.microsoft.com/en-us/windows/win32/debug/pe-format).
- [Enhanced Dynamic Linking Library for MinGW under MS-Windows (edll)][edll].
- [`dlopen` (POSIX)][dlopen].
- [`DllMain`][DllMain].
