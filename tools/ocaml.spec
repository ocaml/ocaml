Name: ocaml
Version: 2.03
Release: 1
Summary: The Objective Caml compiler and programming environment
Source0: ftp://ftp.inria.fr/lang/caml-light/ocaml-2.03.tar.gz
Source1: ftp://ftp.inria.fr/lang/caml-light/ocaml-2.03-refman.html.tar.gz
Source2: ftp://ftp.inria.fr/lang/caml-light/ocaml-2.03-refman.ps.gz
Copyright: freely redistributable
Group: Development/Languages
Vendor: INRIA Rocquencourt
URL: http://caml.inria.fr/

%description
Objective Caml is a high-level, strongly-typed, functional and
object-oriented programming language from the ML family of languages.

This package comprises two batch compilers (a fast bytecode compiler
and an optimizing native-code compiler), an interactive toplevel system,
Lex&Yacc tools, a replay debugger, and a comprehensive library.

%prep
%setup -T -b 0
%setup -T -D -a 1
cp ../../SOURCES/ocaml-2.02-refman.ps.gz refman.ps.gz

%build
./configure -bindir /usr/bin -libdir /usr/lib/ocaml -mandir /usr/man/man1
make world opt ocamlc.opt ocamlopt.opt
(cd emacs; make ocamltags)

%install
rm -rf /usr/lib/ocaml
make install
(cd emacs; make install install-ocamltags EMACSDIR=/usr/lib/emacs/site-lisp)

%files
/usr/bin/ocaml
/usr/bin/ocamlc
/usr/bin/ocamlcp
/usr/bin/ocamldebug
/usr/bin/ocamldep
/usr/bin/ocamllex
/usr/bin/ocamlmktop
/usr/bin/ocamlopt
/usr/bin/ocamlprof
/usr/bin/ocamlrun
/usr/bin/ocamlyacc
/usr/bin/ocamlc.opt
/usr/bin/ocamlopt.opt
/usr/bin/ocamltags
/usr/lib/ocaml
/usr/man/man1/ocaml.1
/usr/man/man1/ocamlc.1
/usr/man/man1/ocamlcp.1
/usr/man/man1/ocamldep.1
/usr/man/man1/ocamllex.1
/usr/man/man1/ocamlopt.1
/usr/man/man1/ocamlprof.1
/usr/man/man1/ocamlrun.1
/usr/man/man1/ocamlyacc.1
/usr/man/man1/ocamlmktop.1
/usr/man/man1/ocamlc.opt.1
/usr/man/man1/ocamlopt.opt.1
/usr/lib/emacs/site-lisp/caml-font.el
/usr/lib/emacs/site-lisp/caml-hilit.el
/usr/lib/emacs/site-lisp/caml.el
/usr/lib/emacs/site-lisp/camldebug.el
/usr/lib/emacs/site-lisp/inf-caml.el
/usr/lib/emacs/site-lisp/caml-compat.el
%doc refman.ps.gz htmlman
