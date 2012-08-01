#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# To use this test set, you OCaml installed in a directory where
# you have write rights.

# Warning: use of this Makefile will install lots of software
# in the same place where OCaml is installed.

# It is recommended that you install OCaml in some isolated
# directory D (for example /usr/local/ocaml/test), add D/bin
# at the front of your PATH, then use this Makefile to test
# your OCaml installation.

WGET = wget --no-check-certificate --progress=dot:mega

PREFIX = "`ocamlc -where | sed -e 's|/[^/]*/[^/]*$$||'`"
VERSION = `ocamlc -vnum`

.PHONY: default
default:
	@printf "\n\n########## Starting make at " >>log-${VERSION}
	@date >>log-${VERSION}
	${MAKE} platform >>log-${VERSION} 2>&1
	mv log-${VERSION} log_${VERSION}_`date -u '+%Y-%m-%d:%H:%M:%S'`

# Platform-dependent subsets: add your own here.

.PHONY: all-cygwin
all-cygwin: findlib ounit res pcre react ocamltext ocamlssl camlzip cryptokit \
            sqlite menhir hevea mysql ocgi xmllight configfile xmlm omake \
	    camomile zen vsyml extlib fileutils ocamlify ocamlmod \
            calendar dbm ocamlscript camlp5 geneweb coq

platform:
	case `uname -s` in \
	  CYGWIN*) ${MAKE} all-cygwin;; \
	  *) ${MAKE} all;; \
	esac

# http://projects.camlcity.org/projects/findlib.html
FINDLIB=findlib-1.3.3
${FINDLIB}.tar.gz:
	${WGET} http://download.camlcity.org/download/$@
findlib: ${FINDLIB}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${FINDLIB}
	tar zxf ${FINDLIB}.tar.gz
	./Patcher.sh ${FINDLIB}
	( cd ${FINDLIB} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure && \
	  ${MAKE} all && \
	  ${MAKE} opt && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${FINDLIB} findlib
distclean::
	rm -f ${FINDLIB}.tar.gz
all: findlib

# http://lablgtk.forge.ocamlcore.org/
LABLGTK=lablgtk-2.14.2
${LABLGTK}.tar.gz:
	${WGET} https://forge.ocamlcore.org/frs/download.php/561/$@
lablgtk: ${LABLGTK}.tar.gz findlib # TODO: add lablgl
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${LABLGTK}
	tar zxf ${LABLGTK}.tar.gz
	./Patcher.sh ${LABLGTK}
	( cd ${LABLGTK} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure -prefix ${PREFIX} && \
	  ${MAKE} world && \
	  ${MAKE} install && \
	  ocamlfind remove lablgtk2 && \
	  mkdir ${PREFIX}/lib/ocaml/site-lib/lablgtk2 && \
	  cp META ${PREFIX}/lib/ocaml/site-lib/lablgtk2/META )
	echo ${VERSION} >$@
clean::
	rm -rf ${LABLGTK} lablgtk
distclean::
	rm -f ${LABLGTK}.tar.gz
all: lablgtk

# http://ocamlgraph.lri.fr/
OCAMLGRAPH=ocamlgraph-1.8.2
${OCAMLGRAPH}.tar.gz:
	${WGET} http://ocamlgraph.lri.fr/download/$@
ocamlgraph: ${OCAMLGRAPH}.tar.gz findlib lablgtk
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLGRAPH}
	tar zxf ${OCAMLGRAPH}.tar.gz
	./Patcher.sh ${OCAMLGRAPH}
	( cd ${OCAMLGRAPH} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure -prefix ${PREFIX} && \
	  ${MAKE} && \
	  rm -rf ${PREFIX}/lib/ocaml/ocamlgraph && \
	  ocamlfind remove ocamlgraph && \
	  ${MAKE} install install-findlib && \
	  ln -s ${PREFIX}/lib/ocaml/site-lib/ocamlgraph \
	        ${PREFIX}/lib/ocaml/ocamlgraph )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLGRAPH} ocamlgraph
distclean::
	rm -f ${OCAMLGRAPH}.tar.gz
all: ocamlgraph

# http://ounit.forge.ocamlcore.org/
OUNIT=ounit-1.1.2
${OUNIT}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/886/$@
ounit: ${OUNIT}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OUNIT}
	tar zxf ${OUNIT}.tar.gz
	./Patcher.sh ${OUNIT}
	( cd ${OUNIT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} && \
	  ocamlfind remove oUnit && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OUNIT} ounit
distclean::
	rm -f ${OUNIT}.tar.gz
all: ounit

# https://bitbucket.org/mmottl/res
RES=res-3.2.0
${RES}.tar.gz:
	${WGET} https://bitbucket.org/mmottl/res/downloads/$@
res: ${RES}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${RES}
	tar zxf ${RES}.tar.gz
	./Patcher.sh ${RES}
	( cd ${RES} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} && \
	  ocamlfind remove res && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${RES} res
distclean::
	rm -f ${RES}.tar.gz
all: res

# https://bitbucket.org/mmottl/pcre-ocaml
PCRE=pcre-ocaml-6.2.5
${PCRE}.tar.gz:
	${WGET} https://bitbucket.org/mmottl/pcre-ocaml/downloads/$@
pcre: ${PCRE}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${PCRE}
	tar zxf ${PCRE}.tar.gz
	./Patcher.sh ${PCRE}
	( cd ${PCRE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} && \
	  ocamlfind remove pcre && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${PCRE} pcre
distclean::
	rm -f ${PCRE}.tar.gz
all: pcre

# https://bitbucket.org/yminsky/ocaml-core/downloads
CORE=core-suite-108.00.02
${CORE}.tar.gz:
	${WGET} https://ocaml.janestreet.com/ocaml-core/108.00.02/$@
core: ${CORE}.tar.gz findlib pcre res ounit
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CORE}
	tar zxf ${CORE}.tar.gz && mv ocaml-core-* ${CORE}
	./Patcher.sh ${CORE}
	( cd ${CORE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./build-and-install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CORE} core
distclean::
	rm -f ${CORE}.tar.gz
all: core

# http://erratique.ch/software/react
REACT=react-0.9.3
${REACT}.tbz:
	${WGET} http://erratique.ch/software/react/releases/$@
react: ${REACT}.tbz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${REACT}
	tar jxf ${REACT}.tbz
	./Patcher.sh ${REACT} oasis-common.patch
	( cd ${REACT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocaml setup.ml -configure && \
	  ocaml setup.ml -build && \
	  ./test.native && \
	  ocamlfind remove react && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${REACT} react
distclean::
	rm -f ${REACT}.tbz
all: react

# http://forge.ocamlcore.org/projects/ocaml-text/
OCAMLTEXT=ocaml-text-0.5
${OCAMLTEXT}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/641/$@
ocamltext: ${OCAMLTEXT}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLTEXT}
	tar zxf ${OCAMLTEXT}.tar.gz
	./Patcher.sh ${OCAMLTEXT} oasis-common.patch
	( cd ${OCAMLTEXT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} build && \
	  ${MAKE} test && \
	  ocamlfind remove text && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLTEXT} ocamltext
distclean::
	rm -f ${OCAMLTEXT}.tar.gz
all: ocamltext

# http://sourceforge.net/projects/savonet/files/ocaml-ssl/
OCAMLSSL=ocaml-ssl-0.4.6
${OCAMLSSL}.tar.gz:
	${WGET} http://voxel.dl.sourceforge.net/project/savonet/ocaml-ssl/0.4.6/$@
ocamlssl: ${OCAMLSSL}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLSSL}
	tar zxf ${OCAMLSSL}.tar.gz
	./Patcher.sh ${OCAMLSSL}
	( cd ${OCAMLSSL} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure && \
	  ${MAKE} && \
	  ocamlfind remove ssl && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLSSL} ocamlssl
distclean::
	rm -f ${OCAMLSSL}.tar.gz
all: ocamlssl

# http://ocsigen.org/lwt/install
LWT=lwt-2.4.0
${LWT}.tar.gz:
	${WGET} http://ocsigen.org/download/$@
lwt: ${LWT}.tar.gz findlib react ocamltext ocamlssl lablgtk
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${LWT}
	tar zxf ${LWT}.tar.gz
	./Patcher.sh ${LWT}
	( cd ${LWT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  export C_INCLUDE_PATH=/usr/include:/opt/local/include && \
	  export LIBRARY_PATH=/usr/lib:/opt/local/lib && \
	  ./configure --enable-ssl --enable-react && \
	  ${MAKE} && \
	  ocamlfind remove lwt && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${LWT} lwt
distclean::
	rm -f ${LWT}.tar.gz
all: lwt

# http://forge.ocamlcore.org/projects/camlzip/
CAMLZIP=camlzip-1.04
${CAMLZIP}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/328/$@
camlzip: ${CAMLZIP}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CAMLZIP}
	tar zxf ${CAMLZIP}.tar.gz
	./Patcher.sh ${CAMLZIP}
	( cd ${CAMLZIP} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} all && \
	  ${MAKE} allopt && \
	  ${MAKE} install && \
	  ${MAKE} installopt && \
	  ocamlfind remove camlzip && \
	  ocamlfind install camlzip META )
	echo ${VERSION} >$@
clean::
	rm -rf ${CAMLZIP} camlzip
distclean::
	rm -f ${CAMLZIP}.tar.gz
all: camlzip

# http://forge.ocamlcore.org/projects/cryptokit/
CRYPTOKIT=cryptokit-1.6
${CRYPTOKIT}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/891/$@
cryptokit: ${CRYPTOKIT}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CRYPTOKIT}
	tar zxf ${CRYPTOKIT}.tar.gz
	./Patcher.sh ${CRYPTOKIT}
	( cd ${CRYPTOKIT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} build && \
	  ${MAKE} test && \
	  ocamlfind remove cryptokit && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CRYPTOKIT} cryptokit
distclean::
	rm -f ${CRYPTOKIT}.tar.gz
all: cryptokit

# https://bitbucket.org/mmottl
SQLITE=sqlite3-ocaml-2.0.1
${SQLITE}.tar.gz:
	${WGET} https://bitbucket.org/mmottl/sqlite3-ocaml/downloads/$@
sqlite: ${SQLITE}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${SQLITE}
	tar zxf ${SQLITE}.tar.gz
	./Patcher.sh ${SQLITE} oasis-common.patch
	( cd ${SQLITE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocaml setup.ml -configure && \
	  ocaml setup.ml -build && \
	  ocamlfind remove sqlite3 && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${SQLITE} sqlite
distclean::
	rm -f ${SQLITE}.tar.gz
all: sqlite

# http://gallium.inria.fr/~fpottier/menhir/
MENHIR=menhir-20120123
${MENHIR}.tar.gz:
	${WGET} http://gallium.inria.fr/~fpottier/menhir/$@
menhir: ${MENHIR}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${MENHIR}
	tar zxf ${MENHIR}.tar.gz
	./Patcher.sh ${MENHIR}
	( cd ${MENHIR} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} PREFIX=${PREFIX} && \
	  ocamlfind remove MenhirLib && \
	  ${MAKE} PREFIX=${PREFIX} install)
	echo ${VERSION} >$@
clean::
	rm -rf ${MENHIR} menhir
distclean::
	rm -f ${MENHIR}.tar.gz
all: menhir

# http://ocsigen.org/obrowser/install
OBROWSER=obrowser-1.1.1
${OBROWSER}.tar.gz:
	${WGET} http://ocsigen.org/download/$@
obrowser: ${OBROWSER}.tar.gz lwt menhir
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OBROWSER}
	tar zxf ${OBROWSER}.tar.gz
	./Patcher.sh ${OBROWSER}
	( cd ${OBROWSER} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} && \
	  ocamlfind remove obrowser && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OBROWSER} obrowser
distclean::
	rm -f ${OBROWSER}.tar.gz
all: obrowser

# http://hevea.inria.fr/
HEVEA=hevea-1.10
${HEVEA}.tar.gz:
	${WGET} http://hevea.inria.fr/distri/$@
hevea: ${HEVEA}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${HEVEA}
	tar zxf ${HEVEA}.tar.gz
	./Patcher.sh ${HEVEA}
	( cd ${HEVEA} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} PREFIX=${PREFIX} && \
	  ${MAKE} PREFIX=${PREFIX} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${HEVEA} hevea
distclean::
	rm -f ${HEVEA}.tar.gz
all: hevea

# http://www.seas.upenn.edu/~bcpierce/unison//download/releases/
UNISON=unison-2.45.4
${UNISON}.tar.gz:
	${WGET} http://www.seas.upenn.edu/~bcpierce/unison/download/releases/unison-2.45.4/$@
unison: ${UNISON}.tar.gz lablgtk
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${UNISON}
	tar zxf ${UNISON}.tar.gz
	./Patcher.sh ${UNISON}
	( cd ${UNISON} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} UISTYLE=gtk2 && \
	  touch ${PREFIX}/bin/unison && \
	  ${MAKE} UISTYLE=gtk2 INSTALLDIR=${PREFIX}/bin/ install )
	echo ${VERSION} >$@
clean::
	rm -rf ${UNISON} unison
distclean::
	rm -f ${UNISON}.tar.gz
all: unison

# http://raevnos.pennmush.org/code/ocaml-mysql/
MYSQL=ocaml-mysql-1.0.4
${MYSQL}.tar.gz:
	${WGET} http://raevnos.pennmush.org/code/ocaml-mysql/$@
mysql: ${MYSQL}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${MYSQL}
	tar zxf ${MYSQL}.tar.gz
	./Patcher.sh ${MYSQL}
	( cd ${MYSQL} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  export CPPFLAGS=-I/opt/local/include/mysql5 && \
	  export LDFLAGS=-L/opt/local/lib/mysql5/mysql && \
	  ./configure -prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} opt && \
	  ocamlfind remove mysql && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${MYSQL} mysql
distclean::
	rm -f ${MYSQL}.tar.gz
all: mysql

# http://gallium.inria.fr/~guesdon/Tools/ocgi/
OCGI=ocgi-0.5
${OCGI}.tar.gz:
	${WGET} http://pauillac.inria.fr/~guesdon/Tools/Tars/$@
ocgi: ${OCGI}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCGI}
	tar zxf ${OCGI}.tar.gz
	./Patcher.sh ${OCGI}
	( cd ${OCGI} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure && \
	  ${MAKE} && \
	  ${MAKE} opt && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCGI} ocgi
distclean::
	rm -f ${OCGI}.tar.gz
all: ocgi

# http://tech.motion-twin.com/xmllight
XMLLIGHT=xml-light-2.2
${XMLLIGHT}.zip:
	${WGET} http://tech.motion-twin.com/zip/$@
xmllight: ${XMLLIGHT}.zip
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf xml-light ${XMLLIGHT}
	unzip ${XMLLIGHT}.zip && mv xml-light ${XMLLIGHT}
	./Patcher.sh ${XMLLIGHT}
	( cd ${XMLLIGHT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} all opt && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${XMLLIGHT} xml-light xmllight
distclean::
	rm -f ${XMLLIGHT}.zip
all: xmllight

# http://config-file.forge.ocamlcore.org/
CONFIGFILE=config-file-1.1
${CONFIGFILE}.tar.gz:
	${WGET} https://forge.ocamlcore.org/frs/download.php/845/$@
configfile: ${CONFIGFILE}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CONFIGFILE}
	tar zxf ${CONFIGFILE}.tar.gz
	./Patcher.sh ${CONFIGFILE}
	( cd ${CONFIGFILE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix=${PREFIX} && \
	  ${MAKE} all && \
	  ocamlfind remove config-file && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CONFIGFILE} configfile
distclean::
	rm -f ${CONFIGFILE}.tar.gz
all: configfile

# http://erratique.ch/software/xmlm
XMLM=xmlm-1.1.0
${XMLM}.tbz:
	${WGET} http://erratique.ch/software/xmlm/releases/$@
xmlm: ${XMLM}.tbz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${XMLM}
	tar jxf ${XMLM}.tbz
	./Patcher.sh ${XMLM} oasis-common.patch
	( cd ${XMLM} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocaml setup.ml -configure && \
	  ocaml setup.ml -build && \
	  ocamlfind remove xmlm && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${XMLM} xmlm
distclean::
	rm -f ${XMLM}.tbz
all: xmlm

# http://forge.ocamlcore.org/projects/gtk-extras/
LABLGTKEXTRAS=lablgtkextras-1.1
${LABLGTKEXTRAS}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/848/$@
lablgtkextras: ${LABLGTKEXTRAS}.tar.gz lablgtk configfile xmlm
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${LABLGTKEXTRAS}
	tar zxf ${LABLGTKEXTRAS}.tar.gz
	./Patcher.sh ${LABLGTKEXTRAS}
	( cd ${LABLGTKEXTRAS} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} all && \
	  ocamlfind remove lablgtk2-extras && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${LABLGTKEXTRAS} lablgtkextras
distclean::
	rm -f ${LABLGTKEXTRAS}.tar.gz
all: lablgtkextras

# https://bitbucket.org/skskeyserver/sks-keyserver/downloads
SKS=sks-1.1.3
${SKS}.tgz:
	${WGET} https://bitbucket.org/skskeyserver/sks-keyserver/downloads/$@
sks: ${SKS}.tgz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${SKS}
	tar zxf ${SKS}.tgz
	./Patcher.sh ${SKS}
	( cd ${SKS} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} dep PREFIX=${PREFIX} && \
	  ${MAKE} all PREFIX=${PREFIX} && \
	  ${MAKE} all.bc PREFIX=${PREFIX} && \
	  ${MAKE} install PREFIX=${PREFIX} )
	echo ${VERSION} >$@
clean::
	rm -rf ${SKS} sks
distclean::
	rm -f ${SKS}.tgz
all: sks

# http://omake.metaprl.org/download.html
OMAKE=omake-0.9.8.6
${OMAKE}-0.rc1.tar.gz:
	${WGET} http://omake.metaprl.org/downloads/$@
omake: ${OMAKE}-0.rc1.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OMAKE}
	tar zxf ${OMAKE}-0.rc1.tar.gz
	./Patcher.sh ${OMAKE}
	( cd ${OMAKE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  export PREFIX=${PREFIX} && \
	  ${MAKE} all && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OMAKE} omake
distclean::
	rm -f ${OMAKE}-0.rc1.tar.gz
all: omake

# http://ergo.lri.fr/
ALTERGO=alt-ergo-0.94
${ALTERGO}.tar.gz:
	${WGET} http://ergo.lri.fr/http/$@
altergo: ${ALTERGO}.tar.gz ocamlgraph
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${ALTERGO}
	tar zxf ${ALTERGO}.tar.gz
	./Patcher.sh ${ALTERGO}
	( cd ${ALTERGO} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure -prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${ALTERGO} altergo
distclean::
	rm -f ${ALTERGO}.tar.gz
all: altergo

# http://www.seas.upenn.edu/~harmony/
BOOMERANG=boomerang-0.2
${BOOMERANG}-source.tar.gz:
	${WGET} http://www.seas.upenn.edu/~harmony/download/$@
boomerang: ${BOOMERANG}-source.tar.gz omake
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${BOOMERANG}
	tar zxf ${BOOMERANG}-source.tar.gz && mv boomerang-20090902 ${BOOMERANG}
	./Patcher.sh ${BOOMERANG}
	( cd ${BOOMERANG} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  omake )
	echo ${VERSION} >$@
clean::
	rm -rf ${BOOMERANG} boomerang
distclean::
	rm -f ${BOOMERANG}-source.tar.gz
all: boomerang

# https://github.com/yoriyuki/Camomile/wiki
CAMOMILE=camomile-0.8.4
${CAMOMILE}.tar.bz2:
	${WGET} https://github.com/downloads/yoriyuki/Camomile/$@
camomile: ${CAMOMILE}.tar.bz2
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CAMOMILE}
	tar xf ${CAMOMILE}.tar.bz2
	./Patcher.sh ${CAMOMILE}
	( cd ${CAMOMILE} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure -prefix ${PREFIX} && \
	  ${MAKE} && \
	  ocamlfind remove camomile && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CAMOMILE} camomile
distclean::
	rm -f ${CAMOMILE}.tar.bz2
all: camomile

# http://sanskrit.inria.fr/ZEN/
ZEN=zen_2.3.2
${ZEN}.tar.gz:
	${WGET} http://sanskrit.inria.fr/ZEN/$@
zen: ${ZEN}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${ZEN}
	tar zxf ${ZEN}.tar.gz && mv ZEN_* ${ZEN}
	./Patcher.sh ${ZEN}
	( cd ${ZEN} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} depend && \
	  ${MAKE} all && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${ZEN} zen
distclean::
	rm -f ${ZEN}.tar.gz
all: zen

# http://users-tima.imag.fr/vds/ouchet/index_fichiers/vsyml.html
VSYML=vsyml-2010-04-06
${VSYML}.tar.gz:
	${WGET} http://users-tima.imag.fr/vds/ouchet/vsyml/$@
vsyml: ${VSYML}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${VSYML}
	tar zxf ${VSYML}.tar.gz
	./Patcher.sh ${VSYML}
	( cd ${VSYML} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} )
	echo ${VERSION} >$@
clean::
	rm -rf ${VSYML} vsyml
distclean::
	rm -f ${VSYML}.tar.gz
all: vsyml

# http://projects.camlcity.org/projects/ocamlnet.html
OCAMLNET=ocamlnet-3.5.1
${OCAMLNET}.tar.gz:
	${WGET} http://download.camlcity.org/download/$@
ocamlnet: ${OCAMLNET}.tar.gz findlib pcre
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLNET}
	tar zxf ${OCAMLNET}.tar.gz
	./Patcher.sh ${OCAMLNET}
	( cd ${OCAMLNET} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure && \
	  ${MAKE} all && \
	  ${MAKE} opt && \
	  ocamlfind remove netsys && \
	  ocamlfind remove netshm && \
	  ocamlfind remove netstring && \
	  ocamlfind remove equeue && \
	  ocamlfind remove shell && \
	  ocamlfind remove rpc-generator && \
	  ocamlfind remove rpc-auth-local && \
	  ocamlfind remove rpc && \
	  ocamlfind remove pop && \
	  ocamlfind remove smtp && \
	  ocamlfind remove netclient && \
	  ocamlfind remove netcgi2 && \
	  ocamlfind remove netplex && \
	  ocamlfind remove netcgi2-plex && \
	  ocamlfind remove netcamlbox && \
	  ocamlfind remove netmulticore && \
	  ocamlfind remove netgssapi && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLNET} ocamlnet
distclean::
	rm -f ${OCAMLNET}.tar.gz
all: ocamlnet

# http://code.google.com/p/ocaml-extlib/
EXTLIB=extlib-1.5.2
${EXTLIB}.tar.gz:
	${WGET} http://ocaml-extlib.googlecode.com/files/$@
extlib: ${EXTLIB}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${EXTLIB}
	tar zxf ${EXTLIB}.tar.gz
	./Patcher.sh ${EXTLIB}
	( cd ${EXTLIB} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocamlfind remove extlib && \
	  ocaml install.ml -b -n -doc )
	echo ${VERSION} >$@
clean::
	rm -rf ${EXTLIB} extlib
distclean::
	rm -f ${EXTLIB}.tar.gz
all: extlib

# http://forge.ocamlcore.org/projects/ocaml-fileutils
FILEUTILS=ocaml-fileutils-0.4.4
${FILEUTILS}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/892/$@
fileutils: ${FILEUTILS}.tar.gz findlib ounit
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${FILEUTILS}
	tar xf ${FILEUTILS}.tar.gz
	./Patcher.sh ${FILEUTILS}
	( cd ${FILEUTILS} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ocamlfind remove fileutils && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${FILEUTILS} fileutils
distclean::
	rm -f ${FILEUTILS}.tar.gz
all: fileutils

# http://forge.ocamlcore.org/projects/odn
ODN=ocaml-data-notation-0.0.9
${ODN}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/881/$@
odn: ${ODN}.tar.gz findlib core ounit fileutils
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${ODN}
	tar zxf ${ODN}.tar.gz
	./Patcher.sh ${ODN} oasis-common.patch
	( cd ${ODN} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocaml setup.ml -configure && \
	  ocaml setup.ml -build && \
	  ocamlfind remove odn && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${ODN} odn
distclean::
	rm -f ${ODN}.tar.gz
all: odn

# http://forge.ocamlcore.org/projects/ocamlify
OCAMLIFY=ocamlify-0.0.1
${OCAMLIFY}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/379/$@
ocamlify: ${OCAMLIFY}.tar.gz findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLIFY}
	tar zxf ${OCAMLIFY}.tar.gz
	./Patcher.sh ${OCAMLIFY} oasis-common.patch
	( cd ${OCAMLIFY} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ocaml setup.ml -build && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLIFY} ocamlify
distclean::
	rm -f ${OCAMLIFY}.tar.gz
all: ocamlify

# http://forge.ocamlcore.org/projects/ocaml-expect
EXPECT=ocaml-expect-0.0.3
${EXPECT}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/894/$@
expect: ${EXPECT}.tar.gz findlib extlib pcre ounit
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${EXPECT}
	tar zxf ${EXPECT}.tar.gz
	./Patcher.sh ${EXPECT} oasis-common.patch
	( cd ${EXPECT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ocaml setup.ml -configure && \
	  ocaml setup.ml -build && \
	  ocamlfind remove expect && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${EXPECT} expect
distclean::
	rm -f ${EXPECT}.tar.gz
all: expect

# http://forge.ocamlcore.org/projects/ocamlmod/
OCAMLMOD=ocamlmod-0.0.3
${OCAMLMOD}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/856/$@
ocamlmod: ${OCAMLMOD}.tar.gz findlib fileutils pcre
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLMOD}
	tar zxf ${OCAMLMOD}.tar.gz
	./Patcher.sh ${OCAMLMOD}
	( cd ${OCAMLMOD} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLMOD} ocamlmod
distclean::
	rm -f ${OCAMLMOD}.tar.gz
all: ocamlmod

# http://forge.ocamlcore.org/projects/oasis
OASIS=oasis-0.3.0
${OASIS}.tar.gz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/918/$@
oasis: ${OASIS}.tar.gz findlib fileutils pcre extlib odn ocamlgraph ocamlify \
       ounit expect ocamlmod
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OASIS}
	tar zxf ${OASIS}.tar.gz
	./Patcher.sh ${OASIS} oasis-common.patch
	( cd ${OASIS} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ocaml setup.ml -build && \
	  ocamlfind remove oasis && \
	  ocamlfind remove userconf && \
	  ocamlfind remove plugin-loader && \
	  ocaml setup.ml -install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OASIS} oasis
distclean::
	rm -f ${OASIS}.tar.gz
all: oasis

# http://calendar.forge.ocamlcore.org/
CALENDAR=calendar-2.03.2
${CALENDAR}.tar.gz:
	${WGET} https://forge.ocamlcore.org/frs/download.php/915/$@
calendar: ${CALENDAR}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CALENDAR}
	tar zxf ${CALENDAR}.tar.gz
	./Patcher.sh ${CALENDAR}
	( cd ${CALENDAR} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CALENDAR} calendar
distclean::
	rm -f ${CALENDAR}.tar.gz
all: calendar

# http://gallium.inria.fr/camlimages/
CAMLIMAGES=camlimages-4.0.1
${CAMLIMAGES}.tar.gz:
	${WGET} https://bitbucket.org/camlspotter/camlimages/get/v4.0.1.tar.gz
	mv v4.0.1.tar.gz $@
camlimages: ${CAMLIMAGES}.tar.gz findlib omake lablgtk
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CAMLIMAGES}
	tar xf ${CAMLIMAGES}.tar.gz
	mv camlspotter-camlimages-c803efa9d5d3 ${CAMLIMAGES}
	mv ${CAMLIMAGES}/doc/old/* ${CAMLIMAGES}/doc/
	./Patcher.sh ${CAMLIMAGES}
	( cd ${CAMLIMAGES} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  omake && \
	  ocamlfind remove camlimages && \
	  omake install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CAMLIMAGES} camlimages
distclean::
	rm -f ${CAMLIMAGES}.tar.gz
all: camlimages

# http://advi.inria.fr/
ADVI=advi-1.10.2
${ADVI}.tar.gz:
	${WGET} http://advi.inria.fr/$@
advi: ${ADVI}.tar.gz findlib camlimages
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${ADVI}
	tar zxf ${ADVI}.tar.gz
	./Patcher.sh ${ADVI}
	( cd ${ADVI} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${ADVI} advi
distclean::
	rm -f ${ADVI}.tar.gz
all: advi

# http://forge.ocamlcore.org/projects/camldbm
DBM=camldbm-1.0
${DBM}.tgz:
	${WGET} http://forge.ocamlcore.org/frs/download.php/728/$@
dbm: ${DBM}.tgz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${DBM}
	tar zxf ${DBM}.tgz
	./Patcher.sh ${DBM}
	( cd ${DBM} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${DBM} dbm
distclean::
	rm -f ${DBM}.tgz
all: dbm

# http://ocsigen.org/
OCSIGEN=ocsigen-bundle-2.2.2
${OCSIGEN}.tar.gz:
	${WGET} http://ocsigen.org/download/$@
ocsigen: ${OCSIGEN}.tar.gz findlib lwt obrowser pcre ocamlnet ocamlssl \
         sqlite camlzip cryptokit calendar dbm
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCSIGEN}
	tar zxf ${OCSIGEN}.tar.gz
	./Patcher.sh ${OCSIGEN}
	( cd ${OCSIGEN} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  export OCSIGEN_USER=${USER}; export OCSIGEN_GROUP=everyone && \
	  ./configure --prefix=${PREFIX} && \
	  ${MAKE} && \
	  rm -rf ${PREFIX}/lib/ocaml/ocsigenserver/extensions && \
	  ocamlfind remove -destdir ${PREFIX}/lib/ocaml deriving-ocsigen && \
	  ocamlfind remove -destdir ${PREFIX}/lib/ocaml js_of_ocaml && \
	  ocamlfind remove -destdir ${PREFIX}/lib/ocaml ocsigenserver && \
	  ocamlfind remove -destdir ${PREFIX}/lib/ocaml tyxml && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCSIGEN} ocsigen
distclean::
	rm -f ${OCSIGEN}.tar.gz
all: ocsigen

# http://mldonkey.sourceforge.net/
MLDONKEY=mldonkey-3.1.2
${MLDONKEY}.tar.bz2:
	${WGET} http://freefr.dl.sourceforge.net/project/mldonkey/mldonkey/3.1.2/$@
mldonkey: ${MLDONKEY}.tar.bz2 lablgtk
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${MLDONKEY}
	tar zxf ${MLDONKEY}.tar.bz2
	./Patcher.sh ${MLDONKEY}
	( cd ${MLDONKEY} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure && \
	  ${MAKE} )
	echo ${VERSION} >$@
clean::
	rm -rf ${MLDONKEY} mldonkey
distclean::
	rm -f ${MLDONKEY}.tar.bz2
all: mldonkey

# http://mjambon.com/ocamlscript.html
OCAMLSCRIPT=ocamlscript-2.0.2
${OCAMLSCRIPT}.tar.bz2:
	${WGET} http://mjambon.com/$@
ocamlscript: ${OCAMLSCRIPT}.tar.bz2 findlib
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${OCAMLSCRIPT}
	tar jxf ${OCAMLSCRIPT}.tar.bz2
	./Patcher.sh ${OCAMLSCRIPT}
	( cd ${OCAMLSCRIPT} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ${MAKE} && \
	  ocamlfind remove ocamlscript && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${OCAMLSCRIPT} ocamlscript
distclean::
	rm -f ${OCAMLSCRIPT}.tar.bz2
all: ocamlscript

# http://pauillac.inria.fr/~ddr/camlp5/
CAMLP5=camlp5-6.06
${CAMLP5}.tgz:
	${WGET} http://pauillac.inria.fr/~ddr/camlp5/distrib/src/$@
camlp5: ${CAMLP5}.tgz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${CAMLP5}
	tar zxf ${CAMLP5}.tgz
	./Patcher.sh ${CAMLP5}
	( cd ${CAMLP5} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure --transitional && \
	  ${MAKE} world.opt && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${CAMLP5} camlp5
distclean::
	rm -f ${CAMLP5}.tgz
all: camlp5

# http://opensource.geneanet.org/projects/geneweb
GENEWEB=gw-6.05-src
${GENEWEB}.tgz:
	${WGET} http://opensource.geneanet.org/attachments/download/190/$@
geneweb: ${GENEWEB}.tgz camlp5
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${GENEWEB}
	tar zxf ${GENEWEB}.tgz
	./Patcher.sh ${GENEWEB}
	( cd ${GENEWEB} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure && \
	  ${MAKE} )
	echo ${VERSION} >$@
clean::
	rm -rf ${GENEWEB} geneweb
distclean::
	rm -f ${GENEWEB}.tgz
all: geneweb

# http://coq.inria.fr/download
COQ=coq-8.3pl4
${COQ}.tar.gz:
	${WGET} http://coq.inria.fr/distrib/V8.3pl4/files/$@
coq: ${COQ}.tar.gz camlp5
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${COQ}
	tar zxf ${COQ}.tar.gz
	./Patcher.sh ${COQ}
	( cd ${COQ} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure -prefix ${PREFIX} -with-doc no && \
	  ${MAKE} world && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${COQ} coq
distclean::
	rm -f ${COQ}.tar.gz
#all: coq

# http://frama-c.com/
FRAMAC=frama-c-Nitrogen-20111001
${FRAMAC}.tar.gz:
	${WGET} http://frama-c.com/download/$@
framac: ${FRAMAC}.tar.gz lablgtk ocamlgraph altergo coq
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${FRAMAC}
	tar zxf ${FRAMAC}.tar.gz
	./Patcher.sh ${FRAMAC}
	( cd ${FRAMAC} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  ./configure --enable-verbosemake --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ${MAKE} oracles && \
	  ${MAKE} install )
	echo ${VERSION} >$@
clean::
	rm -rf ${FRAMAC} framac
distclean::
	rm -f ${FRAMAC}.tar.gz
#all: framac

##################################################################
### Template for new entries
##################################################################

FOO=
${FOO}.tar.gz:
	${WGET} http://foo.bar.com/.../$@
foo: ${FOO}.tar.gz
	printf "%s " "$@" >/dev/tty
	test -d ${PREFIX}
	rm -rf ${FOO}
	tar zxf ${FOO}.tar.gz
	./Patcher.sh ${FOO}
	( cd ${FOO} && \
	  export PATH=${PREFIX}/bin:$$PATH && \
	  sh ./configure --prefix ${PREFIX} && \
	  ${MAKE} && \
	  ocamlfind remove foo \
	  ${MAKE} install )
	echo ${VERSION} >$@
xxclean::
	rm -rf ${FOO} foo
xxdistclean::
	rm -f ${FOO}.tar.gz
xxall: foo

##################################################################

.PHONY: clean

.PHONY: distclean
distclean::
	${MAKE} clean

.PHONY: all
all:
	echo >/dev/tty
