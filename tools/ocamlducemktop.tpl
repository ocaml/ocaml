#!/bin/sh

TO_EXPUNGE="\
 asttypes btype bytegen bytelibrarian bytelink bytepackager \
 bytesections ccomp  clflags compile config consistbl ctype \
 datarepr dll emitcode env errors genprintval ident includeclass \
 includecore includemod instruct lambda lexer linenum location \
 longident matching meta misc mtype opcodes oprint parmatch \
 parse parser parsetree path pparse predef primitive printast \
 printinstr printlambda printtyp ratio runtimedef simplif std_exit \
 stypes subst switch symtable syntaxerr tbl terminfo topmain topstart \
 trace translclass translcore translext translmod translobj typeclass \
 typecore typedecl typedtree typeext typemod typeopt types typetexp \
 unused_var warnings xparser"

%%OCAMLC_DIR%%/ocamlc -linkall -I %%LIBDIR%% nums.cma toplevelducelib.cma "$@" topstart.cmo

s=a.out
while : ; do
  case "$1" in
    "") break;;
    -o)
        s=$2; shift;;
  esac
  shift
done

mv $s $s.tmp

%%EXPUNGEDUCE_DIR%%/expungeduce $s.tmp $s -v $TO_EXPUNGE

rm $s.tmp