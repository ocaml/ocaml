CSC=csc

default: prepare bytecode bytecode-dll native native-dll

prepare:
	@$(OCAMLC) -c plugin.ml
	@$(OCAMLOPT) -o plugin.cmxs -shared plugin.ml

bytecode:
	@printf " ... testing 'bytecode':"
	@if [ ! `which $(CSC) > /dev/null` ]; then \
	  echo " => passed"; \
	else \
	  $(OCAMLC) -output-obj -o main.dll dynlink.cma main.ml entry.c; \
	  $(CSC) /out:main.exe main.cs; \
	  ./main.exe > bytecode.result; \
	  diff -q bytecode.reference bytecode.result > /dev/null && echo " => passed" || echo " => failed"; \
	fi

bytecode-dll:
	@printf " ... testing 'bytecode-dll':"
	@if [ ! `which $(CSC) > /dev/null` ]; then \
	  echo " => passed"; \
	else \
	  $(OCAMLC) -output-obj -o main_obj.$(O) dynlink.cma entry.c main.ml; \
	  $(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) ../../byterun/libcamlrun.$(A)  $(BYTECCLIBS) -v; \
	  $(CSC) /out:main.exe main.cs; \
	  ./main.exe > bytecode.result; \
	  diff -q bytecode.reference bytecode.result > /dev/null && echo " => passed" || echo " => failed"; \
	fi

native:
	@printf " ... testing 'native':"
	@if [ ! `which $(CSC) > /dev/null` ]; then \
	  echo " => passed"; \
	else \
	  $(OCAMLOPT) -output-obj -o main.dll dynlink.cmxa entry.c main.ml; \
	  $(CSC) /out:main.exe main.cs; \
	  ./main.exe > native.result; \
	  diff -q native.reference native.result > /dev/null && echo " => passed" || echo " => failed"; \
	fi

native-dll:
	@printf " ... testing 'native-dll':"
	@if [ ! `which $(CSC) > /dev/null` ]; then \
	  echo " => passed"; \
	else \
	  $(OCAMLOPT) -output-obj -o main_obj.$(O) dynlink.cmxa entry.c main.ml; \
	  $(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) ../../asmrun/libasmrun.lib -v; \
	  $(CSC) /out:main.exe main.cs; \
	  ./main.exe > native.result; \
	  diff -q native.reference native.result > /dev/null && echo " => passed" || echo " => failed"; \
	fi

clean: defaultclean
	@rm -f *.result *.exe *.dll

include ../../makefiles/Makefile.common
