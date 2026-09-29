#CC = cl /MD
#O = obj
#CHAIN = msvc

#CC = gcc
#O = o
#CHAIN=cygwin

#CC = gcc -mno-cygwin
#O = o
#CHAIN = mingw

RUN=
FLEXLINK = $(RUN) ../flexlink.exe -chain $(CHAIN) -merge-manifest $(EXTRA_OPTS)

.PHONY: demo
demo: dump.exe plug1.dll plug2.dll
	$(RUN) ./dump.exe plug1.dll plug2.dll

dump.exe: dump.$(O)
	$(FLEXLINK) -exe -o dump.exe dump.$(O)

dump.$(O): dump.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -I.. -c dump.c

plug1.$(O): plug1.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PLUG1_CFLAGS) -c plug1.c

plug2.$(O): plug2.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PLUG2_CFLAGS) -c plug2.c

plug1.dll: plug1.$(O)
	$(FLEXLINK) -o plug1.dll plug1.$(O)

plug2.dll: plug2.$(O)
	$(FLEXLINK) -o plug2.dll plug2.$(O)

plug12.dll: plug1.$(O) plug2.$(O)
	$(FLEXLINK) -o plug12.dll plug1.$(O) plug2.$(O)


clean:
	rm -f *.o *.obj *.dll *.exe *~ *.manifest
