EXE=test1.native test2.native test3.native test4.native test5.native \
		test6.native test7.native test8.native test9.native test10.native \
		test11.native test12.native test13.native test14.native test15.native \
		test16.native test17.native test18.native test19.native test20.native
DEBUG=false
FLAGS=
OCAMLOPT=../ocamlopt.opt

ifeq ($(DEBUG),true)
	FLAGS=-g -runtime-variant d -S
endif

all: $(EXE)

test6.native: test6.ml test6.mli test6.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test6.c
	mv test6.o test6_c.o
	$(OCAMLOPT) $(FLAGS) -c test6.mli
	$(OCAMLOPT) $(FLAGS) -o test6.native test6_c.o test6.ml -linkall

test7.native: test7.ml test7.mli test7.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test7.c
	mv test7.o test7_c.o
	$(OCAMLOPT) $(FLAGS) -c test7.mli
	$(OCAMLOPT) $(FLAGS) -o test7.native test7_c.o test7.ml -linkall

test8.native:test8.ml test8.mli test8.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test8.c
	mv test8.o test8_c.o
	$(OCAMLOPT) $(FLAGS) -c test8.mli
	$(OCAMLOPT) $(FLAGS) -o test8.native test8_c.o test8.ml -linkall

test9.native: test9.ml test9.mli test9.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test9.c
	mv test9.o test9_c.o
	$(OCAMLOPT) $(FLAGS) -c test9.mli
	$(OCAMLOPT) $(FLAGS) -o test9.native test9_c.o test9.ml -linkall

test16.native: test16.ml test16.mli test16.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test16.c
	mv test16.o test16_c.o
	$(OCAMLOPT) $(FLAGS) -c test16.mli
	$(OCAMLOPT) $(FLAGS) -o test16.native test16_c.o test16.ml -linkall

test17.native: test17.ml test17.mli test17.c
	$(OCAMLOPT) $(FLAGS) -ccopt -c test17.c
	mv test17.o test17_c.o
	$(OCAMLOPT) $(FLAGS) -c test17.mli
	$(OCAMLOPT) $(FLAGS) -o test17.native test17_c.o test17.ml -linkall

%.native: %.ml
	$(OCAMLOPT) $(FLAGS) -o $@ $<

.PHONY: clean test expected

clean:
	rm -f *.cmo *.cmi *.cmx *.s *~ *.o op *.native seen.out dump a.out

test: all
	rm -f seen.out
	for p in $(EXE); do echo "Running $$p" && bash -c `pwd`/$$p >>seen.out 2>&1; done || true
	diff expected.out seen.out && echo "SUCCESS!"

expected.out: all
	rm -f expected.out
	for p in $(EXE); do bash -c `pwd`/$$p >>expected.out 2>&1; done
