OCAMLC=../ocaml-namespaces/ocamlc
OCAMLOPT=../ocaml-namespaces/ocamlopt


all: byte

byte: test1.byte
opt: test1.opt

# example of namespace usage
test1.byte:
	$(OCAMLC) -debug env,ns -c -namespace - a/x1.ml
	$(OCAMLC) -c -namespace - a/x2.ml
	$(OCAMLC) -c -namespace - b/x1.ml
	$(OCAMLC) -c -namespace - b/x2.ml
	(cd b; ../$(OCAMLC) -c -namespace B x2.ml)
	$(OCAMLC) -c -namespace - y.ml
	$(OCAMLC) -o test1_byte a/x1.cmo a/x2.cmo b/x1.cmo b/x2.cmo y.cmo


# example of namespace usage
test1.opt:
	$(OCAMLOPT) -debug env,ns -c -namespace - a/x1.ml
	$(OCAMLOPT) -c -namespace - a/x2.ml
	$(OCAMLOPT) -c -namespace - b/x1.ml
	$(OCAMLOPT) -c -namespace - b/x2.ml
	(cd b; ../$(OCAMLOPT) -c -namespace B x2.ml)
	$(OCAMLOPT) -c -namespace - y.ml
	$(OCAMLOPT) -o test1_opt a/x1.cmx a/x2.cmx b/x1.cmx b/x2.cmx y.cmx

# These examples do not work, for good reasons.
test2.byte:
	$(OCAMLC) -c b/x.ml
	$(OCAMLC) -c -namespace - y.ml
	$(OCAMLC) -o test2_byte a/x.cmo b/x.cmo y.cmo

test3.byte:
	$(OCAMLC) -c -I a z.ml
	$(OCAMLC) -o test3_byte a/x.cmo z.cmo


test2.opt:
	$(OCAMLOPT) -c -namespace - a/x.ml
	$(OCAMLOPT) -c -namespace - b/x.ml
	$(OCAMLOPT) -c -namespace - y.ml
	$(OCAMLOPT) -o test1.opt a/x.cmx b/x.cmx y.cmx
	$(OCAMLOPT) -c -I a z.ml
	$(OCAMLOPT) -o test2.opt a/x.cmx z.cmx
