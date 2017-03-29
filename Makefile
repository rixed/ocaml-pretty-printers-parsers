top_srcdir = .
OCAMLC     = ocamlfind ocamlc
OCAMLOPT   = ocamlfind ocamlopt
OCAMLDEP   = ocamlfind ocamldep
QTEST      = qtest
WARNS      = Ael-31-41-44-45-48
override OCAMLOPTFLAGS += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir) -O2
override OCAMLFLAGS    += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir)

SOURCES = PPP.ml PPP_OCaml.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot
.PHONY: clean distclean all check dep install uninstall reinstall

PACKAGES = 

all: .depend PPP.cmxa PPP.cma

%.cmo %.annot: %.ml
	ocamlfind ocamlc   $(OCAMLFLAGS) -package "$(PACKAGES)" -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -c $<

%.cmxs: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -o $@ -shared $<

PPP.cmxa: PPP.cmx PPP_OCaml.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package "$(PACKAGES)" $^ -o $@

PPP.cma: PPP.cmo PPP_OCaml.cmo
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package "$(PACKAGES)" -custom $^ -o $@

clean:
	$(RM) *.cm[iox] *.cmxs *.a *.s *.o .depend *.annot all_tests.*

distclean: clean
	$(RM) *.cma *.cmxa oUnit-anon.cache qtest.targets.log

# Tests

all_tests.ml: $(SOURCES)
	$(QTEST) --shuffle -o $@ extract $^

all_tests.opt: PPP.cmx PPP_OCaml.cmx all_tests.ml
	$(OCAMLOPT) -o $@ $(SYNTAX) -package "$(PACKAGES)" -package qcheck -linkpkg $(OCAMLOPTFLAGS) $^

check: all_tests.opt
	@./all_tests.opt || echo "FAILURE"

# Installation

INSTALLED = \
	META PPP.cmxa PPP.cma PPP.a \
	PPP.cmx PPP.cmi PPP.cmo \
	PPP_OCaml.cmx PPP_OCaml.cmi PPP_OCaml.cmo

install: $(INSTALLED)
	ocamlfind install ppp $^

uninstall:
	ocamlfind remove ppp

reinstall: uninstall install

# Dependencies

dep:
	$(RM) .depend
	$(MAKE) .depend

.depend: $(SOURCES)
	$(OCAMLDEP) -package "$(PACKAGES)" $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@

-include .depend
