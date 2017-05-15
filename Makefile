top_srcdir = .
WARNS      = Ael-31-41-44-45-48
override OCAMLOPTFLAGS += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir) -O2
override OCAMLFLAGS    += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir)

SOURCES = PPP.ml PPP_OCaml.ml PPP_JSON.ml PPP_CSV.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot
.PHONY: clean distclean all check dep install uninstall reinstall

PACKAGES = 

all: .depend PPP.cmxa PPP.cma ppx_ppp.opt ppx_test.opt

%.cmo %.annot: %.ml
	ocamlfind ocamlc   $(OCAMLFLAGS) -package "$(PACKAGES)" -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -c $<

%.cmxs: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -o $@ -shared $<

PPP.cmxa: $(SOURCES:.ml=.cmx)
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package "$(PACKAGES)" $^ -o $@

PPP.cma: $(SOURCES:.ml=.cmo)
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package "$(PACKAGES)" -custom $^ -o $@

# PPX

ppx_ppp.opt: ppx_ppp.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -w -27 -package compiler-libs,ppx_tools.metaquot -linkpkg $^ -o $@

ppx_test.opt: PPP.cmxa ppx_ppp.opt ppx_test.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -ppx ./ppx_ppp.opt -package "$(PACKAGES)" PPP.cmxa ppx_test.ml -o $@

clean:
	$(RM) *.cm[iox] *.cmxs *.a *.s *.o .depend *.annot all_tests.*

distclean: clean
	$(RM) *.cma *.cmxa *.opt oUnit-anon.cache qtest.targets.log

# Tests

all_tests.ml: $(SOURCES)
	qtest --shuffle -o $@ extract $^

all_tests.opt: $(SOURCES:.ml=.cmx) all_tests.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -o $@ -package "$(PACKAGES)" -package qcheck -linkpkg $^

check: all_tests.opt
	@./all_tests.opt || echo "FAILURE"

# Installation

INSTALLED = \
	META PPP.cmxa PPP.cma PPP.a \
	PPP.cmx PPP.cmi PPP.cmo \
	PPP_OCaml.cmx PPP_OCaml.cmi PPP_OCaml.cmo \
	PPP_JSON.cmx PPP_JSON.cmi PPP_JSON.cmo \
	PPP_CSV.cmx PPP_CSV.cmi PPP_CSV.cmo \
	ppx_ppp.opt

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
	ocamlfind ocamldep -package "$(PACKAGES)" $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@

-include .depend
