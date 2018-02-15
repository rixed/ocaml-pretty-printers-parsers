top_srcdir = .
WARNS      = -w -40
override OCAMLOPTFLAGS += $(INCS) $(WARNS) -g -annot -I $(top_srcdir) -O2
override OCAMLFLAGS    += $(INCS) $(WARNS) -g -annot -I $(top_srcdir)

PPP_SOURCES = PPP.ml PPP_prettify.ml PPP_OCaml.ml PPP_JSON.ml PPP_CSV.ml
SOURCES = $(PPP_SOURCES)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot
.PHONY: clean distclean all check dep install uninstall reinstall

PACKAGES = stdint

all: .depend \
		 ppx_ppp.opt ppx_test.opt ppx_test_source.ml

%.cmo %.annot: %.ml
	ocamlfind ocamlc   $(OCAMLFLAGS) -package "$(PACKAGES)" -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -c $<

%.cmxs: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -o $@ -shared $<

PPP.cmxa: $(PPP_SOURCES:.ml=.cmx)
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package "$(PACKAGES)" $^ -o $@

PPP.cma: $(PPP_SOURCES:.ml=.cmo)
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package "$(PACKAGES)" -custom $^ -o $@

# PPX

ppx_ppp.opt: ppx_ppp.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg -package compiler-libs,ppx_tools,stdint $^ -o $@

ppx_test.cmx: PPP.cmxa ppx_ppp.opt ppx_test.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint -ppx ./ppx_ppp.opt PPP.cmxa -c ppx_test.ml -o $@

ppx_test.opt: PPP.cmxa ppx_test.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg -package stdint PPP.cmxa ppx_test.cmx -o $@

ppx_test_source.ml: ppx_test.ml ppx_ppp.opt
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint -ppx ./ppx_ppp.opt PPP.cmxa -c $< -dsource 2> $@

clean:
	$(RM) *.cm[iox] *.cmxs *.a *.s *.o .depend *.annot all_tests.*

distclean: clean
	$(RM) *.cma *.cmxa *.opt oUnit-anon.cache qtest.targets.log

# Tests

all_tests.ml: $(SOURCES)
	qtest --shuffle -o $@ extract $^

all_tests.opt: $(SOURCES:.ml=.cmx) all_tests.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -o $@ -package "$(PACKAGES)" -package qcheck -linkpkg $^

check: all_tests.opt ppx_test.opt
	@./all_tests.opt || echo "FAILURE"
	@./ppx_test.opt > ppx_test.actual &&\
	 diff ppx_test.expected ppx_test.actual &&\
	 $(RM) ppx_test.actual

# Installation

INSTALLED = \
	META PPP.cmxa PPP.cma PPP.a \
	PPP.cmx PPP.cmi PPP.cmo \
	PPP_OCaml.cmx PPP_OCaml.cmi PPP_OCaml.cmo \
	PPP_JSON.cmx PPP_JSON.cmi PPP_JSON.cmo \
	PPP_CSV.cmx PPP_CSV.cmi PPP_CSV.cmo \
	PPP_prettify.cmx PPP_prettify.cmi PPP_prettify.cmo \
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
