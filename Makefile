top_srcdir = .
WARNS      = -w -40+27
override OCAMLOPTFLAGS += $(INCS) $(WARNS) -g -annot -I $(top_srcdir) -O2
override OCAMLFLAGS    += $(INCS) $(WARNS) -g -annot -I $(top_srcdir)

PPP_SOURCES = PPP.ml PPP_prettify.ml PPP_OCaml.ml PPP_JSON.ml PPP_CSV.ml
SOURCES = $(PPP_SOURCES) PPP_Unix.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot
.PHONY: clean distclean all check dep install uninstall reinstall

all: .depend \
     ppx_ppp.opt ppx_test.opt ppx_test_source.ml \
     PPP.cma PPP.cmxa PPP-unix.cma PPP-unix.cmxa \
     pretty_please

%.cmo %.annot: %.ml
	ocamlfind ocamlc   $(OCAMLFLAGS) -package stdint -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint -c $<

%.cmxs: %.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint -o $@ -shared $<

PPP.cmxa: $(PPP_SOURCES:.ml=.cmx)
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package stdint $^ -o $@

PPP.cma: $(PPP_SOURCES:.ml=.cmo)
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package stdint -custom $^ -o $@

PPP-unix.cmxa: PPP_Unix.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package unix $^ -o $@

PPP-unix.cma: PPP_Unix.cmo
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package unix -custom $^ -o $@

# PPX

ppx_ppp.opt: ppx_ppp.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg -package compiler-libs,ppx_tools,stdint,unix $^ -o $@

ppx_test.cmx: PPP.cmxa PPP-unix.cmxa ppx_ppp.opt ppx_test.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint,unix -ppx ./ppx_ppp.opt PPP.cmxa PPP-unix.cmxa -c ppx_test.ml -o $@

ppx_test.opt: PPP.cmxa PPP-unix.cmxa ppx_test.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg -package stdint,unix PPP.cmxa PPP-unix.cmxa ppx_test.cmx -o $@

ppx_test_source.ml: ppx_test.ml PPP.cmxa PPP-unix.cmxa ppx_ppp.opt
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint,unix -ppx ./ppx_ppp.opt PPP.cmxa PPP-unix.cmxa -c $< -dsource 2> $@

# Pretty Please
pretty_please: PPP_prettify.cmx pretty_please.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg $^ -o $@

clean:
	$(RM) *.cm[iox] *.cmxs *.a *.s *.o .depend *.annot all_tests.*

distclean: clean
	$(RM) *.cma *.cmxa *.opt oUnit-anon.cache qtest.targets.log

# Tests

all_tests.ml: $(SOURCES)
	qtest --shuffle -o $@ extract $^

all_tests.opt: $(SOURCES:.ml=.cmx) all_tests.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -o $@ -package stdint -package qcheck -linkpkg $^

check: all_tests.opt ppx_test.opt
	@./all_tests.opt || echo "FAILURE"
	@./ppx_test.opt > ppx_test.actual &&\
	 diff ppx_test.expected ppx_test.actual &&\
	 $(RM) ppx_test.actual

# Installation

INSTALLED_LIB = \
	META PPP.cmxa PPP.cma PPP.a \
	PPP.cmx PPP.cmi PPP.cmo \
	PPP-unix.cmxa PPP-unix.cma PPP-unix.a \
	PPP_Unix.cmx PPP_Unix.cmi PPP_Unix.cmo \
	PPP_OCaml.cmx PPP_OCaml.cmi PPP_OCaml.cmo \
	PPP_JSON.cmx PPP_JSON.cmi PPP_JSON.cmo \
	PPP_CSV.cmx PPP_CSV.cmi PPP_CSV.cmo \
	PPP_prettify.cmx PPP_prettify.cmi PPP_prettify.cmo \
	ppx_ppp.opt
INSTALLED_BIN = pretty_please
INSTALLED = $(INSTALLED_LIB) $(INSTALLED_BIN)

bin_dir ?= /usr/bin/

install: $(INSTALLED)
	ocamlfind install ppp $(INSTALLED_LIB)
	install -d $(prefix)$(bin_dir)
	install $(INSTALLED_BIN) $(prefix)$(bin_dir)/

uninstall:
	$(RM) $(bin_dir)/$(INSTALLED_BIN)
	ocamlfind remove ppp

reinstall: uninstall install

# Dependencies

dep:
	$(RM) .depend
	$(MAKE) .depend

.depend: $(SOURCES)
	ocamlfind ocamldep -package stdint $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@

-include .depend
