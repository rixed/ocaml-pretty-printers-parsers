top_srcdir = .
WARNS      = Ael-31-41-44-45-48
override OCAMLOPTFLAGS += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir) -O2
override OCAMLFLAGS    += $(INCS) -w $(WARNS) -g -annot -I $(top_srcdir)

PPP_SOURCES = PPP.ml PPP_OCaml.ml PPP_JSON.ml PPP_CSV.ml
SOURCES = $(PPP_SOURCES) PPP_block.ml PPP_lwt.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot
.PHONY: clean distclean all check dep install uninstall reinstall

PACKAGES = lwt.unix stdint

all: .depend \
     PPP_block.cmxa PPP_block.cma \
		 PPP_lwt.cmxa PPP_lwt.cma \
		 ppx_ppp.opt ppx_test.opt

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

PPP_block.cmxa: PPP_block.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package "$(PACKAGES)" $^ -o $@

PPP_block.cma: PPP_block.cmo
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package "$(PACKAGES)" -custom $^ -o $@

PPP_lwt.cmxa: PPP_lwt.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -a -package "$(PACKAGES)" $^ -o $@

PPP_lwt.cma: PPP_lwt.cmo
	ocamlfind ocamlc   $(OCAMLFLAGS) -a -linkpkg -package "$(PACKAGES)" -custom $^ -o $@

# PPX

ppx_ppp.opt: ppx_ppp.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -w -27 -linkpkg -package compiler-libs,ppx_tools,stdint $^ -o $@

ppx_test.cmx: PPP.cmxa PPP_block.cmxa ppx_ppp.opt ppx_test.ml
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -package stdint -ppx ./ppx_ppp.opt PPP.cmxa PPP_block.cmxa -c ppx_test.ml -o $@

ppx_test.opt: PPP.cmxa PPP_block.cmxa ppx_test.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) -linkpkg -package stdint PPP.cmxa PPP_block.cmxa ppx_test.cmx -o $@

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
	PPP_block.cmxa PPP_block.cma PPP_block.a \
	PPP_block.cmx PPP_block.cmi PPP_block.cmo \
	PPP_lwt.cmxa PPP_lwt.cma PPP_lwt.a \
	PPP_lwt.cmx PPP_lwt.cmi PPP_lwt.cmo \
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
