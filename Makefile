OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

SOURCES = expatwrap.c expat.ml \
	  stringprepwrap.c stringprep.mli \
	  xml.ml main.ml
RESULT = jamler
PACKS = lwt lwt.unix camlp4.macro lwt.syntax cryptokit
OCAMLFLAGS = -syntax camlp4o -ppopt jlib_pp.cmo -w A -g
ANNOTATE = yes

PRE_TARGETS = jlib_pp.cmo


CFLAGS = -Wall -g
LDFLAGS = -lexpat -g

all:	nc

jlib_pp.cmo:	jlib_pp.ml
	ocamlfind ocamlc -package camlp4.quotations.o -c -syntax camlp4o \
		-w A jlib_pp.ml

-include $(OCAMLMAKEFILE)

OCAML_DEP_PACKAGES := -syntax camlp4o -ppopt jlib_pp.cmo -package lwt.syntax
