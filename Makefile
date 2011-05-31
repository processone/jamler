OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

SOURCES = expatwrap.c expat.ml \
	  stringprepwrap.c stringprep.mli \
	  xml.ml main.ml
RESULT = jamler
PACKS = lwt lwt.unix lwt.syntax
OCAMLFLAGS = -syntax camlp4o -w A
ANNOTATE = yes

CFLAGS = -Wall
LDFLAGS = -lexpat

all:	nc

-include $(OCAMLMAKEFILE)

OCAML_DEP_PACKAGES := -syntax camlp4o -package lwt.syntax
