OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

SOURCES = main.ml
RESULT = jamler
PACKS = lwt lwt.unix lwt.syntax
OCAMLFLAGS = -syntax camlp4o -w A
ANNOTATE = yes

all:	nc

-include $(OCAMLMAKEFILE)

OCAML_DEP_PACKAGES := -syntax camlp4o -package lwt.syntax
