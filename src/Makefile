OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

SOURCES = expatwrap.c expat.ml \
	  stringprepwrap.c stringprep.mli \
	  xml.ml process.mli process.ml tcp.ml \
	  jamler_receiver.ml gen_server.ml jlib.mli jlib.ml \
	  jamler_hooks.mli jamler_hooks.ml \
	  jamler_auth.mli jamler_auth.ml \
	  jamler_sasl.ml jamler_sasl_plain.ml jamler_sasl_digest.ml \
	  jamler_router.mli jamler_router.ml \
	  jamler_gen_iq_handler.mli jamler_gen_iq_handler.ml \
	  jamler_sm.mli jamler_sm.ml \
	  jamler_local.ml \
	  jamler_c2s.ml \
	  gen_roster.ml \
	  mod_roster.ml \
	  sql.mli sql.ml main.ml
RESULT = jamler
PACKS = lwt lwt.unix camlp4.macro lwt.syntax cryptokit pgocaml
OCAMLFLAGS = -syntax camlp4o -ppopt jlib_pp.cmo -ppopt pa_sql.cmo -w A -g
ANNOTATE = yes

PRE_TARGETS = jlib_pp.cmo pa_sql.cmo


CFLAGS = -Wall -g -O2
LDFLAGS = -lexpat -g

all:	nc

jlib_pp.cmo:	jlib_pp.ml
	ocamlfind ocamlc -package camlp4.quotations.o -c -syntax camlp4o \
		-w A jlib_pp.ml

pa_sql.cmo:	pa_sql.ml
	ocamlfind ocamlc -package camlp4.quotations.o -c -syntax camlp4o \
		-w A pa_sql.ml

-include $(OCAMLMAKEFILE)

OCAML_DEP_PACKAGES := -syntax camlp4o -ppopt jlib_pp.cmo -ppopt pa_sql.cmo -package lwt.syntax
