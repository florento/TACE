
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa

.PHONY: all lib libopt exec execopt clean tar doc html dep tags

# file names
EXEC_NAME=tace
TESTER_NAME=start-test
#LIB_NAME=tace.cma
#LIB_OPT_NAME=tace.cmxa
ARCHIVE_NAME=TACE.tar

# commands
#CAMLC=ocamlc -w A -i -g
# with asserts & debug
CAMLC=ocamlc -w A -g
# without asserts & debug
#CAMLC=ocamlc -w A -noassert
CAMLLIB=ocamlc -a
CAMLCOPT=ocamlopt 
CAMLLIBOPT=ocamlopt -a
MKDEP=ocamldep
Y2TEX=scripts/y2tex
CAMLYACC=ocamlyacc -v
CAMLLEX=ocamllex
MKHTMLDOC=ocamldoc -v -html -colorize-code -t "TACE" -d html
MKTODOLIST=ocamldoc -g /usr/local/lib/ocaml/ocamldoc/custom/odoc_tdl.cma -v -html -colorize-code -t "TACE" -d todol


# parser and lexer
PARSER=  #parser.ml
PARSERY= #parser.mly
PARSERI= #parser.mli
LEXER=  #lexer.ml
LEXERL= #lexer.mll
LEXERI= #lexer.mli

# ocaml source modules
MMODULES=trace.ml util.ml chrono.ml location.ml parsing.ml myqueue.ml \
 symbol.ml variable.ml renaming.ml \
 term.ml \
 substitution.ml unification.ml matching.ml \
 equation.ml trs.ml \
 rewriting.ml narrowing.ml \
 atom.ml eblocks.ml \
 clausetype.ml clause.ml \
 rparamodulation.ml lparamodulation.ml solving.ml \
 tautology.ml elim.ml subsumption.ml resolution.ml esplitting.ml \
 instruction.ml clauseset.ml \
 interpreter.ml history.ml saturation.ml \
 input.ml output.ml


# ocaml source modules for lexical/syntaxic analysis, made with ocamlex, ocamlyacc
PMODULES=$(PARSER) $(LEXER) 

# test source files
TEST=$(wildcard test_*.ml)

# ocaml source main
MAIN=tace.ml

# ocaml source main tester
TESTMAIN=teststart.ml

# bytecode objects
OBJECTS=${MMODULES:%.ml=%.cmo} ${PMODULES:%.ml=%.cmo} 

# bytecode object for test
TESTOBJ=${TEST:%.ml=%.cmo} 

# native code objects
OBJECTS_OPT=${MMODULES:%.ml=%.cmx} ${PMODULES:%.ml=%.cmx} 

NOTES=$(wildcard ../notes/*) 

# contents of the tar package
ARCHIVE=$(MMODULES) ${MMODULES:%.ml=%.mli} $(MAIN) $(PARSERY) $(LEXERI) $(LEXERL) \
  Makefile README ../test/README $(EXAMPLES) $(NOTES) .depend


#all: lib or exec or tester
all: exec

# lib and cmi files
lib: ${OBJECTS:%.cmo=%.cmi} $(OBJECTS)
	$(CAMLLIB) -o $(LIB_NAME) $(OBJECTS)

# native lib and cmi files
libopt: ${OBJECTS:%.cmo=%.cmi} $(OBJECTS_OPT)
	$(CAMLLIBOPT) -o $(LIB_OPT_NAME) $(OBJECTS_OPT)

tester: $(OBJECTS) $(TESTOBJ) ${TESTMAIN:%.ml=%.cmo}
	$(CAMLC) -o $(TESTER_NAME) $(OBJECTS) $(TESTOBJ) ${TESTMAIN:%.ml=%.cmo}

# bytecode executable
exec: $(OBJECTS) ${MAIN:%.ml=%.cmo}
	$(CAMLC) -g -o $(EXEC_NAME) $(OBJECTS) ${MAIN:%.ml=%.cmo}

# native executable
execopt: $(OBJECTS_OPT) ${MAIN:%.ml=%.cmx}
	$(CAMLCOPT)  -o $(EXEC_NAME).opt $(OBJECTS_OPT) ${MAIN:%.ml=%.cmx}

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLCOPT) -c $<


$(PARSERI) $(PARSER): $(PARSERY)
	$(CAMLYACC) $(PARSERY)

$(LEXER): $(LEXERL)
	$(CAMLLEX) $(LEXERL)

# doc: APIs
doc: html


# API documentation (html)
# only interfaces (.mli)
# ocamldoc needs the objects
html: ${OBJECTS:%.cmo=%.cmi}
	$(MKHTMLDOC) ${OBJECTS:%.cmo=%.mli};

# make a todo list from the @todo tags in .ml and .mli
# with custom doc generator for ocamldoc from
# http://home.gna.org/ocamltdl
todo:  $(OBJECTS) ${OBJECTS:%.cmo=%.cmi}
	$(MKTODOLIST) ${MMODULES:%.ml=%.mli} $(MMODULES)

# package 
tar: $(ARCHIVE)
	tar -c -v -f $(ARCHIVE_NAME) -C ../.. ${ARCHIVE:%=TACE/%}

# dependencies (needs .ml and .mli for lexer and parser)
dep: $(LEXER) $(PARSER)
	$(MKDEP) ${MMODULES:%.ml=%.mli} $(MMODULES) > .depend

# tags file for emacs
tags: ${MMODULES:%.ml=%.mli} $(MMODULES) $(PARSERY) $(LEXERI) $(LEXERL)
	etags "--regex=/let[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/and[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/type[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/exception[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/val[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/module[ \t]+\([^ \t]+\)/\1/" \
              ${MMODULES:%.ml=%.mli} $(LEXERI) $(MMODULES) $(PARSERY) $(LEXERL)


clean:
	rm -f *.cmi \
	rm -f *.cmo \
	rm -f *.cmx \
	rm -f *.o \
	rm -f $(EXEC_NAME) \
	rm -f $(EXEC_NAME).opt \
	rm -f $(TESTER_NAME) \
	rm -f $(LIB_NAME) \
	rm -f $(LIB_OPT_NAME) \
	rm -f $(ARCHIVE_NAME) \
	rm -f html/* \
	rm -f $(PARSERI) \
	rm -f $(PARSER) \
	rm -f $(LEXER) \
	rm *~

# dependencies
include .depend
