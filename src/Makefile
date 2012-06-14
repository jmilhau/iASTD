YACC=ocamlyacc
LEX=ocamllex
OCAMLOPT = ocamlopt
OCAMLC = ocamlc 
OCAMLDEP = ocamldep
MKTOPLEVEL = ocamlmktop
INCLUDES = -g 
OCAMLOPTFLAGS = $(INCLUDES)
OCAMLFLAGS = $(INCLUDES)
OCAMLDEPFLAGS =

OCAMLTOPLEVEL = ocaml_ASTD
INTERPRETER = iASTD

TARGET = $(INTERPRETER) $(OCAMLTOPLEVEL)

BASE = functions \
	ASTD_constant \
	ASTD_variable ASTD_term \
	ASTD_label ASTD_environment\
	ASTD_predicate ASTD_predicate_definitions \
	ASTD_transition \
	ASTD_event \
	ASTD_arrow \
	ASTD_astd \
	ASTD_state \
	ASTD_kappa_indirect \
	ASTD_possibilities \
	ASTD_execute
	

PARSER = ASTD_parser

PARSERRULES = ASTD_parser_rules ASTD_lexer_rules  

OBJ = $(BASE) $(PARSERRULES) $(PARSER)
OBJCMX = $(addsuffix .cmx,$(OBJ)) 
OBJCMO = $(addsuffix .cmo,$(OBJ))  

DOC = $(BASE) $(PARSER)
DOCMLI = $(addsuffix .mli,$(DOC))  

GENDOC = gen_ocamldoc/gen_ocamlpai

# MAIN RULES 

.PHONY : depend 
all : $(TARGET)

$(OCAMLTOPLEVEL) : $(OBJCMO)
	$(MKTOPLEVEL) $(OCAMLFLAGS) $^ -o $@

$(INTERPRETER) : $(OBJCMX) ASTD_main.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $^ -o $@

# SPECIAL RULES FOR PARSER

ASTD_lexer_rules.ml: ASTD_lexer_rules.mll ASTD_parser_rules.cmi
	$(LEX) ASTD_lexer_rules.mll

ASTD_parser_rules.ml: ASTD_parser_rules.mly
	$(YACC) ASTD_parser_rules.mly

ASTD_parser_rules.mli: ASTD_parser_rules.ml

ASTD_lexer_rules.cmx: ASTD_lexer_rules.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

ASTD_lexer_rules.cmo: ASTD_lexer_rules.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# GENERIC RULES

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml %.cmi 
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml %.cmi 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# CLEAN
clean: 
	-\rm -f $(addsuffix .ml,$(PARSERRULES)) $(addsuffix .mli,$(PARSERRULES))
	-\rm -f *.cm[iox] *.o 

cleantarget:
	-\rm $(TARGET)

cleandoc:
	-\rm -f types_dependencies.dot modules_dependencies.dot 
	-\rm -f -r doc/*

cleangendoc: 
	-\rm -f $(GENDOC).cm*

cleanall: clean cleandoc cleangendoc cleantarget
	-\rm -f .depend

# DOC 
doc: cleandoc $(DOCMLI) types_dependencies.dot modules_dependencies.dot doc/index.html

$(GENDOC).cmo : $(GENDOC).ml
	ocamlc -I $$(dirname `ocamldoc -customdir`) -c $^

doc/index.html : $(GENDOC).cmo $(TARGET) 
	-if [ ! -d doc/ ] ; then mkdir doc/ ; fi 
	cp -f -p template/style.css doc/
	ocamldoc -v -css-style style.css \
             -colorize-code -keep-code \
             -g $(GENDOC).cmo -max-line-length 800 \
             -t OCamlPAI -d doc/ $(DOCMLI) 

modules_dependencies.dot : $(TARGET)
	ocamldoc -dot -dot-include-all ASTD*.ml -o modules_dependencies.dot

types_dependencies.dot : $(TARGET)
	ocamldoc -dot -dot-include-all -dot-types ASTD*.ml -o types_dependencies.dot

# DEPENDENCIES
.depend:
	$(OCAMLDEP) $(OCAMLDEPFLAGS) *.mli *.ml > .depend

include .depend

