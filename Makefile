#
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file c
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between
#                  existing modules.  (The graph is stored in the file
#                  .depend)
#  make full    runs clean, depend and all in order

# These are the object files needed to rebuild the mysplinterpreter executable file

OBJS   = Environment.cmo Types.cmo Checker.cmo Evaluator.cmo Parser.cmo Lexer.cmo Language.cmo mysplinterpreter.cmo
COMMON = str.cma
DEPEND = Lexer.ml Parser.ml# Files that need to be generated from other files

OCAMLC = ocamlc -w +a-3-4-6-7-9-27-29-32..39-41..42-44-45# Disable deprecation warning

# Fix Makefile on Windows, not sure how it works
ifeq ($(OS), Windows_NT)
	SHELL=cmd
endif

# When "make" is invoked with no arguments, we build an executable
# typechecker, after building everything that it depends on
all: $(DEPEND) $(OBJS) mysplinterpreter

# Include an automatically generated list of dependencies between source files
include .depend

# Build an executable typechecker
mysplinterpreter: $(OBJS) mysplinterpreter.cmo
	@echo Linking $@
	$(OCAMLC) -o $@ $(COMMON) $(OBJS)

# Compile an ML module interface
%.cmi : %.mli
	$(OCAMLC) -c $<

# Compile an ML module implementation
%.cmo : %.ml
	$(OCAMLC) -c $<

# Generate ML files from a Parser definition file
Parser.ml Parser.mli: Parser.mly
	@rm -f Parser.ml Parser.mli
	ocamlyacc -v Parser.mly
	@chmod -w Parser.ml Parser.mli

# Generate ML files from a Lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf Lexer.ml Parser.ml Parser.mli *.o *.cmo *.cmi Parser.output c TAGS *~

# Rebuild intermodule dependencies
depend: $(DEPEND)
	ocamldep *.mli *.ml > .depend

full: clean depend all
