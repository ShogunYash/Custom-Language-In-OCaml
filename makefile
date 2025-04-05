# Makefile for the Matrix/Vector Language Parser and Interpreter

# OCaml compiler and flags
OCAMLC = ocamlfind ocamlc -package str
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamlfind ocamldep -package str
OCAMLFLAGS = -g -w A

# Source files
ML_SOURCES = ast.ml parser.ml lexer.ml ast_type_checker.ml interpreter.ml main.ml
MLI_SOURCES = parser.mli

# Generated files
LEXER_GEN = lexer.ml
PARSER_GEN = parser.ml parser.mli

# Object files
CMO_FILES = ast.cmo parser.cmo lexer.cmo ast_type_checker.cmo interpreter.cmo main.cmo

# Executable name
EXEC = interpreter

# Default target
all: $(EXEC)

# Run the executable
run: $(EXEC)
	@echo "Running test cases: lexing -> parsing -> type checking -> interpreting"
	./$(EXEC)

# Bytecode executable
$(EXEC): $(CMO_FILES)
	$(OCAMLC) -o $(EXEC) -linkpkg $(CMO_FILES)

# Generate lexer from .mll
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

# Generate parser from .mly
parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

# Generic rules for OCaml compilation
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Clean target
clean:
	rm -f *.cmo *.cmi *.o $(LEXER_GEN) $(PARSER_GEN) $(EXEC) .depend

# Dependencies
depend: $(LEXER_GEN) $(PARSER_GEN) $(ML_SOURCES) $(MLI_SOURCES)
	$(OCAMLDEP) $^ > .depend

# Include dependencies
-include .depend

# New target to run a specific .txt file through the interpreter
runtest:
	@if [ -z "$(FILE)" ]; then \
	  echo "Please supply a test file name. For example:"; \
	  echo "  make runtest FILE=test_type_check.txt"; \
	  exit 1; \
	fi; \
	./$(EXEC) -run-test $(FILE)

# Phony targets
.PHONY: all clean depend run runtest