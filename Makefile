OCAMLC = ocamlc
OCAMLFLAGS = -g

KRIVINE_SRC = krivine.ml
SECD_SRC = secd.ml
KRIVINE_TEST = tests/test_krivine.ml
SECD_TEST = tests/test_secd.ml

KRIVINE_EXE = testkri
SECD_EXE = testsecd

all: $(KRIVINE_EXE) $(SECD_EXE)

$(KRIVINE_EXE): $(KRIVINE_SRC) $(KRIVINE_TEST)
	@$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

$(SECD_EXE): $(SECD_SRC) $(SECD_TEST)
	@$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

clean:
	@rm -f *.cmi *.cmo tests/*.cmi tests/*.cmo
	@rm -f $(KRIVINE_EXE) $(SECD_EXE) tests/*_test*

test: $(KRIVINE_EXE) $(SECD_EXE)
	@./$(KRIVINE_EXE)
	@./$(SECD_EXE)

.PHONY: all clean test