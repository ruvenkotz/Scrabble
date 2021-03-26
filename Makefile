MODULES=bag hand
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip:
	zip enigma.zip *.ml* *.sh _tags .merlin .ocamlinit LICENSE Makefile

docs:
	mkdir -p _doc
	ocamldoc -d _doc -html enigma.ml

clean:
	ocamlbuild -clean
	rm -rf _doc enigma.zip
