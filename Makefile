.PHONY: test
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
play:
	OCAMLRUNPARAM=b dune exec terminal/main.exe
build:
	dune build
clean:
	dune clean
bisect:
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html
bisect-clean:
	rm -rf _coverage bisect*.coverage
doc:
	dune build @doc
opendoc: doc
	@bash opendoc.sh
web_play: 
	@bash openweb.sh