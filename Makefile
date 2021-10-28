all:
	dune build --root .
	cp _build/install/default/bin/satysfifmt .