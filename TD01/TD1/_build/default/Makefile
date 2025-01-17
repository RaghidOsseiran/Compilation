all: tuto.out term_exe.out

tuto.out: td_1/tuto/*.ml
	rm -rf tuto.out
	dune build
	ln -s _build/default/td_1/tuto/tuto.exe tuto.out

term_exe.out: td_1/term/*.ml td_1/bin/*.ml
	rm -rf term_exe.out
	dune build
	ln -s _build/default/td_1/bin/main.exe term_exe.out

.PHONY: clean
clean:
	rm -rf term_exe.out tuto.out doc.html

.PHONY: doc
doc: doc.html

doc.html:
	rm -f doc.html
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html