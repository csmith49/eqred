ocb_flags = -r -use-ocamlfind -pkgs 'containers'
ocb = ocamlbuild $(ocb_flags)

.phony: all
all: eqred

eqred: $(shell find src -type f) $(shell find builtins -type f)
	$(ocb) eqred.native
	mv eqred.native eqred

.phony: clean
clean:
	rm -rf _build eqred
