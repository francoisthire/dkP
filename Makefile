.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

dkP:
	@ln -s _build/install/default/bin/dkpsuler dkP || true

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
