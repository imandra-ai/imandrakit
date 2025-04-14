
DUNE_OPTS?=
build:
	@dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)
test-autopromote:
	@dune runtest $(DUNE_OPTS) --auto-promote

doc:
	@dune build $(DUNE_OPTS) @doc

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

opam-install-deps:
	# Until there's a new release of `trace`, we need to pin it.
	opam pin https://github.com/c-cube/ocaml-trace.git -y -n
	opam install . --deps-only

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)
watch-autopromote:
	dune build $(DUNE_OPTS) -w $(WATCH) --auto-promote

.PHONY: test clean build doc build-dev

VERSION=$(shell awk '/^version:/ {print $$2}' imandrakit.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
