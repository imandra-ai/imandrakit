
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

OPAM_PKGS=imandrakit imandrakit-io imandrakit-log imandrakit-thread twine-utils

# `--deps-only` skips depopts, and won't add test deps once the
# packages themselves are installed, so list both explicitly
OPAM_DEPOPTS=camlzip
OPAM_TEST_DEPS=qcheck-core trace-tef hex

_opam:
	opam switch create . --empty

opam-install-deps:
	opam install . --deps-only

opam-install-dev-deps:
	opam install . --deps-only --with-test --with-doc
	opam install $(OPAM_DEPOPTS) $(OPAM_TEST_DEPS)

opam-pin:
	opam pin . -y -n

# -w pins your working tree, not just the latest commit
opam-pin-dev:
	opam pin . -y -n -w

opam-install: opam-pin
	opam install $(OPAM_PKGS)

opam-uninstall:
	opam remove $(OPAM_PKGS)

opam-unpin:
	opam pin remove $(OPAM_PKGS)

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)
watch-autopromote:
	dune build $(DUNE_OPTS) -w $(WATCH) --auto-promote

.PHONY: test clean build doc build-dev \
	opam-install-deps opam-install-dev-deps opam-pin opam-install \
	opam-uninstall opam-unpin

VERSION=$(shell awk '/^version:/ {print $$2}' imandrakit.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
