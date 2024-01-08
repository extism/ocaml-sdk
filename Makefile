VERSION?=1.0.0

PREFIX?=$$HOME/.local

build:
	dune build

test:
	dune test

format:
	dune build @fmt --auto-promote

prepare:
	opam install . --deps-only --with-test --with-doc

publish:
	opam publish -v $(VERSION) https://github.com/extism/ocaml-sdk/archive/refs/tags/v$(VERSION).tar.gz .

install-cli: build
	install _build/default/bin/main.exe "$(PREFIX)/bin/extism-call"
