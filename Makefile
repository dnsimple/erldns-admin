all: clean build

.PHONY: build
build:
	rebar3 compile

.PHONY: clean
clean:
	rebar3 clean

.PHONY: wipe
wipe:
	rm -Rf _build
	rebar3 clean

.PHONY: fresh
fresh: wipe build

.PHONY: test
test:
	rebar3 fmt --check
	rebar3 lint
	rebar3 xref
	rebar3 dialyzer
	rebar3 ex_doc

.PHONY: format
format:
	rebar3 fmt
