REBAR:=$(shell which rebar3 || echo ./rebar3)
REBAR_URL:="https://s3.amazonaws.com/rebar3/rebar3"

all: clean build

$(REBAR):
	wget $(REBAR_URL) && chmod +x rebar3

.PHONY: build
build: $(REBAR)
	$(REBAR) compile

.PHONY: clean
clean: $(REBAR)
	$(REBAR) clean

.PHONY: wipe
wipe: $(REBAR)
	rm -Rf _build
	$(REBAR) clean

.PHONY: fresh
fresh: wipe build

.PHONY: test
test: $(REBAR)
	$(REBAR) fmt --check

.PHONY: format
format: $(REBAR)
	$(REBAR) fmt
