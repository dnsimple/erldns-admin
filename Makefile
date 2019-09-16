REBAR:=$(shell which rebar3 || echo ./rebar3)
REBAR_URL:="https://s3.amazonaws.com/rebar3/rebar3"

all: clean build

$(REBAR):
	wget $(REBAR_URL) && chmod +x rebar3

build: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

wipe: $(REBAR)
	rm -Rf _build
	$(REBAR) clean

fresh: wipe build
