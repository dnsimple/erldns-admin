all: clean build

build:	
	./rebar3 get-deps
	./rebar3 compile

clean:
	./rebar3 clean

wipe:
	rm -Rf deps
	./rebar3 clean

fresh: wipe build
