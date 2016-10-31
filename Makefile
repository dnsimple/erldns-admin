all: clean build

build:	
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean

wipe:
	rm -Rf deps
	./rebar clean

fresh: wipe build
