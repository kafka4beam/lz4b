all: compile test

test: compile
	rebar3 eunit

compile:
	rebar3 compile

clean:
	make -C c_src clean
