all: compile test

test: compile
	rebar3 eunit

compile:
	rebar3 compile

clean:
	rm -rf ebin/*
	make -C c_src clean
	rebar3 clean
