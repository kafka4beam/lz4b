PROJECT = lz4b
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include erlang.mk

ERLC=erlc

all:: ebin/lz4b_nif.so lz4b.beam

deps::
	git submodule init
	git submodule update

test:: eunit

test-build:: ebin/lz4b_nif.so

ebin/lz4b_nif.so: lz4b_nif.so
	cp c_src/lz4b_nif.so $@

lz4b_nif.so: c_src/lz4b.c
	@$(MAKE) -C c_src $@

lz4b.beam: src/lz4b_frame.erl
	@$(ERLC)  -o ebin/ $^
