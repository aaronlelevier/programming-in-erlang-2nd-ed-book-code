 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = multi_server adapter_db

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/idioms' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s adapter_db test

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
