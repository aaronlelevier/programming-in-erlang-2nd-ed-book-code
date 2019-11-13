 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = esolang

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/codewars' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s esolang test

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
