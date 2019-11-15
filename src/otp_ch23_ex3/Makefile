 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = event_handler lb_app lb_supervisor lb_server worker_server \
	round_robin

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/idioms' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s lb_app test

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
