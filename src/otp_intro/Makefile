 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = server1 name_server

all: compile
	@erl \
		-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro' \
		-s server1 init \
		-s name_server init0

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


