 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = name_server server1 server2

all: compile
	@erl \
		-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro' \
		-s name_server init0 \
		-s server1 init \
		-s server2 init

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


