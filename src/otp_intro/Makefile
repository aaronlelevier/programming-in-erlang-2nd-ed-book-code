 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = server1 server2 server3 server4 \
	name_server name_server1 new_name_server


all: compile
	@erl \
		-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro' \
		-s name_server init0 \
		-s server1 init \
		-s server2 init \
		-s server3 init \
		-s server4 init \
		-s name_server1 init0 \
		-s new_name_server init0

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


