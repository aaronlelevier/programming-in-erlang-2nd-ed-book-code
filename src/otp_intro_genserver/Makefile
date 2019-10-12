 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = job_centre


all: compile
	@erl \
		-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro_genserver' \
		-s job_centre start_link

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


