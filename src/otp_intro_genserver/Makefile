 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = job_queue job_queue_test job_centre job_centre_test

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro_genserver' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s job_queue_test test \
	-s job_centre_test test

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
