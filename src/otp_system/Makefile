 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = event_handler motor_controller logger_api

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/otp_intro_genserver' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s event_handler init \
	-s motor_controller init \
	-s logger_api init

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
