 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = event_handler motor_controller logger_api my_alarm_handler \
	terminal_logger file_logger product_handler prime_server \
	lib_primes lib_lin area_server sellaprime_supervisor \
	sellaprime_app prime_tester_server helpers round_robin

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/otp_system' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s round_robin test

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
