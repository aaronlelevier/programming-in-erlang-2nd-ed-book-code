 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean

MODS = event_handler motor_controller logger_api my_alarm_handler \
	terminal_logger file_logger product_handler prime_server \
	lib_primes lib_lin area_server

all: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/otp_system' \
	-pa '/Users/aaron/Documents/erlang/book-code/src' \
	-s event_handler init \
	-s motor_controller init \
	-s logger_api init \
	-s my_alarm_handler init0

events: compile
	@erl \
	-pa '/Users/aaron/Documents/erlang/book-code/src/otp_system'

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
