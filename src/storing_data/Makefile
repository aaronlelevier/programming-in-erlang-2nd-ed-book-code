 # leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<
.yrl.erl:
		erlc -W $<

ERL = erl -boot start_clean
# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character
# to the end of the line and continue on the next line
# Edit the lines below
MODS = ets_test dets_test

all: compile
	${ERL} -pa '/Users/aaron/Documents/erlang/src/files' \
		-s ets_test start \
		-s dets_test init

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


