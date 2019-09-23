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
MODS = src/ch19_ex2 test/ch19_ex2_test

all: compile
	${ERL} -noshell \
	    -pa '/Users/aaron/Documents/erlang/book-code/src' \
	    -pa '/Users/aaron/Documents/erlang/book-code/test' \
	    -s ch19_ex2_test test -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
