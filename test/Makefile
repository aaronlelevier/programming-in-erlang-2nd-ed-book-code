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
MODS = ch19_ex2_test

all: compile

compile: ${MODS:%=%.beam}
	@echo "make clean - clean up"

clean:
	rm -rf *.beam erl_crash.dump
