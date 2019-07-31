#!/bin/sh
erl -noshell -pa /Users/aaron/Documents/erlang/src/ \
             -s hello start -s init stop
