-module(temperature).
-export([convert/1]).

convert({c,C}) -> {f, 32 + C*9/5};
convert({f,F}) -> {c, (F-32) *5/9}.
