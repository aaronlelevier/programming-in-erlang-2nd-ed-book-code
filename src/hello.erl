-module(hello).
-export([start/0]).

%% how to run this module/function from the CL
%%
%% erl -noshell -s hello start -s init stop

start() ->
  io:format("Hello world~n").