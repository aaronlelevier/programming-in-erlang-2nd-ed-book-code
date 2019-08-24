%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Run `make example1` then code from pg. 239 of the book
%%% @end
%%% Created : 24. Aug 2019 09:25
%%%-------------------------------------------------------------------
-module(example1).
-author("aaron lelevier").
-chapter(["14"]).
-desc("Port process example").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start() ->
  register(
    example1,
    spawn(
      fun() ->
        process_flag(trap_exit, true),
        Port = open_port({spawn, "./example1"}, [{packet, 2}]),
        loop(Port)
      end)
  ).

stop() -> ?MODULE ! stop.

sum(A, B) -> call_port({sum, A, B}).

twice(A) -> call_port({twice, A}).

%% Caller below in the `loop` func
call_port(Msg) ->
  ?MODULE ! {call, self(), Msg},
  receive
    {?MODULE, Result} ->
      Result
  end.

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, encode(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {?MODULE, decode(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit({port_terminated, Reason})
  end.

encode({sum, A, B}) -> [1, A, B];
encode({twice, A}) -> [2, A].

decode([Int]) -> Int.

