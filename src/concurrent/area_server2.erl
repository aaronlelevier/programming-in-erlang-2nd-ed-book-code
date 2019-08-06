%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2019 07:03
%%%-------------------------------------------------------------------
-module(area_server2).
-author("aaron lelevier").

-chapter(["12"]).

-summary("Priority receive with timeout example").

%% API
-export([start/0, loop/0]).

start() -> spawn(area_server2, loop, []).

loop() ->
  Timer = 10000,
  % priority receive block
  % if a match occurs in here, the `Timer` will be reset
  receive
    {circle, Radius} ->
    io:format("Circle area is: ~p~n", [2 * math:pi() * Radius]),
    loop()
  after Timer ->
    % secondary receive block
    % process will suspend in this block however until a match
    % occurs, so the Msg `restart` can be sent to restart `loop`
    receive
      {rectangle, Height, Width} ->
        io:format("Rectangle area is: ~p~n", [Height * Width]),
        loop();
      {square, Side} ->
        io:format("Square area is: ~p~n", [Side * Side]),
        loop();
      restart ->
        io:fwrite("Restarting loop~n"),
        loop()
    end
  end.

