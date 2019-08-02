%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2019 07:03
%%%-------------------------------------------------------------------
-module(area_server0).
-author("aaron lelevier").

-chapter(["12"]).

%% API
-export([loop/0]).


loop() ->
  receive
    {rectangle, Height, Width} ->
      io:format("Rectangle area is: ~p~n", [Height * Width]),
      loop();
    {square, Side} ->
      io:format("Square area is: ~p~n", [Side * Side]),
      loop();
    {circle, Radius} ->
      io:format("Circle area is: ~p~n", [2 * math:pi() * Radius]),
      loop()
  end.