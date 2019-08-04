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
-export([loop/0, start/0]).


start() -> ok.

loop() ->
  receive
    {From, {rectangle, Height, Width}} ->
%%      io:format("Rectangle area is: ~p~n", [Height * Width]),
      From ! Height * Width,
      loop();
    {From, {square, Side}} ->
%%      io:format("Square area is: ~p~n", [Side * Side]),
      From ! Side * Side,
      loop();
    {From, {circle, Radius}} ->
%%      io:format("Circle area is: ~p~n", [2 * math:pi() * Radius]),
      From ! 2 * math:pi() * Radius,
      loop();
    Request ->
      io:format("No match: ~p~n", [Request]),
      loop()
  end.