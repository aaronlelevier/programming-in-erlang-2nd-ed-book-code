%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 06:26
%%%-------------------------------------------------------------------
-module(walks).
-author("aaron").
-chapter([nine]).

%% API
-export([plan_route/2, plan_route1/2, plan_route2/2]).


%%%%%% public %%%%%%


-spec plan_route(From:: point(), To:: point()) -> route().

-type direction() :: north | south | east | west.
-type point() :: {integer(), integer()}.
-type route() :: {go, direction(), integer()}.

plan_route(Point1, Point2) -> 0.


%% can use a range with "X..Y", and nested Enum example

-type angle() :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
-type position() :: {latitude | longitude, angle()}.
-spec plan_route1(From::position(), To::position()) -> route().

plan_route1(Position1, Position2) -> 0.


%% define some more types - p. 144

-type on_off() :: on | off.
-type full_name :: {string(), string()}.
-type age() :: 0..120.
-type person() :: {full_name(), age()}.


%% can use the `when` keyword to add type def's after the method signature

-spec plan_route2(Point1, Point2) -> Route when
  Point1 :: point(),
  Point2 :: point(),
  Route :: route().


plan_route2(Point1, Point2) -> 0.
