%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 08:24
%%%-------------------------------------------------------------------
-module(location).
-author("aaron").

%% API
-export([test/0, distance/2, available/2, available_units/1]).

-include_lib("records.hrl").

%%%%%% public %%%%%%


%% returns the distance between two Locations
%% @returns float
distance(Location1, Location2) ->
  latlon:distance(
    Location1#location.latlon, Location2#location.latlon).


%% returns a boolean if the Location is available for the request
%% Amount of storage, which is an integer Amount of units needed
%% @returns boolean
available(Location, Amount) ->
   available_units(Location) - Amount >= 0.


%% returns the number of available units
%% @returns integer
available_units(Location) ->
  units:available(Location#location.units).


%%%%%% tests %%%%%%

test() ->
  ok = test_distance(),
  ok = test_available(),
  ok = test_available_units().


test_distance() ->
  Location1 = #location{latlon = #latlon{lat = 6, lon = 7}},
  Location2 = #location{latlon = #latlon{lat = 5, lon = 9}},
  Ret = location:distance(Location1, Location2),
  154 = round(Ret),
  ok.


test_available() ->
  Location = #location{units = #units{total = 20, in_use = 15}},
  % not available case
  false = available(Location, 10),
  % available
  true = available(Location, 5),
  ok.


test_available_units() ->
  Location = #location{units = #units{total = 20, in_use = 15}},
  5 = available_units(Location),
  ok.
