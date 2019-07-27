%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 06:34
%%%-------------------------------------------------------------------
-module(query).
-author("aaron").

%% API
-export([test/0, available/3]).

-include_lib("records.hrl").

%% config
-define(MAX_DISTANCE, 5).


%%%%%% public %%%%%%


%% returns a list of available `location` records based upon the LatLong
%% @param Amount - integer - requested amount of storage
%% @param LatLon - #latlon{} - origin of request
%% @param Locations - list #location{}
%% @returns list<location>
available(Amount, LatLon, Locations) ->
  lists:filter(
    fun(Location) ->
      location_is_nearby(LatLon, Location) and
      location_is_in_radius(LatLon, Location) and
      location:is_available(Location, Amount)
    end,
    Locations).


%%%%%% private %%%%%%


filter_by_radius(LatLon, Locations) ->
  lists:filter(fun(Location) ->
    location_is_in_radius(LatLon, Location) end,
    Locations).


location_is_in_radius(LatLon, Location) ->
  OriginLocation = #location{latlon = LatLon},
  location:distance(Location, OriginLocation) =< ?MAX_DISTANCE.


%% filter out Locations that aren't w/i +/- 1 Lon and Lat
filter_by_latlon(LatLon, Locations) ->
  lists:filter(
    fun(Location) -> location_is_nearby(LatLon, Location) end,
    Locations).


location_is_nearby(LatLon, Location) ->
  {Lat, Lon} = latlon:to_tuple(LatLon),
  {Lat2, Lon2} = latlon:to_tuple(Location#location.latlon),
  is_nearby(Lat, Lat2) and is_nearby(Lon, Lon2).


is_nearby(X, Y) ->
  A = round(X),
  B = round(Y),
  (A == B) or (A + 1 == B) or (A - 1 == B).


%%%%%% tests %%%%%%

test() ->
  ok = test_available(),
  ok = test_filter_by_radius(),
  ok = test_filter_guard(),
  ok = test_is_plus_or_minus_one().


test_available() ->
  Amount = 10,
  LatLon = #latlon{lat = 1, lon = 1},

  % match
  Location1 = #location{
    units = #units{total = 20, in_use = 0},
    latlon = #latlon{lat = 1, lon = 1.01}},

  % no match - not nearby
  Location2 = #location{
    units = #units{total = 20, in_use = 0},
    latlon = #latlon{lat = 50, lon = 50}},
  false = location_is_nearby(LatLon, Location2),

  % no match - not within radius
  Location3 = #location{
    units = #units{total = 20, in_use = 0},
    latlon = #latlon{lat = 2, lon = 2}},
  false = location_is_in_radius(LatLon, Location3),

  % not match - not enough available units
  Location4 = #location{
    units = #units{total = 20, in_use = 15},
    latlon = #latlon{lat = 1, lon = 1}},
  false = location:is_available(Location4, Amount),

  % locations to check
  Locations = [Location1, Location2, Location3, Location4],

  Ret = available(Amount, LatLon, Locations),

  1 = length(Ret),
  [H | _T] = Ret,
  Location1 = H,
  ok.


test_filter_by_radius() ->
  LatLon = #latlon{lat = 1, lon = 1},
  % matches
  Location1 = #location{latlon = #latlon{lat = 1.01, lon = 1}},
  % doesn't match
  Location2 = #location{latlon = #latlon{lat = 2, lon = 2}},

  Ret = filter_by_radius(LatLon, [Location1, Location2]),

  1 = length(Ret),
  [H | _T] = Ret,
  Location1 = H,
  ok.


test_filter_guard() ->
  LatLon = #latlon{lat = 7, lon = 7},
  % matches
  Location1 = #location{latlon = #latlon{lat = 6, lon = 7}},
  % doesn't match
  Location2 = #location{latlon = #latlon{lat = 5, lon = 9}},

  Ret = filter_by_latlon(LatLon, [Location1, Location2]),

  1 = length(Ret),
  [H | _T] = Ret,
  Location1 = H,
  ok.


test_is_plus_or_minus_one() ->
  true = is_nearby(7, 6),
  true = is_nearby(7, 7),
  true = is_nearby(6, 7),
  false = is_nearby(5, 7),
  false = is_nearby(7, 5),
  ok.
