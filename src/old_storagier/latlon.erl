%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @notes
%%% Distance SO example in Python - https://stackoverflow.com/a/19412565/1913888
%%%
%%% Adopted Erlang Example - https://github.com/pdincau/erlang-blog/blob/harvesine/haversine.erl
%%%
%%% Lat/Lon Online Distance calculator - https://www.nhc.noaa.gov/gccalc.shtml
%%%
%%% @end
%%% Created : 27. Jul 2019 06:39
%%%-------------------------------------------------------------------
-module(latlon).
-author("aaron").

%% API
-export([test/0, distance/2, to_tuple/1]).

-include_lib("records.hrl").


%%%%%% public %%%%%%


%% returns the distance between 2 LatLon points
-spec distance(#latlon{}, #latlon{}) -> float().

distance(LatLon1, LatLon2) ->
  Lat1 = radians(LatLon1#latlon.lat),
  Lon1 = radians(LatLon1#latlon.lon),
  Lat2 = radians(LatLon2#latlon.lat),
  Lon2 = radians(LatLon2#latlon.lon),

  DLat = Lat2 - Lat1,
  DLon = Lon2 - Lon1,

  A = math:pow(math:sin(DLat / 2), 2) +
    math:cos(Lat1) * math:cos(Lat2) * math:pow(math:sin(DLon / 2), 2),

  C = 2 * math:asin(math:sqrt(A)),

  EarthRadius = 6372.8,
  Km = EarthRadius  * C,
  to_miles(Km).


%% returns the LatLon as a 2 item tuple
-spec to_tuple(#latlon{}) -> {float(), float()}.

to_tuple(LatLon) ->
  {LatLon#latlon.lat, LatLon#latlon.lon}.


%%%%%% private %%%%%%


%% converts kilometers to miles
%% @returns float
to_miles(Kilometers) ->
  Kilometers * 0.621371.


%% converts N to radians
%% @returns float
radians(N) ->
  N * math:pi() / 180.


%%%%%% tests %%%%%%

test() ->
  ok = test_distance(),
  ok.

test_distance() ->
  LatLonA = #latlon{lat=1.5, lon=2.2},
  LatLonB = #latlon{lat=3.9, lon=4},
  Ret = latlon:distance(LatLonA, LatLonB),
  207 = round(Ret),
  ok.
