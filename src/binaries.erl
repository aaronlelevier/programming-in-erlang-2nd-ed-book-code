%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2019 06:47
%%%-------------------------------------------------------------------
-module(binaries).
-author("aaron").

%% API
-export([reverse/1, term_to_packet/1, packet_to_term/1]).


%% ch-7 ex-1 - reverse the order of bytes in a binary
reverse(Bin) ->
  List = binary_to_list(Bin),
  list_to_binary(lists:reverse(List)).


%% ch-7 ex-2
term_to_packet(Term) ->
  Bin = term_to_binary(Term),
  Header = size(Bin),
  <<Header:4, Bin/binary>>.


%% ch-7 ex-3
packet_to_term(Packet) ->
  <<_:4, Bin/binary>> = Packet,
  binary_to_term(Bin).


%% 153> f().
%% ok
%% 154> c(binaries).
%% {ok,binaries}
%% 155> Term = "Bob Cohen".
%% "Bob Cohen"
%% 156> Packet = binaries:term_to_packet(Term).
%% <<216,54,176,0,148,38,246,34,4,54,246,134,86,14:4>>
%% 157> binaries:packet_to_term(Packet).
%% "Bob Cohen"