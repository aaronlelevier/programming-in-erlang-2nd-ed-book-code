%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Making a Set of Processes That All Die Together - pg. 208-9
%%% @end
%%% Created : 14. Aug 2019 07:15
%%%-------------------------------------------------------------------
-module(ft2).
-author("aaron lelevier").

%% API
-export([]).
-compile(export_all).


start() ->
  Funs = [ft:list_to_atom_fun(), ft:list_to_atom_fun()],
  init_workers(Funs).


%% Fs - a list of Funs
init_workers(Fs) ->
  spawn(
    fun() ->
      %[spawn_link(F) || F <- Fs],
      for_spawn_link(Fs),
      receive
      after infinity ->
        true
      end
    end).


%% process list of Funs, spawn_link, and register last Pid as the main_worker
for_spawn_link([]) -> done;
for_spawn_link(L) ->
  [H|T] = L,
  Pid = spawn_link(H),
  ft:set_on_exit_handler(Pid),
  Name = utils:integer_to_atom(length(T)),
  register(Name, Pid),
  for_spawn_link(T).


%%% Demo of how to call `start()` above in the Terminal

%% 62> c(ft2).
%% ft2.erl:14: Warning: export_all flag enabled - all functions will be exported
%% {ok,ft2}
%% 63> c(ft2).
%% ft2.erl:14: Warning: export_all flag enabled - all functions will be exported
%% <0.257.0> died with:killed
%% <0.255.0> died with:killed
%% {ok,ft2}
%% 64> ft2:start().
%% <0.271.0>
%% 65> registered().
%% [init,erts_code_purger,'1',logger_sup,erl_signal_server,
%% logger_proxy,kernel_refc,logger_std_h_default,
%% standard_error_sup,logger_handler_watcher,erl_prim_loader,
%% global_group,logger,rex,user_drv,code_server,kernel_sup,
%% global_name_server,application_controller,file_server_2,'0',
%% standard_error,user,inet_db,kernel_safe_sup]
%% 66> '1' ! 'not a list'.
%% <0.272.0> died with:{badarg,[{erlang,list_to_atom,['not a list'],[]},
%% {ft,'-list_to_atom_fun/0-fun-0-',0,
%% [{file,"ft.erl"},{line,57}]}]}
%% =ERROR REPORT==== 14-Aug-2019::07:46:32.566838 ===
%% Error in process <0.272.0> with exit value:
%% {badarg,[{erlang,list_to_atom,['not a list'],[]},
%% {ft,'-list_to_atom_fun/0-fun-0-',0,[{file,"ft.erl"},{line,57}]}]}
%% 
%% <0.274.0> died with:{badarg,[{erlang,list_to_atom,['not a list'],[]},
%% {ft,'-list_to_atom_fun/0-fun-0-',0,
%% [{file,"ft.erl"},{line,57}]}]}
%% 'not a list'
%% 67> 
