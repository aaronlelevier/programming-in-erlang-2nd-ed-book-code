%% @doc macros for this module should live here

-define(DEBUG(X), io:format("MOD:~p LINE:~p ~p~n", [?MODULE, ?LINE, X])).

-define(HTML_DIR, "html/").