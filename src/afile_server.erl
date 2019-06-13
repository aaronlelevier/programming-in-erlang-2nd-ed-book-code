-module(afile_server).

-export([start/1, loop/1]).

%% arguments are: module, function, list of args for the function
start(Dir) -> spawn(afile_server, loop, [Dir]).

loop(Dir) ->
  receive
    % ls
    {Client, list_dir} ->
      io:format("list_dir cmd received~n"),
      Client ! {self(), file:list_dir(Dir)};

    % get_file
    {Client, {get_file, File}} ->
      io:format("get_file cmd received~n"),
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)};

    % put_file
    {Client, {put_file, File, Content}} ->
      file:write_file(File, io_lib:fwrite("~p.\n", [Content])),
      Client ! {self(), File}
  end,
  loop(Dir).

