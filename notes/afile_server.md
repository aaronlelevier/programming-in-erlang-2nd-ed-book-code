# afile_server notes

I got back in to Erlang for 1 day at this point!

Some of it is coming back and actually makes more sense than last time. Maybe my mind is ready to start learning it..

Here's some shell output from today, June 2, 2019.

```
aaron@ ~/Documents/erlang/src (master) $ erl
Erlang/OTP 20 [erts-9.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V9.2.1  (abort with ^G)
1> File = "/tmp/bar".
"/tmp/bar"
2> Content = "yo".
"yo"
3> io_lib:fwrite("~p.\n", [Content]).
["\"yo\"",46,10]
4> file:read_file("/tmp/bar").
{ok,<<"\"yo\".\n">>}
5> file:read_file("/tmp/bar").
{error,enoent}
6> file:write_file(File, io_lib:fwrite("~p.\n", [Content])).
ok
7> file:read_file("/tmp/bar").                              
{ok,<<"\"yo\".\n">>}
8> c(afile_server).
{ok,afile_server}
9> c(afile_client).
{ok,afile_client}
10> FileServer = afile_server:start(".").
<0.79.0>
11> afile_client:put_file(FileServer, "/tmp/biz", "Bro").
"/tmp/biz"
12> file:read_file("/tmp/biz").
{ok,<<"\"Bro\".\n">>}
```