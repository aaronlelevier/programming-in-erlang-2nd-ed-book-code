%% This is the application resource file (.app file) for the 'base' %% application.
{application, lb,
  [{description, "Load Balancer example App"},
    {vsn, "1.0"},
    {modules, [
      lb_app,
      lb_supervisor,
      lb_server,
      worker_server,
      event_handler
    ]},
    {registered, [
      lb_supervisor,
      lb_server,
      worker_server
    ]},
    {applications, [kernel, stdlib]},
    {mod, {lb_app, []}},
    {start_phases, []}
  ]}.