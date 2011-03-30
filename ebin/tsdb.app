%% -*- erlang -*-
{application, tsdb,
 [{description, "timeseriesDB"},
  {vsn, "0.1"},
  {modules, [
	     %% Modules should be added!!!
	    ]},
  {registered, []},
  {mod, {'tsdb_app', []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
