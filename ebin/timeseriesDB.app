%% -*- erlang -*-
{application, timeseriesDB,
 [{description, "timeseriesDB"},
  {vsn, "0.1"},
  {modules, [
	     timestampwriter,
	     tsdb,
	     tsdb_app,
	     tsdb_sup,
	     tsdb_web
	    ]},
  {registered, []},
  {mod, {'tsdb_app', []}},
  {env, []},
  {applications, [kernel, stdlib, sasl, crypto]}]}.