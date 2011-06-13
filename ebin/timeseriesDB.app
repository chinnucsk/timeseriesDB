%% -*- erlang -*-
{application, timeseriesDB,
 [{description, "timeseriesDB"},
  {vsn, "0.1"},
  {modules, [
	     tsdb,
	     tsdb_app,
	     tsdb_sup,
	     tsdb_web,
	     tsdb_storage_lib,
	     tsdb_ram_storage
	    ]},
  {registered, []},
  {mod, {'tsdb_app', []}},
  {env, []},
  {applications, [kernel, stdlib, sasl, crypto]}]}.
