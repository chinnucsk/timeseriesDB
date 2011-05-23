-module(tsdb_storage_lib_tests).

-include_lib("eunit/include/eunit.hrl").

open_timeseries_file_test_() ->
    with_tmp_file(
      "aha1.ts",
      fun(File) ->
	      ?cmd("touch " ++ File),
	      ?assertMatch({ok,_}, tsdb_storage_lib:open_timeseries_file(File)),
	      ?cmd("rm " ++ File)
      end).

open_timeseries_file_fail_write_protected_test_() ->
    with_tmp_file(
      "write_protected.ts", 
      fun(File) ->
	      ?cmd("touch " ++ File),
	      ?cmd("chmod ugo-w " ++ File),
	      ?assertMatch({error,_}, tsdb_storage_lib:open_timeseries_file(File)),
	      ?cmd("chmod ugo+w " ++ File),
	      ?cmd("rm " ++ File)
      end).

add_value_test_() ->
    with_tmp_file(
      "aha2.ts",
      fun(File) ->
	      ?cmd("touch " ++ File),
	      ?assertMatch(not_implemented, tsdb_storage_lib:add_value("foo", etu))
      end).

get_values_test_() ->
    with_tmp_file(
      "aha3.ts",
      fun(File) ->
	      ?cmd("touch " ++ File),
	      ?assertMatch(not_implemented, tsdb_storage_lib:get_values("foo", oetn, ste))
      end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tmp_dir() ->
    code:lib_dir(timeseriesDB) ++ "/test/tmp/".

tmp_file(Name) ->
    tmp_dir() ++ Name.

with_tmp_file(Name, Fun) ->
    filelib:ensure_dir(tmp_dir()),
    Tmp = tmp_file(Name),
    {setup,
     fun() -> Tmp end,
     fun(Tmpfile) ->
	     os:cmd("rm " ++ Tmpfile)
     end,
     fun() -> Fun(Tmp) end}.

