

-module(tsdb_web).

%%-export([start/1, stop/0, loop/2]).

-compile([export_all]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            'GET' ->
                case Path of
		    "timeseries/nprocs.json" ->
			Req:ok({"text/javascript", ts_to_json() });
		    _ ->
			error_logger:info_msg("Serving file: ~p", [Path]),
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
		    "testinput" ->
			error_logger:info_msg("---------------------- ~p~n", [Req:parse_post()]),
			Req:ok({"text/plain",[],"test42"});
                    _ ->
			error_logger:info_msg("error ----------------------"),
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

ts_to_json() ->
    {ok, Values} = tsdb_file_storage:get_values(nprocs_ts, first_entry, last_entry),
    Foo = lists:map(fun({Now = {_, _, Micro}, V}) ->
			    {_Date = {D1,D2,D3}, _Time = {T1,T2,T3}} = calendar:now_to_datetime(Now),
			    {struct, [{<<"ts">>, [D1,D2,D3,T1,T2,T3,Micro]},
				      {<<"value">>, V}]}
		    end,
		    Values),
    io:format("~p~n", [Foo]),
    mochijson2:encode(Foo).
