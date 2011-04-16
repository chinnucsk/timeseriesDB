

-module(tsdb_web).

-include_lib("epgsql/include/pgsql.hrl").

-export([start/1, stop/0, loop/2]).

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
		    "test42" ->
			Req:ok({"text/javascript",
				mochijson2:encode({struct, [{<<"test">>, 42}]})});
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
            error_logger_msg:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

