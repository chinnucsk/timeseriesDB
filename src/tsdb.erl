
-module(tsdb).

-export([start/0, stop/0, foo/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(crypto),
    application:start(timeseriesDB).

stop() ->
    application:stop(timeseriesDB).

foo() ->
    42.
