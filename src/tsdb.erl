%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%%
%% @doc This is the main module for the timeseriesDB application.

-module(tsdb).

-export([start/0, stop/0]).

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
