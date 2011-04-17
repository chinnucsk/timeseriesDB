%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%%
%% @doc This is the main module for the timeseriesDB application.

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

%% @doc Function uses for testing the devopment setup.  Will be
%% removed when we get some real tests.
-spec foo() -> 42.
foo() ->
    42.
