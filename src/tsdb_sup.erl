
-module(tsdb_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Web = web_specs(tsdb_web, 8080),
    Processes = [Web],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {127,0,0,1}},
                 {port, Port},
                 {docroot, code:priv_dir(tsdb) ++ "/www/"}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
