
-module(tsdb_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [web_server(), tsdb_os_mon(), nprocs_ts()],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_server() ->
    Cfg = [{ip,      {127,0,0,1}},
	   {port,    8080},
	   {docroot, code:priv_dir(timeseriesDB) ++ "/www/"}],
    Mfa = {tsdb_web, start, [Cfg]},
    ModName = tsdb_web,
    {ModName, Mfa, permanent, 5000, worker, dynamic}.

tsdb_os_mon() ->
    ModName = tsdb_os_mon,
    Cfg = [ModName],
    Mfa = {tsdb_os_mon, start_link, Cfg},
    {ModName, Mfa, permanent, 5000, worker, dynamic}.

nprocs_ts() ->
    Cfg = [nprocs_ts],
    Mfa = {tsdb_ram_storage, start_link, Cfg},
    ModName = nprocs_ts,
    {ModName, Mfa, permanent, 5000, worker, dynamic}.

