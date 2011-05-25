-module(tsdb_ram_storage_tests).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok,TestTS} = tsdb_ram_storage:start_link(test),
    ?assertMatch(test, tsdb_ram_storage:name(TestTS)),
    tsdb_ram_storage:stop(TestTS).

add_get_value_test() ->
    {ok,TS} = tsdb_ram_storage:start_link(test),
    T1 = {0,0,1},
    T2 = {0,0,2},
    T3 = {0,0,3},
    T4 = {0,0,4},
    T5 = {0,0,5},
    ok = tsdb_ram_storage:append_value(TS, 1, T1),
    ?assertMatch({ok, [{T1, 1}]}, tsdb_ram_storage:get_values(TS,first_entry, last_entry)),
    ok = tsdb_ram_storage:append_value(TS, 2, T3),
    ?assertMatch({ok, [{T1, 1}, {T3, 2}]}, tsdb_ram_storage:get_values(TS,first_entry, last_entry)),
    ?assertMatch({ok, [{T1, 1}, {T3, 2}]}, tsdb_ram_storage:get_values(TS,T1, T3)),
    ?assertMatch({error, {timestamp_should_be_larger_than,_,_,_}}, tsdb_ram_storage:append_value(TS, 1, T1)),
    ok = tsdb_ram_storage:append_value(TS, 5, T5),
    ?assertMatch({ok, [{T3, 2}, {T5, 5}]}, tsdb_ram_storage:get_values(TS,T3, T5)),
    ?assertMatch({ok, [{T3, 2}]}, tsdb_ram_storage:get_values(TS,T2, T4)).

%% append value with smaller timestamp that the last_entry, should fail
%% Test append/2

%%% Unfinished experiment with proper...
%%% -include_lib("proper/include/proper.hrl").
%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%  Proper init call  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% run_properties_test() ->
%%%     [] = proper:module(?MODULE).
%%%
%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%  Properties  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -define(NAME, ts_name).
%%%
%%% initial_state() ->
%%%     nil.
%%%
%%% command(nil) ->
%%%     oneof([{call,tsdb_ram_storage, name,[?NAME]}]).
%%%
%%% precondition(_, _) ->
%%%     true.
%%%
%%% next_state(State, _Ignore, {call,_,name,[?NAME]}) ->
%%%     State.
%%%
%%% postcondition(nil, {call,_,name,[?NAME]}, ?NAME) ->
%%%     true;
%%% postcondition(_,_,_) ->
%%%     false.
%%%
%%%
%%% %%% Sample properties
%%%
%%% prop_tsdb_ram_storage() ->
%%%     ?FORALL(Cmds, commands(?MODULE),
%%% 	    begin
%%% 		{ok, Pid} = tsdb_ram_storage:start_link(?NAME),
%%% 		{H,S,Res} = run_commands(?MODULE, Cmds),
%%% 		tsdb_ram_storage:stop(Pid),
%%% 		?WHENFAIL(io:format("History: ~p~nState: ~p~nRes: ~p~n", [H,S,Res]),
%%% 			  Res =:= ok)
%%% 	    end).
%%%
%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%  Stack prop test demo  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -type stack(T) :: [T].
%%%
%%% p2rop_push_pop() ->
%%%     ?FORALL({X,S}, {integer(),stack(integer())},
%%% 	    begin
%%% 		{Y,_} = pop(push(X,S)),
%%% 		X =:= Y
%%% 	    end).
%%%
%%% -spec push(T, stack(T)) -> stack(T).
%%% push(X,S) ->
%%%     [X | S].
%%% -spec pop(stack(T)) -> {integer(),T} | error.
%%% pop([]) ->
%%%     error
%%%     ;
%%% pop([X|S]) ->
%%%     {X,S}.
