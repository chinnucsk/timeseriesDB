
%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%%
%% @doc RAM storage for timeseries.
%%

-module(tsdb_ram_storage).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 stop/1,
	 name/1,
	 append_value/2,
	 append_value/3,
	 get_values/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type timeseries_descriptor() :: term().
-type ts_value() :: float().
-type timestamp() :: {integer(), integer(), integer()}.
-type timeseries() :: [{timestamp(), ts_value()}].


-define(SERVER, ?MODULE).

-record(state, {name=undefined,
	       ts=undefined}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Name :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

stop(Name) ->
    gen_server:call(Name, stop).

init([Name]) ->
    {ok, #state{name=Name, ts=gb_trees:empty()}}.

name(Pid) ->
    gen_server:call(Pid, name).
%% @doc Similar to append_value/3, but Timestamp defaults to now.
%%
%% @see append_value/3.
-spec append_value(timeseries_descriptor(), ts_value()) -> ok | {error, term()}.
append_value(Timeseries, Value) ->
    append_value(Timeseries, Value, now()).

%% @doc Add a value to a timeseries.  Timestamp must be older than the
%% last value already in the timeseries.
-spec append_value(timeseries_descriptor(), ts_value(), timestamp()) -> ok | {error, term()}.
append_value(Timeseries, Value, Timestamp) ->
    gen_server:call(Timeseries, {append_value, Value, Timestamp}).

%% @doc Retrives all values from a timeseries in interval, including
%% From and To.
-spec get_values(timeseries_descriptor(),
		 timestamp() | first_entry,
		 timestamp() | last_entry)
		->
			{ok, timeseries()} |
			{error, term()}.
get_values(Timeseries, FromTime, ToTime) ->
    gen_server:call(Timeseries, {get_values, FromTime, ToTime}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(name, _From, State) ->
    Reply = State#state.name,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State};
handle_call({append_value, Value, Timestamp}, _From, State=#state{ts=TS}) ->
    case gb_trees:is_empty(TS) of
	false ->
	    {LatestTimestamp,_} = gb_trees:largest(TS),
	    case Timestamp =< LatestTimestamp of
		true ->
		    {reply, {error, {timestamp_should_be_larger_than, LatestTimestamp, Timestamp, Value}}, State};
		false ->
		    {reply, ok, State#state{ts=gb_trees:insert(Timestamp, Value, TS)}}
	    end;
	true ->
	    {reply, ok, State#state{ts=gb_trees:insert(Timestamp, Value, TS)}}
    end;
handle_call({get_values, first_entry, last_entry}, _From, State=#state{ts=_TS}) ->
    {reply, {ok, gb_trees:to_list(State#state.ts)}, State};
handle_call({get_values, FromTime, ToTime}, _From, State=#state{ts=TS}) ->
    {reply, {ok, extract_interval(FromTime, ToTime, gb_trees:iterator(TS), [])}, State};
handle_call(Request, From, State) ->
    {stop, {error, {unknown_msg, Request, From}}, {error, {unknown_msg, Request, From}}, State}.

extract_interval(FromTime, ToTime, Iter, Acc) ->
    case gb_trees:next(Iter) of
	none ->
	    lists:reverse(Acc);
	{Timestamp, Value, Iter2} ->
	    case FromTime =< Timestamp andalso Timestamp =< ToTime of
		true ->
		    extract_interval(FromTime, ToTime, Iter2, [{Timestamp, Value} | Acc]);
		false ->
		    extract_interval(FromTime, ToTime, Iter2, Acc)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
