
%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%% @author Lars Hesel Christensen <larshesel@gmail.com>
%%
%% @doc Library for storing a timeseries to a file.
%%
%% A timeseries is a sequence af timestamp, value pairs, where values
%% must be floats.


-module(tsdb_file_storage).
-export([start_link/1, 
	 append_value/2,
	 append_value/3,
	 get_values/3]).

-type timeseries_descriptor() :: term().
-type ts_value() :: float().
-type timestamp() :: {integer(), integer(), integer()}.
-type timeseries() :: [{timestamp(), ts_value()}].


-spec start_link(Name :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link(Timeseries) ->
    Pid = spawn(fun() ->
		  {ok, LastEntryTimeStamp} = get_last_timestamp_from_file(Timeseries),
		  {ok, Device} = file:open(Timeseries, [append]),
		  loop(Timeseries, Device, LastEntryTimeStamp) 
		end),
    Pid,
    true = erlang:register(list_to_atom(Timeseries), Pid),
    {ok, Pid}.
    

%% @doc Similar to add_value/3, but Timestamp defaults to now.
%%
%% @see add_value/3.
-spec append_value(timeseries_descriptor(), ts_value()) -> ok | {error, term()}.
append_value(Timeseries, Value) -> 
    Timeseries ! {write, erlang:now(), Value},
    ok.


%% @doc Add a value to a timeseries.  Timestamp must be more recent than the
%% last value already in the timeseries.
-spec append_value(timeseries_descriptor(), ts_value(), timestamp()) -> ok | {error, term()}.
append_value(Timeseries, Value, Timestamp) ->
    Timeseries ! {write, Timestamp, Value},
    ok.

%% @doc Retrives all values from a timeseries in interval, including
%% From and To.
-spec get_values(timeseries_descriptor(),
		 timestamp() | first_entry,
		 timestamp() | last_entry) ->
			timeseries() |
			{error, term()}.
get_values(Timeseries, From, To) ->
    flush(),
    Timeseries ! {get, self(), From, To},
    receive
    	{ok, Results} -> {ok, Results}
    after 2000 -> {error, timeout}
    end.

loop(Filename, Device, OldTime) ->
    receive
	{write, NewTime, Value} ->
	    case (OldTime < NewTime) of
		true -> 
		    ok = file:write(Device, format_entry(NewTime, Value)),
		    loop(Filename, Device, NewTime);
		false -> 
		    loop(Filename, Device, OldTime)
	    end;
	{get, Pid, From, To} ->
	    Pid ! {ok, handle_get(Filename, From, To)},
	    loop(Filename, Device, OldTime)
    end.

handle_get(Filename, From, To) ->
    {ok, ReadDevice} = file:open(Filename, [read]),
    extract_from_file_result(ReadDevice, From, To, []).

extract_from_file_result(Device, From, To, Result) ->
    case file:read_line(Device) of
	eof -> lists:reverse(Result);
	{ok, Line} -> 
	    {ok, Entry, Value} =
		parse_entry_from_line(Line),
	    {IntValue, _} = string:to_integer(Value),

	    case true of %From =< Entry andalso To >= Entry of
		true -> 
		    extract_from_file_result(Device, From, To, [{Entry, IntValue}|Result]);
		false ->
		    extract_from_file_result(Device, From, To, Result)
	    end
    end.

%% Auxilliary functions
%%

flush() ->
    receive
	{ok, R} -> {ok,R}
    after 0 -> ok
    end.

get_last_timestamp_from_file(Filename) ->
    %% opening the file for write is a hack, it should just just check
    %% if the file exists instead.
    {ok, Device} = file:open(Filename, [raw, read, write]),
    case get_last_line_from_file(Device) of
	"" ->
	    Result = {ok, {0,0,0}};
	Line -> 
	    {ok, Entry, _} = parse_entry_from_line(Line), 
	    Result = {ok, Entry}
    end,
    ok = file:close(Device), 
    Result.

get_last_line_from_file(Device) ->
    get_last_line_from_file_acc(Device, "").

get_last_line_from_file_acc(Device, Accum) ->
    case file:read_line(Device) of
	eof -> Accum;
	{ok, Line} -> get_last_line_from_file_acc(Device, Line)
    end.

format_entry(Time, Value)  -> 
    lists:flatten(io_lib:format("~p ~p~n", [Time, Value])).

parse_entry_from_line(Line) ->
    [MegaSecs, Secs, Microsecs, Value] = string:tokens(Line, "{} ,"),
    {ok, {list_to_integer(MegaSecs),list_to_integer(Secs), list_to_integer(Microsecs)}, Value}.


