
%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%% @author Lars Hesel Christensen <larshesel@gmail.com>
%%
%% @doc Library for storing a timeseries to a file.
%%
%% A timeseries is a sequence af timestamp, value pairs, where values
%% must be floats.


-module(tsdb_file_storage).
-export([open_timeseries_file/1,
	 add_value/2,
	 add_value/3,
	 get_values/3]).

-type filename() :: string().
-type timeseries_descriptor() :: term().
-type ts_value() :: float().
-type timestamp() :: {integer(), integer(), integer()}.
-type timeseries() :: [{timestamp(), ts_value()}].

%% @doc Opens or creates a timeseries.  If file exists it is opened so
%% more data can be appended to the timeseries.  If the file does not
%% exist, it is created and opened.
-spec open_timeseries_file(filename()) -> timeseries_descriptor().
open_timeseries_file(Filename) -> 
    {ok, start(Filename)}.

%% @doc Similar to add_value/3, but Timestamp defaults to now.
%%
%% @see add_value/3.
-spec add_value(timeseries_descriptor(), ts_value()) -> ok | {error, term()}.
add_value(Timeseries, Value) -> 
    Timeseries ! {write, erlang:now(), Value},
    ok.


%% @doc Add a value to a timeseries.  Timestamp must be more recent than the
%% last value already in the timeseries.
-spec add_value(timeseries_descriptor(), ts_value(), timestamp()) -> ok | {error, term()}.
add_value(Timeseries, Timestamp, Value) ->
    Timeseries ! {write, Timestamp, Value},
    ok.

%% @doc Retrives all values from a timeseries in interval, including
%% From and To.
-spec get_values(timeseries_descriptor(),
		 timestamp() | first_entry,
		 timestamp() | last_entry) ->
			timeseries() |
			{error, term()}.
get_values(_Timeseries, _From, _To) ->
   not_implemented.

start(Filename) ->
    spawn(fun() ->
		  {ok, LastEntryTimeStamp} = get_last_timestamp_from_file(Filename),
		  {ok, Device} = file:open(Filename, [append]),
		  loop(Device, LastEntryTimeStamp) 
	  end).

loop(Device, OldTime) ->
    receive
	{write, NewTime, Value} ->
	    case (OldTime < NewTime) of
		true -> 
		    ok = file:write(Device, format_entry(NewTime, Value)),
		    loop(Device, NewTime);
		false -> 
		    loop(Device, OldTime)
	    end
    end.



%% Auxilliary functions
%%

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


