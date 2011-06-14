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
    file:open(Filename, [append]).

%% @doc Similar to add_value/3, but Timestamp defaults to now.
%%
%% @see add_value/3.
-spec add_value(timeseries_descriptor(), ts_value()) -> ok | {error, term()} | not_implemented.
add_value(Timeseries, Value) -> 
    file:write(Timeseries,  format_entry(erlang:now(), Value)).

%% @doc Add a value to a timeseries.  Timestamp must be more recent than the
%% last value already in the timeseries.
-spec add_value(timeseries_descriptor(), ts_value(), timestamp()) -> ok | {error, term()}.
add_value(Timeseries, Value, Timestamp) ->
    file:write(Timeseries,  format_entry(Timestamp, Value)).

%% @doc Retrives all values from a timeseries in interval, including
%% From and To.
-spec get_values(timeseries_descriptor(),
		 timestamp() | first_entry,
		 timestamp() | last_entry) ->
			timeseries() |
			{error, term()}.
get_values(_Timeseries, _From, _To) ->
   not_implemented.

format_entry(Time, Value)  -> 
    lists:flatten(io_lib:format("~p ~p~n", [Time, Value])).

parse_entry_from_line(Line) ->
    [MegaSecs, Secs, Microsecs, Value] = string:tokens(Line, "{} ,"),
    {ok, {list_to_integer(MegaSecs),list_to_integer(Secs), list_to_integer(Microsecs)}, Value}.

    
