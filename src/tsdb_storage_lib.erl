%% @author Peter Mechlenborg <peter.mechlenborg@gmail.com>
%% @author Lars Hesel Christensen <larshesel@gmail.com>
%%
%% @doc Library for storing a timeseries to a file.
%%
%% A timeseries is a sequence af timestamp, value pairs, where values
%% must be floats.


-module(tsdb_storage_lib).
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
open_timeseries_file(_Filename) ->
    case file:open(_Filename, [append]) of 
	{ok, WriteDescr} ->
	    {ok, WriteDescr};
	{error, Reason} ->
	    {error, Reason}
	end.

%% @doc Similar to add_value/3, but Timestamp defaults to now.
%%
%% @see add_value/3.
-spec add_value(timeseries_descriptor(), ts_value()) -> ok | {error, term()} | not_implemented.
add_value(_Timeseries, _Value) ->
    not_implemented.

%% @doc Add a value to a timeseries.  Timestamp must be older than the
%% last value already in the timeseries.
-spec add_value(timeseries_descriptor(), ts_value(), timestamp()) -> ok | {error, term()} | not_implemented.
add_value(_Timeseries, _Value, _Timestamp) ->
    not_implemented.

%% @doc Retrives all values from a timeseries in interval, including
%% From and To.
-spec get_values(timeseries_descriptor(),
		 timestamp() | first_entry,
		 timestamp() | last_entry) ->
			timeseries() |
			{error, term()} |
			not_implemented.
get_values(_Timeseries, _From, _To) ->
    not_implemented.
