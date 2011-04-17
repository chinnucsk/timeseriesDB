-module(timestampwriter).
-export([write/1]).

convert_to_unix_time({Mega,Sec,Micro}) ->
    (Mega*1000000+Sec)*1000000+Micro.

write(Unixtimestamp, Value) when is_float(Value) ->
    case file:open(integer_to_list(Unixtimestamp), [raw, append]) of 
	{ok, WriteDescr} ->
	    file:write(WriteDescr,float_to_list(Value)),
	    file:close(WriteDescr);
	{error, Reason} ->
	    log("Error", [Reason])
	end.

write(Value) ->
    write(convert_to_unix_time(erlang:now()), Value).


log(_Val, Reason) ->
    Reason.
