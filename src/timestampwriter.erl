-module(timestampwriter).
-export([write/1]).

convert_to_unix_time({Mega,Sec,Micro}) ->
    (Mega*1000000+Sec)*1000000+Micro.

write(Unixtimestamp, Value) when is_float(Value) ->
    {ok, WriteDescr} = file:open(integer_to_list(Unixtimestamp), [raw, append]),
    file:write(WriteDescr,float_to_list(Value)),
    file:close(WriteDescr).

write(Value) ->
    write(convert_to_unix_time(erlang:now()), Value).

