-module(tsdb_tests).

-include_lib("eunit/include/eunit.hrl").

foo_test() ->
    ?assertMatch(42, tsdb:foo()).
