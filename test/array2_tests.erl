-module(array2_tests).

-include_lib("eunit/include/eunit.hrl").


to_list_test() ->
    ?assertEqual([], array2:to_list(array2:new())),
    ?assertEqual([a, b, c, d], array2:to_list(sample(1))).


to_orddict_test() ->
    ?assertEqual([], array2:to_orddict(array2:new())),
    ?assertEqual([{1, a}, {10, b}, {11, c}, {15, d}], array2:to_orddict(sample(1))).

sample(1) ->
    A1 = array2:set(10, b, array2:new()),
    A2 = array2:set(15, d, A1),
    A3 = array2:set(11, c, A2),
    array2:set(1, a, A3).
