-module(retrie_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


lookup_test() ->
    T1 = retrie:new(),
    T2 = retrie:insert(<<"abc">>, val1, T1),
    ?assertEqual(val1, retrie:lookup(<<"abc">>, T2)),
    T3 = retrie:insert(<<"a">>, val2, T2),
    T4 = retrie:insert(<<"baca">>, val3, T3),
    T5 = retrie:insert(<<"abced">>, val4, T4),
    ?assertEqual(val2, retrie:lookup(<<"a">>, T5)),
    ?assertEqual(val3, retrie:lookup(<<"baca">>, T5)),
    ?assertEqual(val4, retrie:lookup(<<"abced">>, T5)),
    T6 = retrie:insert(<<"a">>, val5, T5),
    T7 = retrie:insert(<<"b">>, val6, T6),
    T8 = retrie:insert(<<"abcabc">>, val7, T7),
    ?assertEqual(val5, retrie:lookup(<<"a">>, T8)),
    ?assertEqual(val6, retrie:lookup(<<"b">>, T8)),
    ?assertEqual(val7, retrie:lookup(<<"abcabc">>, T8)),
    ?assertEqual(undefined, retrie:lookup(<<"ab">>, T8)),
    ?assertEqual(undefined, retrie:lookup(<<"xb">>, T8)).
