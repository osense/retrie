-module(trie_tests).

-include_lib("eunit/include/eunit.hrl").


lookup_test() ->
    T1 = trie:new(),
    T2 = trie:insert(<<"abc">>, val1, T1),
    ?assertEqual(val1, trie:lookup(<<"abc">>, T2)),
    T3 = trie:insert(<<"a">>, val2, T2),
    T4 = trie:insert(<<"baca">>, val3, T3),
    T5 = trie:insert(<<"abce">>, val4, T4),
    ?assertEqual(val1, trie:lookup(<<"abc">>, T5)),
    ?assertEqual(val2, trie:lookup(<<"a">>, T5)),
    ?assertEqual(val3, trie:lookup(<<"baca">>, T5)),
    ?assertEqual(val4, trie:lookup(<<"abce">>, T5)),
    T6 = trie:insert(<<"a">>, val5, T5),
    T7 = trie:insert(<<"b">>, val6, T6),
    T8 = trie:insert(<<"abcabc">>, val7, T7),
    ?assertEqual(val5, trie:lookup(<<"a">>, T8)),
    ?assertEqual(val6, trie:lookup(<<"b">>, T8)),
    ?assertEqual(val7, trie:lookup(<<"abcabc">>, T8)),
    ?assertEqual(undefined, trie:lookup(<<"ab">>, T8)),
    ?assertEqual(undefined, trie:lookup(<<"xb">>, T8)).
