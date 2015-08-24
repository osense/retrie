-module(retrie_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


lookup_test() ->
    T1 = retrie:new(),
    T2 = retrie:insert(<<"abc">>, val1, T1),
    ?assertEqual(val1, retrie:lookup(<<"abc">>, T2)),
    T3 = retrie:insert(<<"a">>, val2, T2),
    T4 = retrie:insert(<<"baca">>, val3, T3),
    T5 = retrie:insert(<<"abce">>, val4, T4),
    ?assertEqual(val1, retrie:lookup(<<"abc">>, T5)),
    ?assertEqual(val2, retrie:lookup(<<"a">>, T5)),
    ?assertEqual(val3, retrie:lookup(<<"baca">>, T5)),
    ?assertEqual(val4, retrie:lookup(<<"abce">>, T5)),
    T6 = retrie:insert(<<"a">>, val5, T5),
    T7 = retrie:insert(<<"b">>, val6, T6),
    T8 = retrie:insert(<<"abcabc">>, val7, T7),
    ?assertEqual(val5, retrie:lookup(<<"a">>, T8)),
    ?assertEqual(val6, retrie:lookup(<<"b">>, T8)),
    ?assertEqual(val7, retrie:lookup(<<"abcabc">>, T8)),
    ?assertEqual(undefined, retrie:lookup(<<"ab">>, T8)),
    ?assertEqual(undefined, retrie:lookup(<<"xb">>, T8)).

match_test() ->
    T0 = retrie:insert(<<"Hello">>, val, retrie:new()),
    T1 = retrie:insert_pattern(<<"%{STRING:name}, hello!">>, T0),
    ?assertEqual([{<<"name">>, <<"World">>}], retrie:lookup_match(<<"World, hello!">>, T1)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T1)),
    T11 = retrie:insert_pattern(<<"Hello, %{STRING:name}">>, T1),
    ?assertEqual([{<<"name">>, <<"hello">>}], retrie:lookup_match(<<"Hello, hello">>, T11)),
    T12 = retrie:insert_pattern(<<"Hel%{STRING:rest}">>, T1),
    ?assertEqual([{<<"rest">>, <<"loo">>}], retrie:lookup_match(<<"Helloo">>, T12)),

    T2 = retrie:insert_pattern(<<"Hello %{STRING:name} id: %{INT:id}">>, T1),
    ?assertEqual([{<<"name">>, <<"Foo">>}, {<<"id">>, 34}], retrie:lookup_match(<<"Hello Foo id: 34">>, T2)),
    T3 = retrie:insert_pattern(<<"Hello %{STRING:name} %{BOOL:b}">>, T2),
    ?assertEqual([{<<"name">>, <<"Foo">>}, {<<"b">>, false}], retrie:lookup_match(<<"Hello Foo false">>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"Hello Foo nobool">>, T3)).
