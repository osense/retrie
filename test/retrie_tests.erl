-module(retrie_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


match_test() ->
    T1 = retrie:insert_pattern(<<"%{STRING:name}, helló!"/utf8>>, p1, retrie:new()),
    ?assertEqual({p1, [{<<"name">>, <<"World">>}]}, retrie:lookup_match(<<"World, helló!"/utf8>>, T1)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T1)),
    T11 = retrie:insert_pattern(<<"Hello, %{STRING:name}">>, p11, T1),
    ?assertEqual({p11, [{<<"name">>, <<"helló"/utf8>>}]}, retrie:lookup_match(<<"Hello, helló"/utf8>>, T11)),
    T12 = retrie:insert_pattern(<<"Hel%{STRING:rest}">>, p12, T1),
    ?assertEqual({p12, [{<<"rest">>, <<"loo">>}]}, retrie:lookup_match(<<"Helloo">>, T12)),

    T2 = retrie:insert_pattern(<<"Hello %{STRING:name} id: %{INT:id}">>, p2, T1),
    ?assertEqual({p2, [{<<"name">>, <<"Foo">>}, {<<"id">>, 34}]}, retrie:lookup_match(<<"Hello Foo id: 34">>, T2)),
    T3 = retrie:insert_pattern(<<"Hello %{STRING:name} %{BOOL:b}">>, p3, T2),
    ?assertEqual({p3, [{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}]}, retrie:lookup_match(<<"Hello Fôô false"/utf8>>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"Hello Foo nobool">>, T3)),

    T4 = retrie:insert_pattern(<<"Hello %{STRING:name} %{BOOL:b} something">>, p4, T3),
    T41 = retrie:compile(T4),
    ?assertEqual({p3, [{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}]}, retrie:lookup_match(<<"Hello Fôô false"/utf8>>, T41)),
    ?assertEqual({p4, [{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}]}, retrie:lookup_match(<<"Hello Fôô false something"/utf8>>, T41)).

