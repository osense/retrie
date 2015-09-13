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
    ?assertEqual({p3, [{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}]}, retrie:lookup_match(<<"Hello Fôô false"/utf8>>, T4)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"Hello Fôô false some"/utf8>>, T4)),
    ?assertEqual({p4, [{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}]}, retrie:lookup_match(<<"Hello Fôô false something"/utf8>>, T4)).

match_literal_test() ->
    T1 = retrie:insert_pattern(<<"Hello A">>, a, retrie:new()),
    T2 = retrie:insert_pattern(<<"Hello B">>, b, T1),
    ?assertEqual({a, []}, retrie:lookup_match(<<"Hello A">>, T2)),
    ?assertEqual({b, []}, retrie:lookup_match(<<"Hello B">>, T2)),
    T3 = retrie:insert_pattern(<<"Hello World">>, c, T2),
    ?assertEqual({a, []}, retrie:lookup_match(<<"Hello A">>, T3)),
    ?assertEqual({b, []}, retrie:lookup_match(<<"Hello B">>, T3)),
    ?assertEqual({c, []}, retrie:lookup_match(<<"Hello World">>, T3)),
    T4 = retrie:insert_pattern(<<"Hell, no!">>, d, T3),
    ?assertEqual({a, []}, retrie:lookup_match(<<"Hello A">>, T4)),
    ?assertEqual({b, []}, retrie:lookup_match(<<"Hello B">>, T4)),
    ?assertEqual({c, []}, retrie:lookup_match(<<"Hello World">>, T4)),
    ?assertEqual({d, []}, retrie:lookup_match(<<"Hell, no!">>, T4)).


priority_test() ->
    T1 = retrie:insert_pattern(<<"Hello, %{STRING:name}">>, p1, retrie:new()),
    T2 = retrie:insert_pattern(<<"Hello, %{INT:id}">>, p2, T1),
    ?assertEqual({p1, [{<<"name">>, <<"54">>}]}, retrie:lookup_match(<<"Hello, 54">>, T2)).