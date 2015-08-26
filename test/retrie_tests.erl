-module(retrie_tests).
-author("osense").

-include_lib("eunit/include/eunit.hrl").


match_test() ->
    T1 = retrie:insert_pattern(<<"%{STRING:name}, helló!"/utf8>>, retrie:new()),
    ?assertEqual([{<<"name">>, <<"World">>}], retrie:lookup_match(<<"World, helló!"/utf8>>, T1)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T1)),
    T11 = retrie:insert_pattern(<<"Hello, %{STRING:name}">>, T1),
    ?assertEqual([{<<"name">>, <<"hello">>}], retrie:lookup_match(<<"Hello, hello">>, T11)),
    T12 = retrie:insert_pattern(<<"Hel%{STRING:rest}">>, T1),
    ?assertEqual([{<<"rest">>, <<"loo">>}], retrie:lookup_match(<<"Helloo">>, T12)),

    T2 = retrie:insert_pattern(<<"Hello %{STRING:name} id: %{INT:id}">>, T1),
    ?assertEqual([{<<"name">>, <<"Foo">>}, {<<"id">>, 34}], retrie:lookup_match(<<"Hello Foo id: 34">>, T2)),
    T3 = retrie:insert_pattern(<<"Hello %{STRING:name} %{BOOL:b}">>, T2),
    ?assertEqual([{<<"name">>, <<"Fôô"/utf8>>}, {<<"b">>, false}], retrie:lookup_match(<<"Hello Fôô false"/utf8>>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"World, World!">>, T3)),
    ?assertEqual(nomatch, retrie:lookup_match(<<"Hello Foo nobool">>, T3)).
