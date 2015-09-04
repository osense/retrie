-module(patterns_tests).

-include_lib("eunit/include/eunit.hrl").


pattern_to_list_test() ->
    Expected = [{<<"STRING">>, <<"val1">>}, <<" hello sshd_">>, {<<"STRING">>, <<"val1">>}, {<<"STRING">>, <<"val1">>}, <<" who-ylo ">>, {<<"INT">>, <<"val2">>}, <<" hello">>],
    Result = patterns:pattern_to_list(<<"%{STRING:val1} hello sshd_%{STRING:val1}%{STRING:val1} who-ylo %{INT:val2} hello">>),
    ?assertEqual(Expected, Result),
    ?assertEqual([<<"ok ">>, {<<"INT">>, <<"id">>}], patterns:pattern_to_list(<<"ok %{INT:id}">>)).

yaml_load_test() ->
    TM = patterns:load("../test/test.pts"), %% CWD is .eunit
    T = maps:get(test, TM),
    ?assertEqual({<<"hello">>, [{<<"name">>, <<"Abca">>}]}, retrie:lookup_match(<<"Hello, Abca">>, T)),
    ?assertEqual({<<"from">>, [{<<"msg">>, <<"Hiya">>}, {<<"from">>, <<"Abca">>}, {<<"port">>, 34}, {<<"valid">>, true}]},
        retrie:lookup_match(<<"Hiya from Abca:34 is true.">>, T)).
