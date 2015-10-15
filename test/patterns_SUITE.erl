-module(patterns_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, yaml_load_test/1]).

all() -> [yaml_load_test].


yaml_load_test(Config) ->
    T = retrie_patterns:load_group(filename:join(?config(data_dir, Config), "test.yaml"), <<"test">>),
    {<<"hello">>, [{<<"name">>, <<"Abca">>}]} = retrie:lookup_match(<<"Hello, Abca">>, T),
    {<<"from">>, [{<<"msg">>, <<"Hiya">>},
                  {<<"from">>, <<"Abca">>}, 
                  {<<"port">>, 34},
                  {<<"valid">>, true}]} = retrie:lookup_match(<<"Hiya from Abca:34 is true.">>, T).
