#!/usr/bin/env escript
%%! -pa ebin deps/re2/ebin

%% To be executed from the main directory.

-mode(compile).

-include("bench.hrl").

-define(TRIALS, 1000000).


main(_) ->
    io:format("=  re  ==================================~n"),
    bench_regex(),
    io:format("=  retrie  ==============================~n"),
    bench_retrie().


bench_regex() ->
    BenchF = fun(Regexes, Input, N) ->
                     Name = integer_to_list(length(Regexes)) ++ " patterns, " ++ integer_to_list(N) ++ " captures" ,
                     bench(Name, fun() ->
                                         lists:any(fun(Re) ->
                                                           re:run(Input, Re, [{capture, all_names, binary}]) /= nomatch
                                                   end, Regexes)
                                 end, ?TRIALS)
             end,
    {ok, R1} = re:compile(<<"Hello, (?P<name>\\p{L}+)">>),
    {ok, R2} = re:compile(<<"Hey (?P<name>\\p{L}+), hello!">>),
    {ok, R3} = re:compile(<<"Hi (?P<name>\\p{L}+), you are (?P<age>[0-9]+) years old.">>),
    BenchF([R1, R2, R3], <<"Hi name, you are 4 years old.">>, 2),

    {ok, R4} = re:compile(<<"yeah id=(?P<id>[0-9]+) from=(?P<from>\\p{L}+) (?P<port>[0-9]+)">>),
    BenchF([R1, R2, R3, R4], <<"yeah id=54 from=abcde 22">>, 3),

    {ok, R5} = re:compile(<<"Hello from (?P<name>\\p{L}+)">>),
    {ok, R6} = re:compile(<<"from=(?P<from>\\p{L}+) to=(?P<to>\\p{L}+) valid=(?P<valid>true|false)">>),
    {ok, R7} = re:compile(<<"(?P<from>\\p{L}+) says to (?P<to>\\p{L}+): (?P<msg>\\p{L}+)">>),
    BenchF([R1, R2, R3, R4, R5, R6, R7], <<"someone says to another: yeahyeah">>, 3).


bench_retrie() ->
    T1 = retrie:insert_pattern(<<"Hello, %{STRING:name}!">>, p1, retrie:new()),
    T2 = retrie:insert_pattern(<<"Hey %{STRING:name}, hello!">>, p2, T1),
    T3 = retrie:insert_pattern(<<"Hi %{STRING:name}, you are %{INT:age} years old.">>, p3, T2),
    bench("3 patterns, 2 captures", fun() -> retrie:lookup_match(<<"Hi name, you are 34 years old.">>, T3) end, ?TRIALS),

    T4 = retrie:insert_pattern(<<"yeah id=%{INT:id} from=%{STRING:from}:%{INT:port}">>, p4, T3),
    bench("4 patterns, 3 captures", fun() -> retrie:lookup_match(<<"yeah id=54 from=abcde:22">>, T4) end, ?TRIALS),

    T5 = retrie:insert_pattern(<<"Hello from %{STRING:name}">>, p5, T4),
    T6 = retrie:insert_pattern(<<"from=%{STRING:from} to=%{STRING:to} valid=%{BOOL:valid}">>, p6, T5),
    T7 = retrie:insert_pattern(<<"%{STRING:from} says to %{STRING:to}: %{STRING:msg}">>, p7, T6),
    bench("7 patterns, 3 captures", fun() -> retrie:lookup_match(<<"someone says to another: yeahyeah">>, T7) end, ?TRIALS).
