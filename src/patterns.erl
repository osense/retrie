%%%-------------------------------------------------------------------
%%% @author xtovarn
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. VIII 2015 10:38
%%%-------------------------------------------------------------------
-module(patterns).
-author("xtovarn").

-include_lib("eunit/include/eunit.hrl").

-define(RE_OPTS, [unicode]).

%% API
-export([new/1, match/2, convert/2]).
-export_type([patterns/0, pattern/0]).

-type patterns() :: list(pattern() | unicode:unicode_binary()).
-type pattern() :: {re:mp(), Name::binary()}.
-type match_result() :: {Match::unicode:unicode_binary(), Rest::unicode:unicode_binary(), Name::binary()} | nomatch.


-spec new(unicode:unicode_binary()) -> patterns().
new(Bin) ->
	RE = <<"(%{[A-Z0-9]+:[a-zA-Z0-9_]+})">>,
	GroupedSplitList = re:split(Bin, RE, [{return, binary}, group, trim, unicode]),
	do_ptl(GroupedSplitList, []).


do_ptl([], Result) ->
	lists:reverse(Result);
do_ptl([[Binary] | T], Result) ->
	do_ptl(T, [Binary | Result]);
do_ptl([[<<"">>, Regex] | T], Result) ->
	do_ptl(T, [extract_re(Regex) | Result]);
do_ptl([[Binary, Regex] | T], Result) ->
	do_ptl(T, [extract_re(Regex), Binary | Result]).

extract_re(Regex) ->
	RE = <<"%{([A-Z0-9]+):([a-zA-Z0-9_]+)}">>,
	{match, [Type, VarName]} = re:run(Regex, RE, [{capture, all_but_first, binary}]),
    {create_re(Type), VarName}.

create_re(<<"STRING">>) ->
    {ok, Mp} = re:compile("\\p{Xan}+", ?RE_OPTS),
    {string, Mp};
create_re(<<"INT">>) ->
    {ok, Mp} = re:compile("[[:digit:]]+", ?RE_OPTS),
    {int, Mp};
create_re(<<"EMAIL">>) ->
    {ok, Mp} = re:compile("[.\\p{Xwd}]+@[.\\p{Xwd}]+", ?RE_OPTS),
    {email, Mp};
create_re(<<"BOOL">>) ->
    {ok, Mp} = re:compile("true|false", ?RE_OPTS),
    {bool, Mp};
create_re(Regex) ->
    {ok, Mp} = re:compile(Regex, ?RE_OPTS),
    {unknown, Mp}.


-spec match(unicode:unicode_binary(), pattern()) -> match_result().
match(Input, {{_, Pattern}, Name}) ->
    case re:run(Input, Pattern) of
        {match, [{0, End}]} -> {binary:part(Input, 0, End), binary:part(Input, End, byte_size(Input) - End), Name};
        _ -> nomatch
    end.


-spec convert(unicode:unicode_binary(), pattern()) -> term().
convert(Input, {{Type, _}, _}) ->
    convert1(Type, Input).

convert1(int, Input) ->
    binary_to_integer(Input);
convert1(bool, <<"false">>) ->
    false;
convert1(bool, <<"true">>) ->
    true;
convert1(_, Input) ->
    Input.


%% Tests
ptl_test() ->
	Expected = [{create_re(<<"STRING">>), <<"val1">>}, <<" hello sshd_">>, {create_re(<<"STRING">>), <<"val1">>}, {create_re(<<"STRING">>), <<"val1">>}, <<" who-ylo ">>, {create_re(<<"INT">>), <<"val2">>}, <<" hello">>],
	Result = new(<<"%{STRING:val1} hello sshd_%{STRING:val1}%{STRING:val1} who-ylo %{INT:val2} hello">>),
	?assertEqual(Expected, Result),
	?assertEqual([<<"ok ">>, {create_re(<<"INT">>), <<"id">>}], new(<<"ok %{INT:id}">>)).
