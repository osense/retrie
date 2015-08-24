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

%% API
-export([new/1, match/2, convert/2, get_name/1]).
-export_type([patterns/0, text/0]).

-type patterns() :: list(pattern() | text()).
-type pattern() :: {PatternFun::fun((text()) -> match_result()), Name::binary()}.
-type match_result() :: {Match::char(), Rest::text()} | nomatch.

-type text() :: unicode:unicode_binary().


-spec new(text()) -> patterns().
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
	{binary_to_atom(Type, latin1), VarName}.


-spec match(text(), pattern()) -> match_result().
match(Input, {Type, _}) ->
    match1(Type, Input).

match1('STRING', <<First/utf8, Rest/bits>>) ->
    {<<First>>, Rest};
match1('INT', <<First, Rest/bits>>) when First >= 48, First =< 57 ->
    {<<First>>, Rest};
match1('BOOL', <<"false", Rest/bits>>) -> 
    {<<"false">>, Rest};
match1('BOOL', <<"true", Rest/bits>>) ->
    {<<"true">>, Rest};
match1(_, _) ->
    nomatch.


-spec convert(text(), pattern()) -> term().
convert(Match, {Type, _}) ->
    convert1(Type, Match).

convert1('INT', Match) ->
    binary_to_integer(Match);
convert1('BOOL', <<"true">>) ->
    true;
convert1('BOOL', <<"false">>) ->
    false;
convert1(_, Match) ->
    Match.



-spec get_name(pattern()) -> text().
get_name({_, Name}) ->
    Name.


%% Tests
ptl_test() ->
	Expected = [{'STRING', <<"val1">>}, <<" hello sshd_">>, {'STRING', <<"val1">>}, {'STRING', <<"val1">>}, <<" who-ylo ">>, {'INT', <<"val2">>}, <<" hello">>],
	Result = new(<<"%{STRING:val1} hello sshd_%{STRING:val1}%{STRING:val1} who-ylo %{INT:val2} hello">>),
	?assertEqual(Expected, Result),
	?assertEqual([<<"ok ">>, {'INT', <<"id">>}], new(<<"ok %{INT:id}">>)).
