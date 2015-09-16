%%%-------------------------------------------------------------------
%%% @author xtovarn
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. VIII 2015 10:38
%%%-------------------------------------------------------------------
-module(retrie_patterns).
-author("xtovarn").

-include_lib("eunit/include/eunit.hrl").

-define(RE_COMPILE_OPTS, [unicode]).
-define(YAML_REGEX_NAME, "regexes").
-define(YAML_PATTERN_NAME, "patterns").


%%% API
-export([compile/1, compile/2, load_group/2, pattern_to_list/1, match/2, compare/2, convert/2, unicode_to_units/1]).
-export_type([patterns/0, pattern/0]).

-type patterns() :: list(pattern() | string()).
-type pattern() :: {{Priority::integer(), type(), Regex::any()}, Name::binary()}.
-type type() :: string | integer | float | boolean.
-type match_result() :: {Match::string(), Rest::string(), Name::string()} | nomatch.


-spec compile(string() | unicode:unicode_binary()) -> patterns().
compile(Input) ->
    compile(Input, create_regexes(default_regex_dict())).

-spec compile(string() | unicode:unicode_binary(), map()) -> patterns().
compile(Input, RegexBindings) ->
    lists:map(fun
                  ({RegexIdent, Name}) ->
                     {maps:get(RegexIdent, RegexBindings), Name};
                  (String) ->
                     unicode_to_units(String)
             end, pattern_to_list(Input)).


-spec load_group(file:name_all(), binary()) -> map(). %% Map of group_name => retrie.
load_group(Filename, Groupname) ->
    ok = application:ensure_started(yamerl),
    [Data] = yamerl_constr:file(Filename),
    {_, Patterns} = lists:keyfind(?YAML_PATTERN_NAME, 1, Data),
    {_, Group} = lists:keyfind(Groupname, 1, Patterns),
    {_, Regexes} = lists:keyfind(?YAML_REGEX_NAME, 1, Data),
    CompRegexes = create_regexes(Regexes),
    lists:foldl(fun({PatternName, Pattern}, AccTree) ->
                        retrie:insert_compiled(compile(Pattern, CompRegexes), PatternName, AccTree)
                end, retrie:new(), Group).


-spec pattern_to_list(string() | unicode:unicode_binary()) -> patterns().
pattern_to_list(Bin) ->
    RE = <<"(%{[A-Z0-9]+:[a-zA-Z0-9_]+})">>,
    GroupedSplitList = re:split(Bin, RE, [{return, list}, group, trim, unicode]),
    do_ptl(GroupedSplitList, []).

do_ptl([], Result) ->
    lists:reverse(Result);
do_ptl([[Binary] | T], Result) ->
    do_ptl(T, [Binary | Result]);
do_ptl([["", Regex] | T], Result) ->
    do_ptl(T, [extract_re(Regex) | Result]);
do_ptl([[Binary, Regex] | T], Result) ->
    do_ptl(T, [extract_re(Regex), Binary | Result]).

extract_re(Regex) ->
    RE = <<"%{([A-Z0-9]+):([a-zA-Z0-9_]+)}">>,
    {match, [Type, VarName]} = re:run(Regex, RE, [{capture, all_but_first, list}]),
    {Type, VarName}.


-spec match(unicode:unicode_binary(), pattern()) -> match_result().
match(Input, {{_, _, Pattern}, Name}) ->
    case re:run(Input, Pattern, [{capture, first, list}]) of
        {match, [Match]} ->
            {Match, Input -- Match, Name};
        _ -> nomatch
    end.


-spec compare(pattern(), pattern()) -> boolean().
compare({{A, _, _}, _}, {{B, _, _}, _}) ->
    A =< B.


-spec convert(unicode:unicode_binary(), pattern()) -> term().
convert(Input, {{_, Type, _}, _}) ->
    convert1(Type, Input).

convert1(string, Input) ->
    Input;
convert1(integer, Input) ->
    list_to_integer(Input);
convert1(float, Input) ->
    list_to_float(Input);
convert1(boolean, "true") ->
    true;
convert1(boolean, "false") ->
    false.


char_to_units(C) when C > 255 ->
    binary_to_list(unicode:characters_to_binary([C]));
char_to_units(C) ->
    [C].

unicode_to_units(List) when is_list(List) ->
    lists:flatten(lists:map(fun char_to_units/1, List)).


%%% Private functions.
-spec create_regexes(list({RName::string(), Regex::list()})) -> map().
create_regexes(Orddict) ->
    {Res, _} = orddict:fold(fun(RName, [Type, Regex], {Acc, Priority}) ->
                                    {ok, Mp} = re:compile(Regex, ?RE_COMPILE_OPTS),
                                    {maps:put(RName, {Priority, list_to_atom(Type), Mp}, Acc),
                                     Priority + 1}
                            end, {#{}, 0}, Orddict),
    Res.

default_regex_dict() ->
    [{"STRING", ["string", "[\\p{L}0-9]+"]},
     {"INT", ["integer", "[-+]?[[:digit:]]+"]},
     {"FLOAT", ["float", "[-+]?([[:digit:]]\.)?[[digit]]+"]},
     {"BOOL", ["boolean", "true|false"]}].