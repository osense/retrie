-module(retrie).

-export([new/0, insert_pattern/3, insert_compiled/3, lookup_match/2]).

-type tree() :: tree_node() | tree_leaf().
-type tree_node() :: {value(), array2:array2(), [{patterns:pattern(), tree()}]}.
-type tree_leaf() :: {key(), value()}.

-type key() :: unicode:unicode_binary().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new(), []}.


-spec insert_pattern(unicode:unicode_binary(), value(), tree()) -> tree().
insert_pattern(Binary, Value, Tree) ->
    insert_compiled(patterns:compile(Binary), Value, Tree).

-spec insert_compiled(patterns:patterns(), value(), tree()) -> tree().
insert_compiled([], Val, _) ->
    {<<>>, Val};
insert_compiled(P, Val, {<<NH, NT/binary>>, NodeVal}) ->
    NewNode = {undefined, array2:set(NH, {NT, NodeVal}, array2:new()), []},
    insert_compiled(P, Val, NewNode);
insert_compiled(P, Val, {<<>>, NodeVal}) ->
    NewNode = {NodeVal, array2:new(), []},
    insert_compiled(P, Val, NewNode);
insert_compiled([<<>> | Rest], Val, Tree) ->
    insert_compiled(Rest, Val, Tree);
insert_compiled([<<H, T/bits>>], Val, {NodeVal, Array, Patterns}) ->
    case array2:get(H, Array) of
        undefined -> {NodeVal, array2:set(H, {T, Val}, Array), Patterns};
        Tree1 -> insert_compiled([T], Val, Tree1)
    end;
insert_compiled([<<H, T/bits>> | Rest], Val, {NodeVal, Array, Patterns}) ->
    SubTree = ensure_defined(array2:get(H, Array)),
    {NodeVal, array2:set(H, insert_compiled([T | Rest], Val, SubTree), Array), Patterns};
insert_compiled([Pattern | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewPatterns = case lists:keytake(Pattern, 1, Patterns) of
                      false -> [{Pattern, insert_compiled(Rest, Val, new())} | Patterns];
                      {value, {_Pattern, Tree}, Ps} -> [{Pattern, insert_compiled(Rest, Val, Tree)} | Ps]
                  end,
    {NodeVal, Array, lists:sort(fun({P1, _}, {P2, _}) -> patterns:compare(P1, P2) end, NewPatterns)}.


-spec lookup_match(key(), tree()) -> {value(), [{binary(), term()}]} | nomatch.
lookup_match(<<H, T/bits>>, {_, Array, []}) ->
    case array2:get(H, Array) of
        undefined -> nomatch;
        Tree -> lookup_match(T, Tree)
    end;
lookup_match(<<H, T/bits>> = In, {_, Array, Patterns}) ->
    case array2:get(H, Array) of
        undefined -> lookup_match_patterns(In, Patterns);
        Tree ->
            case lookup_match(T, Tree) of
                nomatch -> lookup_match_patterns(In, Patterns);
                Res -> Res
            end
    end;
lookup_match(Input, {NodeKey, NodeVal}) when Input == NodeKey ->
    {NodeVal, []};
lookup_match(<<>>, {NodeVal, _, _}) when NodeVal /= undefined ->
    {NodeVal, []};
lookup_match(_, _) ->
    nomatch.

lookup_match_patterns(_, []) ->
    nomatch;
lookup_match_patterns(Input, [{Pattern, Tree} | RestPatterns]) ->
    case patterns:match(Input, Pattern) of
        {Match, Rest, Name} ->
            case lookup_match(Rest, Tree) of
                nomatch -> lookup_match_patterns(Input, RestPatterns);
                {Value, Matches} -> {Value, [{Name, patterns:convert(Match, Pattern)} | Matches]}
            end;
        _ -> lookup_match_patterns(Input, RestPatterns)
    end.


%%% Private functions.
ensure_defined(undefined) ->
    new();
ensure_defined(Tree) ->
    Tree.
