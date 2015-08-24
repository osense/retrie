-module(retrie).

-export([new/0, insert/3, insert_pattern/2, lookup/2, lookup_match/2]).

-type tree() :: tree_node() | tree_leaf().
-type tree_node() :: {value(), array2:array2(), [{patterns:pattern(), tree()}]}.
-type tree_leaf() :: {key(), value()}.

-type key() :: unicode:unicode_binary().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new(), []}.


%%% Basic insert and lookup functions.
-spec insert(key(), value(), tree()) -> tree().
insert(<<H, T/binary>>, Value, Tree) ->
    insert1(H, T, Value, Tree).

-spec insert1(integer(), key(), value(), tree()) -> tree().
insert1(H, <<>>, Value, {NodeVal, Array, Patterns}) ->
    NewTuple = case array2:get(H, Array) of
        {_, Array1, Patterns1} -> {Value, Array1, Patterns1};
        {Key1, Value1} ->
            NewNode = {Value, array2:new(), []},
            insert(Key1, Value1, NewNode);
        undefined -> {Value, array2:new(), []}
    end,
    {NodeVal, array2:set(H, NewTuple, Array), Patterns};
insert1(H, T, Value, {NodeVal, Array, Patterns}) ->
    NewTuple = insert(T, Value, array2:get(H, Array)),
    {NodeVal, array2:set(H, NewTuple, Array), Patterns};
insert1(H, T, Value, {NodeKey, NodeVal}) ->
    NewNode = insert(NodeKey, NodeVal, new()),
    insert1(H, T, Value, NewNode);
insert1(H, T, Value, _) ->
    {<<H, T/bits>>, Value}.


-spec lookup(key(), tree()) -> value().
lookup(<<H, T/bits>>, Tree) ->
    lookup1(H, T, Tree);
lookup(<<>>, {Value, _, _}) ->
    Value.

-spec lookup1(integer(), key(), tree()) -> value().
lookup1(H, T, {_, Array, _}) ->
    case array2:get(H, Array) of
        undefined -> undefined;
        Tree -> lookup(T, Tree)
    end;
lookup1(H, T, {<<H, T/bits>>, Value}) ->
    Value.


%%% Functions for working with patterns.
-spec insert_pattern(unicode:unicode_binary(), tree()) -> tree().
insert_pattern(Binary, Tree) ->
    insert_pattern1(patterns:new(Binary), Tree).

-spec insert_pattern1(patterns:patterns(), tree()) -> tree().
insert_pattern1([], _) ->
    {<<>>, undefined};
insert_pattern1(P, {<<NH, NT/binary>>, NodeVal}) ->
    NewNode = {undefined, array2:set(NH, {NT, NodeVal}, array2:new()), []},
    insert_pattern1(P, NewNode);
insert_pattern1(P, {<<>>, NodeVal}) ->
    NewNode = {NodeVal, array2:new(), []},
    insert_pattern1(P, NewNode);
insert_pattern1([Binary], _) when is_binary(Binary) ->
    {Binary, undefined};
insert_pattern1([<<>> | Rest], Tree) ->
    insert_pattern1(Rest, Tree);
insert_pattern1([<<H, T/bits>> | Rest], {NodeVal, Array, Patterns}) ->
    SubTree = ensure_defined(array2:get(H, Array)),
    {NodeVal, array2:set(H, insert_pattern1([T | Rest], SubTree), Array), Patterns};
insert_pattern1([Match | Rest], {NodeVal, Array, Patterns}) ->
    {NodeVal, Array, [{Match, insert_pattern1(Rest, new())} | Patterns]}.


-spec lookup_match(key(), tree()) -> list({binary(), term()}) | nomatch.
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
lookup_match(Input, {NodeKey, _}) when Input == NodeKey ->
    [];
lookup_match(_, _) ->
    nomatch.

lookup_match_patterns(_, []) ->
    nomatch;
lookup_match_patterns(Input, [{Pattern, Tree} | RestPatterns]) ->
    case patterns:match(Input, Pattern) of
        {Match, Rest, Name} ->
            case lookup_match(Rest, Tree) of
                nomatch -> lookup_match_patterns(Input, RestPatterns);
                Matches -> [{Name, patterns:convert(Match, Pattern)} | Matches]
            end;
        _ -> lookup_match_patterns(Input, RestPatterns)
    end.


%%% Private functions.
ensure_defined(undefined) ->
    new();
ensure_defined(Tree) ->
    Tree.
