-module(retrie).

-export([new/0, insert_pattern/2, lookup_match/2]).

-type tree() :: tree_node() | tree_leaf().
-type tree_node() :: {value(), array2:array2(), [{patterns:pattern(), tree()}]}.
-type tree_leaf() :: {key(), value()}.

-type key() :: unicode:unicode_binary().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new(), []}.


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
