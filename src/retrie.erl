-module(retrie).

-export([new/0, insert_pattern/3, insert_compiled/3, lookup_match/2]).

-type tree() :: tree_node() | tree_leaf().
-type tree_node() :: {value(), array2:array2(), patterns()}.
-type tree_leaf() :: {leaf, key(), value(), patterns()}.
-type patterns() :: [{retrie_patterns:pattern(), tree()}].

-type key() :: unicode:unicode_binary().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new(), []}.


-spec insert_pattern(unicode:unicode_binary(), value(), tree()) -> tree().
insert_pattern(Binary, Value, Tree) ->
    insert_compiled(retrie_patterns:compile(Binary), Value, Tree).

-spec insert_compiled(retrie_patterns:patterns(), value(), tree()) -> tree().
insert_compiled([], Val, {leaf, LeafKey, _, Patterns}) ->
    {leaf, LeafKey, Val, Patterns};
insert_compiled([], Val, {_, Array, Patterns}) ->
    {Val, Array, Patterns};
insert_compiled([Binary | Rest], Val, {leaf, LeafKey, _, _} = Leaf) when Binary == LeafKey ->
    io:format("~p; ~p~n", [Binary, LeafKey]),
    insert_compiled(Rest, Val, Leaf);
insert_compiled([<<>> | Rest], Val, {leaf, <<LH, LT/bits>>, LeafVal, Patterns}) ->
    NewNode = {undefined, array2:set(LH, {leaf, LT, LeafVal, Patterns}, array2:new()), []},
    insert_compiled(Rest, Val, NewNode);
insert_compiled([<<>> | Rest], Val, Node) ->
    insert_compiled(Rest, Val, Node);
insert_compiled([<<H, T/bits>> | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewTree = case array2:get(H, Array) of
                  undefined  -> insert_compiled(Rest, Val, {leaf, T, undefined, []});
                  Tree -> insert_compiled([T | Rest], Val, Tree)
              end,
    {NodeVal, array2:set(H, NewTree, Array), Patterns};
insert_compiled([B | Rest], Val, {leaf, <<LH, LT/bits>>, LeafVal, Patterns}) when is_binary(B) ->
    NewNode = {undefined, array2:set(LH, {leaf, LT, LeafVal, Patterns}, array2:new()), []},
    insert_compiled([B | Rest], Val, NewNode);
insert_compiled([Pattern | Rest], Val, Tree) ->
    Patterns = get_patterns(Tree),
    NewPatterns = case lists:keytake(Pattern, 1, Patterns) of
                      false -> [{Pattern, insert_compiled(Rest, Val, new())} | Patterns];
                      {value, {_Pattern, Tree1}, Ps} -> [{Pattern, insert_compiled(Rest, Val, Tree1)} | Ps]
                  end,
    SortedPatterns = lists:sort(fun({P1, _}, {P2, _}) -> retrie_patterns:compare(P1, P2) end, NewPatterns),
    put_patterns(SortedPatterns, Tree).


-spec lookup_match(key(), tree()) -> {value(), [{binary(), term()}]} | nomatch.
lookup_match(<<H, T/bits>> = In, {_, Array, Patterns}) ->
    case array2:get(H, Array) of
        undefined -> lookup_match_patterns(In, Patterns);
        Tree when Patterns == [] -> lookup_match(T, Tree);
        Tree ->
            case lookup_match(T, Tree) of
                nomatch -> lookup_match_patterns(In, Patterns);
                Res -> Res
            end
    end;
lookup_match(Input, {leaf, LeafKey, LeafVal, Patterns}) ->
    LeafKeyLen = bit_size(LeafKey),
    case Input of
        LeafKey when LeafVal /= undefined -> {LeafVal, []};
        <<LeafKey:LeafKeyLen/bits, Rest/bits>> -> lookup_match_patterns(Rest, Patterns);
        _ -> nomatch
    end;
lookup_match(<<>>, {NodeVal, _, _}) when NodeVal /= undefined ->
    {NodeVal, []};
lookup_match(_, _) ->
    nomatch.

lookup_match_patterns(_, []) ->
    nomatch;
lookup_match_patterns(Input, [{Pattern, Tree} | RestPatterns]) ->
    case retrie_patterns:match(Input, Pattern) of
        {Match, Rest, Name} ->
            case lookup_match(Rest, Tree) of
                nomatch -> lookup_match_patterns(Input, RestPatterns);
                {Value, Matches} -> {Value, [{Name, retrie_patterns:convert(Match, Pattern)} | Matches]}
            end;
        _ -> lookup_match_patterns(Input, RestPatterns)
    end.


%%% Private functions.

-spec get_patterns(tree()) -> patterns().
get_patterns({_, _, Patterns}) ->
    Patterns;
get_patterns({_, _, _, Patterns}) ->
    Patterns.

-spec put_patterns(tree(), patterns()) -> tree().
put_patterns(Patterns, {NodeVal, Array, _}) ->
    {NodeVal, Array, Patterns};
put_patterns(Patterns, {leaf, LeafKey, LeafVal, _}) ->
    {leaf, LeafKey, LeafVal, Patterns}.
