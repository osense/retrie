-module(retrie).

-export([new/0, insert_pattern/3, compile/1, lookup_match/2, lists_take/2]).

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
    insert_pattern1(patterns:new(Binary), Value, Tree).

-spec insert_pattern1(patterns:patterns(), value(), tree()) -> tree().
insert_pattern1([], Val, _) ->
    {<<>>, Val};
insert_pattern1(P, Val, {<<NH, NT/binary>>, NodeVal}) ->
    NewNode = {undefined, array2:set(NH, {NT, NodeVal}, array2:new()), []},
    insert_pattern1(P, Val, NewNode);
insert_pattern1(P, Val, {<<>>, NodeVal}) ->
    NewNode = {NodeVal, array2:new(), []},
    insert_pattern1(P, Val, NewNode);
insert_pattern1([<<>> | Rest], Val, Tree) ->
    insert_pattern1(Rest, Val, Tree);
insert_pattern1([<<H, T/bits>> | Rest], Val, {NodeVal, Array, Patterns}) ->
    SubTree = ensure_defined(array2:get(H, Array)),
    {NodeVal, array2:set(H, insert_pattern1([T | Rest], Val, SubTree), Array), Patterns};
insert_pattern1([Pattern | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewPatterns = case lists:keytake(Pattern, 1, Patterns) of
        false -> [{Pattern, insert_pattern1(Rest, Val, new())} | Patterns];
        {value, {_Pattern, Tree}, Ps} -> [{Pattern, insert_pattern1(Rest, Val, Tree)} | Ps]
    end,
    {NodeVal, Array, NewPatterns}.


compile({NodeVal, Array, Patterns}) ->
    {NodeVal,
     array2:map(fun compile/1, Array),
     [{patterns:compile(P), compile(T)} || {P, T} <- Patterns]};
compile(Leaf) ->
    Leaf.


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
lookup_match(<<>>, {NodeVal, _, _}) ->
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
                {Value, Matches} -> {Value, [{Name, patterns:convert(Match, Pattern)} | Matches]};
                Matches -> [{Name, patterns:convert(Match, Pattern)} | Matches]
            end;
        _ -> lookup_match_patterns(Input, RestPatterns)
    end.


%%% Private functions.
ensure_defined(undefined) ->
    new();
ensure_defined(Tree) ->
    Tree.

lists_take(Elem, L) ->
    lists_take(Elem, L, []).
lists_take(_, [], _) ->
    false;
lists_take(Elem, [Elem | Tail], Acc) ->
    {Elem, Tail ++ Acc};
lists_take(Elem, [Head | Tail], Acc) ->
    lists_take(Elem, Tail, [Head | Acc]).
