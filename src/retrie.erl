-module(retrie).

-export([new/0, insert_pattern/3, insert_compiled/3, lookup_match/2]).
-export_type([tree/0]).

-type tree() :: tree_node() | tree_chain().
-type tree_node() :: {value(), array2:array2(), patterns()}.
-type tree_chain() :: {key(), tree_node()}.
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
insert_compiled([], Val, {_, Array, Patterns}) ->
    {Val, Array, Patterns};
insert_compiled([Bin | Rest], Val, {Chain, NextNode}) when Bin == Chain ->
    {Chain, insert_compiled(Rest, Val, NextNode)};
insert_compiled([Bin | Rest], Val, {Chain, NextNode}) when is_binary(Bin) ->
    case binary:longest_common_prefix([Bin, Chain]) of
        P when (P == 0) or (P == 1) ->
            <<ChainH, ChainT/binary>> = Chain,
            NewNode = {undefined, array2:set(ChainH, {ChainT, NextNode}, array2:new()), []},
            insert_compiled([Bin | Rest], Val, NewNode);
        P when byte_size(Chain) == P ->
            {Chain, insert_compiled([binary:part(Bin, P, byte_size(Bin) - P) | Rest], Val, NextNode)};
        P ->
            <<Common:P/binary, RestChain/binary>> = Chain,
            NewNode = case RestChain of
                          <<RestH>> -> {undefined, array2:set(RestH, NextNode, array2:new()), []};
                          <<RestH, RestT/binary>> -> {undefined, array2:set(RestH, {RestT, NextNode}, array2:new()), []}
                      end,
            {Common, insert_compiled([binary:part(Bin, P, byte_size(Bin) - P) | Rest], Val, NewNode)}
    end;
insert_compiled([<<>> | Rest], Val, Node) ->
    insert_compiled(Rest, Val, Node);
insert_compiled([<<H, T/bits>> | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewTree = case array2:get(H, Array) of
                  undefined when T == <<>> -> insert_compiled(Rest, Val, new());
                  undefined -> {T, insert_compiled(Rest, Val, new())};
                  Tree -> insert_compiled([T | Rest], Val, Tree)
              end,
    {NodeVal, array2:set(H, NewTree, Array), Patterns};
insert_compiled([Pattern | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewPatterns = case lists:keytake(Pattern, 1, Patterns) of
                      false -> [{Pattern, insert_compiled(Rest, Val, new())} | Patterns];
                      {value, {_Pattern, Tree1}, Ps} -> [{Pattern, insert_compiled(Rest, Val, Tree1)} | Ps]
                  end,
    SortedPatterns = lists:sort(fun({P1, _}, {P2, _}) -> retrie_patterns:compare(P1, P2) end, NewPatterns),
    {NodeVal, Array, SortedPatterns}.


-spec lookup_match(key(), tree()) -> {value(), [{binary(), term()}]} | nomatch.
lookup_match(Input, Tree) ->
    lookup_match1(Input, 0, Tree).

lookup_match1(Binary, N, {_, Array, Patterns}) when N < byte_size(Binary) ->
    case array2:get(binary:at(Binary, N), Array) of
        undefined -> lookup_match_patterns(Binary, N, Patterns);
        Tree when Patterns == [] -> lookup_match1(Binary, N+1, Tree);
        Tree ->
            case lookup_match1(Binary, N+1, Tree) of
                nomatch -> lookup_match_patterns(Binary, N, Patterns);
                Res -> Res
            end
    end;
lookup_match1(Binary, N, {Chain, NextNode}) ->
    ChainLen = byte_size(Chain),
    case Binary of
        <<_:N/binary, Chain:ChainLen/binary, _/binary>> -> lookup_match1(Binary, N + ChainLen, NextNode);
        _ -> nomatch
    end;
lookup_match1(Binary, N, {NodeVal, _, _}) when byte_size(Binary) == N, NodeVal /= undefined ->
    {NodeVal, []}.

lookup_match_patterns(_, _, []) ->
    nomatch;
lookup_match_patterns(Binary, N, [{Pattern, Tree} | RestPatterns]) ->
    case retrie_patterns:match(Binary, N, Pattern) of
        {Match, NewN, Name} ->
            case lookup_match1(Binary, NewN, Tree) of
                nomatch -> lookup_match_patterns(Binary, N, RestPatterns);
                {Value, Matches} -> {Value, [{Name, retrie_patterns:convert(Match, Pattern)} | Matches]}
            end;
        _ -> lookup_match_patterns(Binary, N, RestPatterns)
    end.

