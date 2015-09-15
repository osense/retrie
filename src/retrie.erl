-module(retrie).

-export([new/0, insert_pattern/3, insert_compiled/3, lookup_match/2]).

-type tree() :: tree_node() | tree_chain().
-type tree_node() :: {value(), array2:array2(), patterns()}.
-type tree_chain() :: {key(), tree_node()}.
-type patterns() :: [{retrie_patterns:pattern(), tree()}].

-type key() :: string().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new(), []}.


-spec insert_pattern(string() | unicode:unicode_binary(), value(), tree()) -> tree().
insert_pattern(Binary, Value, Tree) ->
    insert_compiled(retrie_patterns:compile(Binary), Value, Tree).

-spec insert_compiled(retrie_patterns:patterns(), value(), tree()) -> tree().
insert_compiled([], Val, {_, Array, Patterns}) ->
    {Val, Array, Patterns};
insert_compiled([List | Rest], Val, {Chain, NextNode}) when List == Chain ->
    {Chain, insert_compiled(Rest, Val, NextNode)};
insert_compiled([List | Rest], Val, {Chain, NextNode}) when is_list(List) ->
    case prefix(List, Chain) of
        {P, _, _} when length(P) < 2 ->
            [ChainH | ChainT] = Chain,
            NewNode = {undefined, array2:set(ChainH, {ChainT, NextNode}, array2:new()), []},
            insert_compiled([List | Rest], Val, NewNode);
        {P, A, _} when length(Chain) == length(P) ->
            {Chain, insert_compiled([A | Rest], Val, NextNode)};
        {P, A, B} ->
            NewNode = case B of
                          [RestH] -> {undefined, array2:set(RestH, NextNode, array2:new()), []};
                          [RestH | RestT] -> {undefined, array2:set(RestH, {RestT, NextNode}, array2:new()), []}
                      end,
            {P, insert_compiled([A | Rest], Val, NewNode)}
    end;
insert_compiled(["" | Rest], Val, Node) ->
    insert_compiled(Rest, Val, Node);
insert_compiled([[H | T] | Rest], Val, {NodeVal, Array, Patterns}) ->
    NewTree = case array2:get(H, Array) of
                  undefined when T == "" -> insert_compiled(Rest, Val, new());
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
    lookup_match1(retrie_patterns:unicode_to_units(Input), Tree).

lookup_match1([H | T] = In, {_, Array, Patterns}) ->
    case array2:get(H, Array) of
        undefined -> lookup_match_patterns(In, Patterns);
        Tree when Patterns == [] -> lookup_match1(T, Tree);
        Tree ->
            case lookup_match1(T, Tree) of
                nomatch -> lookup_match_patterns(In, Patterns);
                Res -> Res
            end
    end;
lookup_match1([H | T], {[H | TChain], NextNode}) ->
    lookup_match1(T, {TChain, NextNode});
lookup_match1(Input, {"", NextNode}) ->
    lookup_match1(Input, NextNode);
lookup_match1("", {NodeVal, _, _}) when NodeVal /= undefined ->
    {NodeVal, []};
lookup_match1(_, _) ->
    nomatch.

lookup_match_patterns(_, []) ->
    nomatch;
lookup_match_patterns(Input, [{Pattern, Tree} | RestPatterns]) ->
    case retrie_patterns:match(Input, Pattern) of
        {Match, Rest, Name} ->
            case lookup_match1(Rest, Tree) of
                nomatch -> lookup_match_patterns(Input, RestPatterns);
                {Value, Matches} -> {Value, [{Name, retrie_patterns:convert(Match, Pattern)} | Matches]}
            end;
        _ -> lookup_match_patterns(Input, RestPatterns)
    end.


%%% Private functions.

-spec prefix(string(), string()) -> {Prefix::string(), RestA::string(), RestB::string()}.
prefix(A, B) ->
    prefix1(A, B, []).

prefix1([X | Rest1], [X | Rest2], Acc) ->
    prefix1(Rest1, Rest2, [X | Acc]);
prefix1(A, B, Acc) ->
    {lists:reverse(Acc), A, B}.