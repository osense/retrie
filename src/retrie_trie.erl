-module(retrie_trie).

-export([new/0, insert/3, lookup/2]).

-type tree() :: {value(), array2:array2()} | {leaf, key(), value()}.

-type key() :: unicode:unicode_binary().
-type value() :: term().


-spec new() -> tree().
new() ->
    {undefined, array2:new()}.


-spec insert(key(), value(), tree()) -> tree().
insert(<<H, T/bits>>, Value, Tree) ->
    insert1(H, T, Value, Tree).

-spec insert1(integer(), key(), value(), tree()) -> tree().
insert1(H, <<>>, Value, {NodeVal, Array}) ->
    NewTuple = case array2:get(H, Array) of
        {_, Array1} -> {Value, Array1};
        {leaf, Key1, Value1} ->
            NewNode = {Value, array2:new()},
            insert(Key1, Value1, NewNode);
        undefined -> {Value, array2:new()}
    end,
    {NodeVal, array2:set(H, NewTuple, Array)};
insert1(H, T, Value, {NodeVal, Array}) ->
    NewTuple = insert(T, Value, array2:get(H, Array)),
    {NodeVal, array2:set(H, NewTuple, Array)};
insert1(H, T, Value, {leaf, NodeKey, NodeVal}) ->
    NewNode = insert(NodeKey, NodeVal, new()),
    insert1(H, T, Value, NewNode);
insert1(H, T, Value, _) ->
    {leaf, <<H, T/bits>>, Value}.


-spec lookup(key(), tree()) -> value().
lookup(<<H, T/bits>>, Tree) ->
    lookup1(H, T, Tree);
lookup(<<>>, {Value, _}) ->
    Value.

-spec lookup1(integer(), key(), tree()) -> value().
lookup1(H, T, {_, Array}) ->
    case array2:get(H, Array) of
        undefined -> undefined;
        Tree -> lookup(T, Tree)
    end;
lookup1(H, T, {leaf, <<H, T/bits>>, Value}) ->
    Value;
lookup1(_, _, _) ->
    undefined.
