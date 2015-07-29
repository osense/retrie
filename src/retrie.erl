-module(retrie).

-export([new/0, insert/3, lookup/2]).

-type tree() :: {integer(), integer(), value(), array2:array2()} | {key(), value()}.

-type key() :: string().
-type value() :: term().


-spec new() -> tree().
new() ->
    {"", undefined}.


-spec insert(key(), value(), tree()) -> tree().
insert([H | T], Value, Tree) ->
    insert1(H, T, Value, Tree).

-spec insert1(integer(), key(), value(), tree()) -> tree().
insert1(H, [], Value, {Lo, Hi, NodeVal, Array}) ->
    NewTuple = case array2:get(H, Array) of
        {Lo1, Hi1, _, Array1} -> {Lo1, Hi1, Value, Array1};
        {Key1, Value1} ->
            NewNode = {H, H, Value, array2:new()},
            insert(Key1, Value1, NewNode);
        undefined -> {H, H, Value, array2:new()}
    end,
    {Lo, Hi, NodeVal, array2:set(H, NewTuple, Array)};
insert1(H, T, Value, {Lo, Hi, NodeVal, Array}) ->
    NewTuple = insert(T, Value, array2:get(H, Array)),
    {min(H, Lo), max(H, Hi), NodeVal, array2:set(H, NewTuple, Array)};
insert1(H, T, Value, {[NH], NodeVal}) ->
    NewNode = {NH, NH, undefined, array2:set(NH, {NH, NH, NodeVal, array2:new()}, array2:new())},
    insert1(H, T, Value, NewNode);
insert1(H, T, Value, {[NH | NT], NodeVal}) ->
    NewNode = {NH, NH, undefined, array2:set(NH, {NT, NodeVal}, array2:new())},
    insert1(H, T, Value, NewNode);
insert1(H, T, Value, _) ->
    {[H | T], Value}.


-spec lookup(key(), tree()) -> value().
lookup([H | T], Tree) ->
    lookup1(H, T, Tree);
lookup([], {_, _, Value, _}) ->
    Value.

-spec lookup1(integer(), key(), tree()) -> value().
lookup1(H, T, {_, _, _, Array}) ->
    case array2:get(H, Array) of
        undefined -> undefined;
        Tree -> lookup(T, Tree)
    end;
lookup1(H, T, {[H | T], Value}) ->
    Value;
lookup1(_, _, _) ->
    undefined.

